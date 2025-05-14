const std = @import("std");
const Expr = @import("./expr.zig").Expr;
const Value = @import("./expr.zig").Value;
const Stmt = @import("./stmt.zig").Stmt;
const Declaration = @import("./decl.zig").Declaration;
const VarDecl = @import("./decl.zig").VarDecl;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;

// Runtime error type
pub const RuntimeError = struct {
    message: []const u8,
    token: Token,
};

pub const Interpreter = struct {
    runtime_error: ?RuntimeError,
    had_error: bool,
    allocator: std.mem.Allocator,
    writer: ?*std.ArrayList(u8),
    environment: std.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return Interpreter{
            .runtime_error = null,
            .had_error = false,
            .allocator = allocator,
            .writer = null,
            .environment = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        if (self.runtime_error) |*err| {
            self.allocator.free(err.message);
        }
        var it = self.environment.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* == .string) {
                self.allocator.free(entry.value_ptr.*.string);
            }
        }
        self.environment.deinit();
    }

    pub fn interpret(self: *Interpreter, declarations: []*Declaration) !void {
        for (declarations) |decl| {
            try self.execute_declaration(decl);
        }
    }

    fn execute_declaration(self: *Interpreter, decl: *Declaration) !void {
        switch (decl.*) {
            .stmt => |stmt| try self.execute(stmt),
            .var_decl => |var_decl| try self.execute_var_decl(var_decl),
        }
    }

    fn execute_var_decl(self: *Interpreter, decl: *VarDecl) !void {
        var value: Value = Value{ .nil = {} };
        if (decl.initializer) |init_expr| {
            value = try self.evaluate(init_expr);
        }

        try self.environment.put(decl.name, value);
    }

    fn execute(self: *Interpreter, stmt: *Stmt) !void {
        switch (stmt.*) {
            .print => |p| {
                const value = try self.evaluate(p.expression);
                const str = self.stringify(value);
                defer if (@as(std.meta.Tag(Value), value) == .double) self.allocator.free(str);
                if (self.writer) |writer| {
                    try writer.appendSlice(str);
                    try writer.appendSlice("\n");
                } else {
                    const stdout = std.io.getStdOut().writer();
                    try stdout.print("{s}\n", .{str});
                }
            },
            .expression => |e| {
                _ = try self.evaluate(e.expression);
            },
        }
    }

    fn evaluate(self: *Interpreter, expr: *Expr) !Value {
        return switch (expr.*) {
            .binary => |b| try self.evaluate_binary(b),
            .unary => |u| try self.evaluate_unary(u),
            .grouping => |g| try self.evaluate(g.expression),
            .literal => |l| l.value,
            .variable => |var_expr| {
                const name = var_expr.name.lexeme;
                const value = self.environment.get(name);
                if (value) |v| {
                    return v;
                } else {
                    const message = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'.", .{name});
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = var_expr.name,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
            },
        };
    }

    fn evaluate_binary(self: *Interpreter, expr: *Expr.BinaryExpr) anyerror!Value {
        const left = try self.evaluate(expr.left);
        const right = try self.evaluate(expr.right);

        switch (expr.operator.type) {
            .MINUS => {
                if (left != .double or right != .double) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .double = left.double - right.double };
            },
            .SLASH => {
                if (left != .double or right != .double) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                if (right.double == 0) {
                    const message = try self.allocator.dupe(u8, "Division by zero.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .double = left.double / right.double };
            },
            .STAR => {
                if (left != .double or right != .double) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .double = left.double * right.double };
            },
            .PLUS => {
                if (left == .double and right == .double) {
                    return Value{ .double = left.double + right.double };
                }
                if (left == .string and right == .string) {
                    var result = std.ArrayList(u8).init(self.allocator);
                    defer result.deinit();
                    try result.appendSlice(left.string);
                    try result.appendSlice(right.string);
                    const final_str = try self.allocator.dupe(u8, result.items);
                    return Value{ .string = final_str };
                }
                const message = try self.allocator.dupe(u8, "Operands must be two numbers or two strings.");
                self.runtime_error = RuntimeError{
                    .message = message,
                    .token = expr.operator,
                };
                self.had_error = true;
                std.debug.print("Error: {s}\n[line {d}]\n", .{ self.runtime_error.?.message, expr.operator.line });
                return error.RuntimeError;
            },
            .GREATER => {
                if (left != .double or right != .double) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .boolean = left.double > right.double };
            },
            .GREATER_EQUAL => {
                if (left != .double or right != .double) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .boolean = left.double >= right.double };
            },
            .LESS => {
                if (left != .double or right != .double) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .boolean = left.double < right.double };
            },
            .LESS_EQUAL => {
                if (left != .double or right != .double) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .boolean = left.double <= right.double };
            },
            .BANG_EQUAL => return Value{ .boolean = !is_equal(left, right) },
            .EQUAL_EQUAL => return Value{ .boolean = is_equal(left, right) },
            else => {
                const message = try self.allocator.dupe(u8, "Invalid binary operator.");
                self.runtime_error = RuntimeError{
                    .message = message,
                    .token = expr.operator,
                };
                self.had_error = true;
                return error.RuntimeError;
            },
        }
    }

    fn evaluate_unary(self: *Interpreter, expr: *Expr.UnaryExpr) anyerror!Value {
        const right = try self.evaluate(expr.right);

        switch (expr.operator.type) {
            .MINUS => {
                if (right != .double) {
                    const message = try self.allocator.dupe(u8, "Operand must be a number.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .double = -right.double };
            },
            .BANG => return Value{ .boolean = !is_truthy(right) },
            else => {
                const message = try self.allocator.dupe(u8, "Invalid unary operator.");
                self.runtime_error = RuntimeError{
                    .message = message,
                    .token = expr.operator,
                };
                self.had_error = true;
                return error.RuntimeError;
            },
        }
    }

    fn is_truthy(_value: Value) bool {
        return switch (_value) {
            .nil => false,
            .boolean => |b| b,
            else => true,
        };
    }

    fn is_equal(_a: Value, _b: Value) bool {
        if (@as(std.meta.Tag(Value), _a) != @as(std.meta.Tag(Value), _b)) return false;
        return switch (_a) {
            .nil => true,
            .boolean => |a_bool| a_bool == _b.boolean,
            .double => |a_num| a_num == _b.double,
            .string => |a_str| std.mem.eql(u8, a_str, _b.string),
            .none => false,
        };
    }

    fn stringify(self: *Interpreter, _value: Value) []const u8 {
        return switch (_value) {
            .nil => "nil",
            .boolean => |b| if (b) "true" else "false",
            .double => |n| blk: {
                const str = std.fmt.allocPrint(self.allocator, "{d}", .{n}) catch "number";
                break :blk str;
            },
            .string => |s| s,
            .none => "none",
        };
    }
};

test "stringify" {
    var interpreter = Interpreter.init(std.testing.allocator);
    // Test nil
    try std.testing.expectEqualStrings("nil", interpreter.stringify(Value{ .nil = {} }));

    // Test booleans
    try std.testing.expectEqualStrings("true", interpreter.stringify(Value{ .boolean = true }));
    try std.testing.expectEqualStrings("false", interpreter.stringify(Value{ .boolean = false }));

    // Test numbers - note that integers are printed without decimal point
    const str_42 = interpreter.stringify(Value{ .double = 42 });
    try std.testing.expectEqualStrings("42", str_42);
    std.testing.allocator.free(str_42);
    const str_314 = interpreter.stringify(Value{ .double = 3.14 });
    try std.testing.expectEqualStrings("3.14", str_314);
    std.testing.allocator.free(str_314);
    const str_0 = interpreter.stringify(Value{ .double = 0 });
    try std.testing.expectEqualStrings("0", str_0);
    std.testing.allocator.free(str_0);
    const str_neg1 = interpreter.stringify(Value{ .double = -1 });
    try std.testing.expectEqualStrings("-1", str_neg1);
    std.testing.allocator.free(str_neg1);

    // Test strings
    try std.testing.expectEqualStrings("hello", interpreter.stringify(Value{ .string = "hello" }));

    // Test none
    try std.testing.expectEqualStrings("none", interpreter.stringify(Value{ .none = {} }));
}

test "variable declaration and access" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a variable declaration
    const name = Token{ .type = .IDENTIFIER, .lexeme = "x", .literal = .{ .none = {} }, .line = 1 };
    const value = try Expr.LiteralExpr.create(allocator, .{ .double = 42.0 });
    const var_decl = try VarDecl.create(allocator, name.lexeme, value);
    defer var_decl.deinit(allocator);

    // Execute the declaration
    try interpreter.execute_var_decl(var_decl.var_decl);

    // Create a variable expression to access it
    const var_expr = try Expr.VariableExpr.create(allocator, name);
    defer var_expr.deinit(allocator);

    // Evaluate the variable
    const result = try interpreter.evaluate(var_expr);
    try std.testing.expectEqual(Value{ .double = 42.0 }, result);
}

test "undefined variable error" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a variable expression for an undefined variable
    const name = Token{ .type = .IDENTIFIER, .lexeme = "undefined", .literal = .{ .none = {} }, .line = 1 };
    const var_expr = try Expr.VariableExpr.create(allocator, name);
    defer var_expr.deinit(allocator);

    // Try to evaluate the undefined variable
    const result = interpreter.evaluate(var_expr);
    try std.testing.expectError(error.RuntimeError, result);
    try std.testing.expect(interpreter.had_error);
    try std.testing.expect(interpreter.runtime_error != null);
    if (interpreter.runtime_error) |err| {
        try std.testing.expectEqualStrings("Undefined variable 'undefined'.", err.message);
    }
}

test "string concatenation" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create two string literals
    const left = try Expr.LiteralExpr.create(allocator, .{ .string = "hello" });
    const right = try Expr.LiteralExpr.create(allocator, .{ .string = " world" });

    // Create a binary expression for concatenation
    const operator = Token{ .type = .PLUS, .lexeme = "+", .literal = .{ .none = {} }, .line = 1 };
    const binary = try Expr.BinaryExpr.create(allocator, left, operator, right);
    defer binary.deinit(allocator);

    // Evaluate the concatenation
    const result = try interpreter.evaluate(binary);
    defer if (result == .string) allocator.free(result.string);
    std.debug.print("result.string = '{s}'\n", .{result.string});
    try std.testing.expectEqualStrings("hello world", result.string);
}

test "type error handling" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a number and a string
    const number = try Expr.LiteralExpr.create(allocator, .{ .double = 42.0 });
    const string = try Expr.LiteralExpr.create(allocator, .{ .string = "hello" });

    // Try to add them
    const operator = Token{ .type = .PLUS, .lexeme = "+", .literal = .{ .none = {} }, .line = 1 };
    const binary = try Expr.BinaryExpr.create(allocator, number, operator, string);
    defer binary.deinit(allocator);

    // Evaluate should fail with a type error
    const result = interpreter.evaluate(binary);
    try std.testing.expectError(error.RuntimeError, result);
    try std.testing.expect(interpreter.had_error);
    try std.testing.expect(interpreter.runtime_error != null);
    if (interpreter.runtime_error) |err| {
        try std.testing.expectEqualStrings("Operands must be two numbers or two strings.", err.message);
    }
}

test "boolean operations" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Test true and false literals
    const true_lit = try Expr.LiteralExpr.create(allocator, .{ .boolean = true });
    const false_lit = try Expr.LiteralExpr.create(allocator, .{ .boolean = false });

    // Test equality
    const eq_op = Token{ .type = .EQUAL_EQUAL, .lexeme = "==", .literal = .{ .none = {} }, .line = 1 };
    const eq_expr = try Expr.BinaryExpr.create(allocator, true_lit, eq_op, false_lit);
    defer eq_expr.deinit(allocator);

    const eq_result = try interpreter.evaluate(eq_expr);
    try std.testing.expectEqual(Value{ .boolean = false }, eq_result);

    // Test not equal
    const neq_op = Token{ .type = .BANG_EQUAL, .lexeme = "!=", .literal = .{ .none = {} }, .line = 1 };
    const neq_expr = try Expr.BinaryExpr.create(allocator, try Expr.LiteralExpr.create(allocator, .{ .boolean = true }), neq_op, try Expr.LiteralExpr.create(allocator, .{ .boolean = false }));
    defer neq_expr.deinit(allocator);

    const neq_result = try interpreter.evaluate(neq_expr);
    try std.testing.expectEqual(Value{ .boolean = true }, neq_result);
}

test "nil handling" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create nil literals
    const nil1 = try Expr.LiteralExpr.create(allocator, .{ .nil = {} });
    const nil2 = try Expr.LiteralExpr.create(allocator, .{ .nil = {} });
    // Note: We don't need to free nil1 and nil2 as they are owned by the binary expressions

    // Test nil equality
    const eq_op = Token{ .type = .EQUAL_EQUAL, .lexeme = "==", .literal = .{ .none = {} }, .line = 1 };
    const eq_expr = try Expr.BinaryExpr.create(allocator, nil1, eq_op, nil2);
    defer eq_expr.deinit(allocator); // This will free both nil1 and nil2

    const eq_result = try interpreter.evaluate(eq_expr);
    try std.testing.expectEqual(Value{ .boolean = true }, eq_result);

    // Test nil with non-nil
    const number = try Expr.LiteralExpr.create(allocator, .{ .double = 42.0 });
    const nil1_2 = try Expr.LiteralExpr.create(allocator, .{ .nil = {} });
    // Note: We don't need to free number or nil1_2 as they are owned by neq_expr

    const neq_expr = try Expr.BinaryExpr.create(allocator, nil1_2, eq_op, number);
    defer neq_expr.deinit(allocator); // This will free both nil1_2 and number

    const neq_result = try interpreter.evaluate(neq_expr);
    try std.testing.expectEqual(Value{ .boolean = false }, neq_result);
}
