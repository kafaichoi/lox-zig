const std = @import("std");
const Expr = @import("./expr.zig").Expr;
const Value = @import("./expr.zig").Value;
const Stmt = @import("./ast.zig").Stmt;
const Declaration = @import("./ast.zig").Declaration;
const VarDecl = @import("./ast.zig").VarDecl;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const ValueType = @import("./expr.zig").ValueType;
const RuntimeError = @import("./error.zig").RuntimeError;
const Environment = @import("./environment.zig").Environment;

// Runtime error type
pub const Interpreter = struct {
    runtime_error: ?RuntimeError,
    had_error: bool,
    allocator: std.mem.Allocator,
    writer: ?*std.ArrayList(u8),
    environment: *Environment,

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        const env = allocator.create(Environment) catch unreachable;
        env.* = Environment.init(allocator, null);
        return Interpreter{
            .runtime_error = null,
            .had_error = false,
            .allocator = allocator,
            .writer = null,
            .environment = env,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        if (self.runtime_error) |*err| {
            self.allocator.free(err.message);
        }
        self.environment.deinit();
        self.allocator.destroy(self.environment);
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
        if (decl.initializer) |init_expr| {
            const value = try self.evaluate(init_expr);
            try self.environment.define(decl.name, value);
        } else {
            // Do not initialize the variable
            try self.environment.define_uninitialized(decl.name);
        }
    }

    fn execute(self: *Interpreter, stmt: *Stmt) anyerror!void {
        switch (stmt.*) {
            .print => |p| {
                var value = try self.evaluate(p.expression);
                defer value.deinit();
                const str = self.stringify(value);
                defer if (@as(std.meta.Tag(ValueType), value.data) == .double) self.allocator.free(str);
                if (self.writer) |writer| {
                    try writer.appendSlice(str);
                    try writer.appendSlice("\n");
                } else {
                    const stdout = std.io.getStdOut().writer();
                    try stdout.print("{s}\n", .{str});
                }
            },
            .expression => |e| {
                var value = try self.evaluate(e.expression);
                defer value.deinit();
            },
            .block => |b| {
                // Create a new environment for the block
                var block_env = self.allocator.create(Environment) catch unreachable;
                block_env.* = Environment.init(self.allocator, self.environment);
                const previous = self.environment;
                self.environment = block_env;
                defer {
                    self.environment = previous;
                    block_env.deinit();
                    self.allocator.destroy(block_env);
                }
                for (b.statements) |decl| try self.execute_declaration(decl);
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
                if (self.environment.get(name)) |v| {
                    return v;
                } else {
                    const message = try std.fmt.allocPrint(self.allocator, "Uninitialized variable '{s}'.", .{name});
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = var_expr.name,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
            },
            .assign => |a| {
                const value = try self.evaluate(a.value);
                if (self.environment.values.contains(a.name.lexeme)) {
                    if (self.environment.values.getPtr(a.name.lexeme)) |old_value| {
                        old_value.deinit();
                    }
                    try self.environment.values.put(a.name.lexeme, value);
                    try self.environment.initialized.put(a.name.lexeme, true);
                    return value;
                } else if (self.environment.enclosing) |parent| {
                    // Try to assign in the enclosing scope
                    if (parent.values.contains(a.name.lexeme)) {
                        if (parent.values.getPtr(a.name.lexeme)) |old_value| {
                            old_value.deinit();
                        }
                        try parent.values.put(a.name.lexeme, value);
                        try parent.initialized.put(a.name.lexeme, true);
                        return value;
                    }
                }
                const message = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'.", .{a.name.lexeme});
                self.runtime_error = RuntimeError{
                    .message = message,
                    .token = a.name,
                };
                self.had_error = true;
                return error.RuntimeError;
            },
        };
    }

    fn evaluate_binary(self: *Interpreter, expr: *Expr.BinaryExpr) anyerror!Value {
        var left = try self.evaluate(expr.left);
        defer left.deinit();
        var right = try self.evaluate(expr.right);
        defer right.deinit();

        switch (expr.operator.type) {
            .MINUS => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .double = left.getNumber() - right.getNumber() }, null);
            },
            .SLASH => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                if (right.getNumber() == 0) {
                    const message = try self.allocator.dupe(u8, "Division by zero.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .double = left.getNumber() / right.getNumber() }, null);
            },
            .STAR => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .double = left.getNumber() * right.getNumber() }, null);
            },
            .PLUS => {
                if (left.isNumber() and right.isNumber()) {
                    return Value.init(.{ .double = left.getNumber() + right.getNumber() }, null);
                }
                if (left.isString() and right.isString()) {
                    var result = std.ArrayList(u8).init(self.allocator);
                    try result.appendSlice(left.getString());
                    try result.appendSlice(right.getString());
                    const concatenated = try result.toOwnedSlice();
                    return Value{
                        .data = .{ .string = concatenated },
                        .allocator = self.allocator,
                        .owned = true,
                    };
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
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .boolean = left.getNumber() > right.getNumber() }, null);
            },
            .GREATER_EQUAL => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .boolean = left.getNumber() >= right.getNumber() }, null);
            },
            .LESS => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .boolean = left.getNumber() < right.getNumber() }, null);
            },
            .LESS_EQUAL => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .boolean = left.getNumber() <= right.getNumber() }, null);
            },
            .BANG_EQUAL => return Value.init(.{ .boolean = !is_equal(left, right) }, null),
            .EQUAL_EQUAL => return Value.init(.{ .boolean = is_equal(left, right) }, null),
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
        var right = try self.evaluate(expr.right);
        defer right.deinit();

        switch (expr.operator.type) {
            .MINUS => {
                if (!right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operand must be a number.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .double = -right.getNumber() }, null);
            },
            .BANG => return Value.init(.{ .boolean = !is_truthy(right) }, null),
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

    fn is_truthy(value: Value) bool {
        return switch (value.data) {
            .nil => false,
            .boolean => |b| b,
            else => true,
        };
    }

    fn is_equal(a: Value, b: Value) bool {
        if (@as(std.meta.Tag(ValueType), a.data) != @as(std.meta.Tag(ValueType), b.data)) return false;
        return switch (a.data) {
            .nil => true,
            .boolean => |a_bool| a_bool == b.getBoolean(),
            .double => |a_num| a_num == b.getNumber(),
            .string => |a_str| std.mem.eql(u8, a_str, b.getString()),
            .none => false,
        };
    }

    fn stringify(self: *Interpreter, value: Value) []const u8 {
        return switch (value.data) {
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

    fn free_value(self: *Interpreter, value: Value) void {
        switch (value.data) {
            .string => |s| self.allocator.free(s),
            else => {},
        }
    }
};

test "stringify" {
    var interpreter = Interpreter.init(std.testing.allocator);
    defer interpreter.deinit();
    // Test nil
    try std.testing.expectEqualStrings("nil", interpreter.stringify(Value.init(.{ .nil = {} }, null)));

    // Test booleans
    try std.testing.expectEqualStrings("true", interpreter.stringify(Value.init(.{ .boolean = true }, null)));
    try std.testing.expectEqualStrings("false", interpreter.stringify(Value.init(.{ .boolean = false }, null)));

    // Test numbers - note that integers are printed without decimal point
    const str_42 = interpreter.stringify(Value.init(.{ .double = 42 }, null));
    try std.testing.expectEqualStrings("42", str_42);
    std.testing.allocator.free(str_42);
    const str_314 = interpreter.stringify(Value.init(.{ .double = 3.14 }, null));
    try std.testing.expectEqualStrings("3.14", str_314);
    std.testing.allocator.free(str_314);
    const str_0 = interpreter.stringify(Value.init(.{ .double = 0 }, null));
    try std.testing.expectEqualStrings("0", str_0);
    std.testing.allocator.free(str_0);
    const str_neg1 = interpreter.stringify(Value.init(.{ .double = -1 }, null));
    try std.testing.expectEqualStrings("-1", str_neg1);
    std.testing.allocator.free(str_neg1);

    // Test strings
    var str_hello = Value.init(.{ .string = "hello" }, std.testing.allocator);
    defer str_hello.deinit();
    try std.testing.expectEqualStrings("hello", interpreter.stringify(str_hello));

    // Test none
    try std.testing.expectEqualStrings("none", interpreter.stringify(Value.init(.{ .none = {} }, null)));
}

test "variable declaration and access" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a variable declaration
    const name = Token{ .type = .IDENTIFIER, .lexeme = "x", .literal = .{ .none = {} }, .line = 1 };
    const value = try Expr.LiteralExpr.create(allocator, Value.init(.{ .double = 42.0 }, null));
    const var_decl = try VarDecl.create(allocator, name.lexeme, value);
    defer var_decl.deinit(allocator);

    // Execute the declaration
    try interpreter.execute_var_decl(var_decl.var_decl);

    // Create a variable expression to access it
    const var_expr = try Expr.VariableExpr.create(allocator, name);
    defer var_expr.deinit(allocator);

    // Evaluate the variable
    const result = try interpreter.evaluate(var_expr);
    try std.testing.expectEqual(Value.init(.{ .double = 42.0 }, null), result);
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
        try std.testing.expectEqualStrings("Uninitialized variable 'undefined'.", err.message);
    }
}

test "string concatenation" {
    var interpreter = Interpreter.init(std.testing.allocator);
    defer interpreter.deinit();

    const plus_op = Token{ .type = .PLUS, .lexeme = "+", .literal = .{ .none = {} }, .line = 1 };
    const left_str = Value.init_borrowed(.{ .string = "hello " }, std.testing.allocator);
    const right_str = Value.init_borrowed(.{ .string = "world" }, std.testing.allocator);
    const concat_expr = try Expr.BinaryExpr.create(
        std.testing.allocator,
        try Expr.LiteralExpr.create(std.testing.allocator, left_str),
        plus_op,
        try Expr.LiteralExpr.create(std.testing.allocator, right_str),
    );
    defer concat_expr.deinit(std.testing.allocator);

    var result = try interpreter.evaluate(concat_expr);
    defer result.deinit();

    try std.testing.expectEqualStrings("hello world", result.getString());
}

test "type error handling" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a number and a string
    const number = try Expr.LiteralExpr.create(allocator, Value.init(.{ .double = 42.0 }, null));
    const string = try Expr.LiteralExpr.create(allocator, Value.init_borrowed(.{ .string = "hello" }, std.testing.allocator));

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
    const true_lit = try Expr.LiteralExpr.create(allocator, Value.init(.{ .boolean = true }, null));
    const false_lit = try Expr.LiteralExpr.create(allocator, Value.init(.{ .boolean = false }, null));

    // Test equality
    const eq_op = Token{ .type = .EQUAL_EQUAL, .lexeme = "==", .literal = .{ .none = {} }, .line = 1 };
    const eq_expr = try Expr.BinaryExpr.create(allocator, true_lit, eq_op, false_lit);
    defer eq_expr.deinit(allocator);

    const eq_result = try interpreter.evaluate(eq_expr);
    try std.testing.expectEqual(Value.init(.{ .boolean = false }, null), eq_result);

    // Test not equal
    const neq_op = Token{ .type = .BANG_EQUAL, .lexeme = "!=", .literal = .{ .none = {} }, .line = 1 };
    const neq_expr = try Expr.BinaryExpr.create(allocator, try Expr.LiteralExpr.create(allocator, Value.init(.{ .boolean = true }, null)), neq_op, try Expr.LiteralExpr.create(allocator, Value.init(.{ .boolean = false }, null)));
    defer neq_expr.deinit(allocator);

    const neq_result = try interpreter.evaluate(neq_expr);
    try std.testing.expectEqual(Value.init(.{ .boolean = true }, null), neq_result);
}

test "nil handling" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create nil literals
    const nil1 = try Expr.LiteralExpr.create(allocator, Value.init(.{ .nil = {} }, null));
    const nil2 = try Expr.LiteralExpr.create(allocator, Value.init(.{ .nil = {} }, null));
    // Note: We don't need to free nil1 and nil2 as they are owned by the binary expressions

    // Test nil equality
    const eq_op = Token{ .type = .EQUAL_EQUAL, .lexeme = "==", .literal = .{ .none = {} }, .line = 1 };
    const eq_expr = try Expr.BinaryExpr.create(allocator, nil1, eq_op, nil2);
    defer eq_expr.deinit(allocator);

    const eq_result = try interpreter.evaluate(eq_expr);
    try std.testing.expectEqual(Value.init(.{ .boolean = true }, null), eq_result);

    // Test nil with non-nil
    const number = try Expr.LiteralExpr.create(allocator, Value.init(.{ .double = 42.0 }, null));
    const nil1_2 = try Expr.LiteralExpr.create(allocator, Value.init(.{ .nil = {} }, null));
    // Note: We don't need to free number or nil1_2 as they are owned by neq_expr

    const neq_expr = try Expr.BinaryExpr.create(allocator, nil1_2, eq_op, number);
    defer neq_expr.deinit(allocator);

    const neq_result = try interpreter.evaluate(neq_expr);
    try std.testing.expectEqual(Value.init(.{ .boolean = false }, null), neq_result);
}
