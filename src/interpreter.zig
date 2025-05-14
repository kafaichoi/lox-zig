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
        };
    }

    fn evaluate_binary(self: *Interpreter, expr: *Expr.BinaryExpr) anyerror!Value {
        const left = try self.evaluate(expr.left);
        const right = try self.evaluate(expr.right);

        switch (expr.operator.type) {
            .MINUS => {
                if (left != .double or right != .double) {
                    self.runtime_error = RuntimeError{
                        .message = "Operands must be numbers.",
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .double = left.double - right.double };
            },
            .SLASH => {
                if (left != .double or right != .double) {
                    self.runtime_error = RuntimeError{
                        .message = "Operands must be numbers.",
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                if (right.double == 0) {
                    self.runtime_error = RuntimeError{
                        .message = "Division by zero.",
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .double = left.double / right.double };
            },
            .STAR => {
                if (left != .double or right != .double) {
                    self.runtime_error = RuntimeError{
                        .message = "Operands must be numbers.",
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
                self.runtime_error = RuntimeError{
                    .message = "Operands must be two numbers or two strings.",
                    .token = expr.operator,
                };
                self.had_error = true;
                std.debug.print("Error: {s}\n[line {d}]\n", .{ self.runtime_error.?.message, expr.operator.line });
                return error.RuntimeError;
            },
            .GREATER => {
                if (left != .double or right != .double) {
                    self.runtime_error = RuntimeError{
                        .message = "Operands must be numbers.",
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .boolean = left.double > right.double };
            },
            .GREATER_EQUAL => {
                if (left != .double or right != .double) {
                    self.runtime_error = RuntimeError{
                        .message = "Operands must be numbers.",
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .boolean = left.double >= right.double };
            },
            .LESS => {
                if (left != .double or right != .double) {
                    self.runtime_error = RuntimeError{
                        .message = "Operands must be numbers.",
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .boolean = left.double < right.double };
            },
            .LESS_EQUAL => {
                if (left != .double or right != .double) {
                    self.runtime_error = RuntimeError{
                        .message = "Operands must be numbers.",
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
                self.runtime_error = RuntimeError{
                    .message = "Invalid binary operator.",
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
                    self.runtime_error = RuntimeError{
                        .message = "Operand must be a number.",
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value{ .double = -right.double };
            },
            .BANG => return Value{ .boolean = !is_truthy(right) },
            else => {
                self.runtime_error = RuntimeError{
                    .message = "Invalid unary operator.",
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
