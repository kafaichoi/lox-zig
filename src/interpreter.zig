const std = @import("std");
const Expr = @import("./expr.zig").Expr;
const Value = @import("./expr.zig").Value;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;

// Runtime error type
pub const RuntimeError = struct {
    token: Token,
    message: []const u8,
};

pub const Interpreter = struct {
    had_error: bool,
    runtime_error: ?RuntimeError,

    pub fn init() Interpreter {
        return Interpreter{
            .had_error = false,
            .runtime_error = null,
        };
    }

    pub fn interpret(self: *Interpreter, expression: *Expr) !Value {
        self.had_error = false;
        self.runtime_error = null;
        return self.evaluate(expression);
    }

    fn evaluate(self: *Interpreter, expr: *Expr) Value {
        return switch (expr.*) {
            .literal => |l| self.evaluateLiteral(l),
            .grouping => |g| self.evaluateGrouping(g),
            .unary => |u| self.evaluateUnary(u),
            .binary => |b| self.evaluateBinary(b),
        };
    }

    fn evaluateLiteral(self: *Interpreter, expr: *Expr.LiteralExpr) Value {
        _ = self;
        return expr.value;
    }

    fn evaluateGrouping(self: *Interpreter, expr: *Expr.GroupingExpr) Value {
        return self.evaluate(expr.expression);
    }

    fn evaluateUnary(self: *Interpreter, expr: *Expr.UnaryExpr) Value {
        const right = self.evaluate(expr.right);

        switch (expr.operator.type) {
            .MINUS => {
                _ = self.checkNumberOperand(expr.operator, right) catch return Value{ .nil = {} };
                if (right == .double) {
                    return Value{ .double = -right.double };
                }
            },
            .BANG => {
                return Value{ .boolean = !isTruthy(right) };
            },
            else => {},
        }

        // Unreachable
        return Value{ .nil = {} };
    }

    fn evaluateBinary(self: *Interpreter, expr: *Expr.BinaryExpr) Value {
        const left = self.evaluate(expr.left);
        const right = self.evaluate(expr.right);

        switch (expr.operator.type) {
            // Comparison operators
            .GREATER => {
                self.checkNumberOperands(expr.operator, left, right) catch return Value{ .nil = {} };
                return Value{ .boolean = left.double > right.double };
            },
            .GREATER_EQUAL => {
                self.checkNumberOperands(expr.operator, left, right) catch return Value{ .nil = {} };
                return Value{ .boolean = left.double >= right.double };
            },
            .LESS => {
                self.checkNumberOperands(expr.operator, left, right) catch return Value{ .nil = {} };
                return Value{ .boolean = left.double < right.double };
            },
            .LESS_EQUAL => {
                self.checkNumberOperands(expr.operator, left, right) catch return Value{ .nil = {} };
                return Value{ .boolean = left.double <= right.double };
            },

            // Equality operators
            .BANG_EQUAL => return Value{ .boolean = !isEqual(left, right) },
            .EQUAL_EQUAL => return Value{ .boolean = isEqual(left, right) },

            // Arithmetic operators
            .MINUS => {
                self.checkNumberOperands(expr.operator, left, right) catch return Value{ .nil = {} };
                return Value{ .double = left.double - right.double };
            },
            .PLUS => {
                // Handle addition or string concatenation
                if (left == .double and right == .double) {
                    return Value{ .double = left.double + right.double };
                }

                if (left == .string and right == .string) {
                    const len = left.string.len + right.string.len;
                    var result = std.heap.page_allocator.alloc(u8, len) catch {
                        self.runtime_error = RuntimeError{
                            .token = expr.operator,
                            .message = "Out of memory.",
                        };
                        return Value{ .nil = {} };
                    };
                    @memcpy(result[0..left.string.len], left.string);
                    @memcpy(result[left.string.len..], right.string);
                    return Value{ .string = result };
                }

                self.runtime_error = RuntimeError{
                    .token = expr.operator,
                    .message = "Operands must be two numbers or two strings.",
                };
                std.debug.print("Error: {s}\n[line {d}]\n", .{ self.runtime_error.?.message, expr.operator.line });
                return Value{ .nil = {} };
            },
            .SLASH => {
                self.checkNumberOperands(expr.operator, left, right) catch return Value{ .nil = {} };

                // Check for division by zero
                if (right.double == 0.0) {
                    self.runtime_error = RuntimeError{
                        .token = expr.operator,
                        .message = "Division by zero.",
                    };
                    return Value{ .nil = {} };
                }

                return Value{ .double = left.double / right.double };
            },
            .STAR => {
                self.checkNumberOperands(expr.operator, left, right) catch return Value{ .nil = {} };
                return Value{ .double = left.double * right.double };
            },
            else => {},
        }

        // Unreachable
        return Value{ .nil = {} };
    }

    // Return true if the value is "truthy" by Lox rules
    fn isTruthy(value: Value) bool {
        return switch (value) {
            .nil => false,
            .boolean => |b| b,
            else => true,
        };
    }

    // Check if two values are equal by Lox rules
    fn isEqual(a: Value, b: Value) bool {
        if (@as(std.meta.Tag(Value), a) != @as(std.meta.Tag(Value), b)) {
            return false;
        }

        return switch (a) {
            .nil => true, // nil is only equal to nil
            .boolean => |boolean_a| boolean_a == b.boolean,
            .double => |double_a| double_a == b.double,
            .string => |string_a| std.mem.eql(u8, string_a, b.string),
            .none => false,
        };
    }

    // Print value for debugging
    pub fn stringify(value: Value) []const u8 {
        return switch (value) {
            .nil => "nil",
            .boolean => |b| if (b) "true" else "false",
            .double => |d| {
                var buf: [4096]u8 = undefined;
                const result = std.fmt.bufPrintZ(&buf, "{d}", .{d}) catch "error";
                return result;
            },
            .string => |s| s,
            .none => "none",
        };
    }

    // Check if a value is a number
    fn checkNumberOperand(self: *Interpreter, operator: Token, operand: Value) !Value {
        switch (operand) {
            .double => return operand,
            else => {
                self.runtime_error = RuntimeError{
                    .token = operator,
                    .message = "Operand must be a number.",
                };
                return operand;
            },
        }
    }

    // Check if two values are numbers
    fn checkNumberOperands(self: *Interpreter, operator: Token, left: Value, right: Value) !void {
        if (left == .double and right == .double) {
            return;
        }

        self.runtime_error = RuntimeError{
            .token = operator,
            .message = "Operands must be numbers.",
        };
    }
};

test "stringify" {
    // Test nil
    try std.testing.expectEqualStrings("nil", Interpreter.stringify(Value{ .nil = {} }));

    // Test booleans
    try std.testing.expectEqualStrings("true", Interpreter.stringify(Value{ .boolean = true }));
    try std.testing.expectEqualStrings("false", Interpreter.stringify(Value{ .boolean = false }));

    // Test numbers - note that integers are printed without decimal point
    try std.testing.expectEqualStrings("42", Interpreter.stringify(Value{ .double = 42.0 }));
    try std.testing.expectEqualStrings("3.14", Interpreter.stringify(Value{ .double = 3.14 }));
    try std.testing.expectEqualStrings("0", Interpreter.stringify(Value{ .double = 0.0 }));
    try std.testing.expectEqualStrings("-1", Interpreter.stringify(Value{ .double = -1.0 }));

    // Test strings
    try std.testing.expectEqualStrings("hello", Interpreter.stringify(Value{ .string = "hello" }));

    // Test none
    try std.testing.expectEqualStrings("none", Interpreter.stringify(Value{ .none = {} }));
}
