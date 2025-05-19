const std = @import("std");
const Token = @import("./scanner.zig").Token;
const Allocator = std.mem.Allocator;
const FuncDecl = @import("./ast.zig").FuncDecl;
const Environment = @import("./environment.zig").Environment;
const Callable = @import("./callable.zig").Callable;

// Runtime function representation
pub const LoxFunction = struct {
    declaration: *FuncDecl,
    closure: *Environment,

    pub fn init(declaration: *FuncDecl, closure: *Environment) LoxFunction {
        return LoxFunction{
            .declaration = declaration,
            .closure = closure,
        };
    }
};

// Value types that can be produced by the interpreter
pub const ValueType = union(enum) {
    nil: void,
    boolean: bool,
    double: f64,
    string: []const u8,
    none: void,
    callable: Callable,
};

pub const Value = struct {
    data: ValueType,
    allocator: ?std.mem.Allocator,

    pub fn init(data: ValueType, allocator: ?std.mem.Allocator) Value {
        if (@as(std.meta.Tag(ValueType), data) == .string) {
            std.debug.assert(allocator != null);
            const s = data.string;
            const heap_str = allocator.?.dupe(u8, s) catch unreachable;
            return .{ .data = .{ .string = heap_str }, .allocator = allocator };
        }
        return .{ .data = data, .allocator = null };
    }

    pub fn init_borrowed(data: ValueType) Value {
        return .{ .data = data, .allocator = null };
    }

    pub fn deinit(self: *Value) void {
        if (self.allocator) |allocator| {
            switch (self.data) {
                .string => |s| allocator.free(s),
                else => {},
            }
        }
    }

    pub fn isString(self: Value) bool {
        return self.data == .string;
    }

    pub fn getString(self: Value) []const u8 {
        return switch (self.data) {
            .string => |s| s,
            else => unreachable,
        };
    }

    pub fn isNumber(self: Value) bool {
        return self.data == .double;
    }

    pub fn getNumber(self: Value) f64 {
        return switch (self.data) {
            .double => |n| n,
            else => unreachable,
        };
    }

    pub fn isBoolean(self: Value) bool {
        return self.data == .boolean;
    }

    pub fn getBoolean(self: Value) bool {
        return switch (self.data) {
            .boolean => |b| b,
            else => unreachable,
        };
    }

    pub fn isNil(self: Value) bool {
        return self.data == .nil;
    }

    pub fn isFunction(self: Value) bool {
        return self.data == .callable;
    }

    pub fn getFunction(self: Value) *FuncDecl {
        return switch (self.data) {
            .callable => |f| switch (f) {
                .function => |func| func.declaration,
                else => unreachable,
            },
            else => unreachable,
        };
    }
};

// Base expression interface
pub const Expr = union(enum) {
    binary: *BinaryExpr,
    unary: *UnaryExpr,
    grouping: *GroupingExpr,
    literal: *LiteralExpr,
    variable: *VariableExpr,
    assign: *AssignExpr,
    logical: *LogicalExpr,
    call: *CallExpr,

    // Free memory recursively
    pub fn deinit(self: *Expr, allocator: Allocator) void {
        switch (self.*) {
            .binary => |b| {
                b.left.deinit(allocator);
                b.right.deinit(allocator);
                allocator.destroy(b);
            },
            .unary => |u| {
                u.right.deinit(allocator);
                allocator.destroy(u);
            },
            .grouping => |g| {
                g.expression.deinit(allocator);
                allocator.destroy(g);
            },
            .literal => |l| {
                l.value.deinit();
                allocator.destroy(l);
            },
            .variable => |v| {
                allocator.destroy(v);
            },
            .assign => |a| {
                a.value.deinit(allocator);
                allocator.destroy(a);
            },
            .logical => |l| {
                l.left.deinit(allocator);
                l.right.deinit(allocator);
                allocator.destroy(l);
            },
            .call => |c| {
                c.callee.deinit(allocator);
                for (c.arguments) |arg| arg.deinit(allocator);
                allocator.free(c.arguments);
                allocator.destroy(c);
            },
        }
        allocator.destroy(self);
    }

    // Binary expression
    pub const BinaryExpr = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,

        // Create a new Binary expression
        pub fn create(allocator: Allocator, left: *Expr, operator: Token, right: *Expr) !*Expr {
            const expr = try allocator.create(BinaryExpr);
            expr.* = BinaryExpr{
                .left = left,
                .operator = operator,
                .right = right,
            };

            const result = try allocator.create(Expr);
            result.* = Expr{ .binary = expr };
            return result;
        }
    };

    // Grouping expression
    pub const GroupingExpr = struct {
        expression: *Expr,

        // Create a new Grouping expression
        pub fn create(allocator: Allocator, expression: *Expr) !*Expr {
            const expr = try allocator.create(GroupingExpr);
            expr.* = GroupingExpr{
                .expression = expression,
            };

            const result = try allocator.create(Expr);
            result.* = Expr{ .grouping = expr };
            return result;
        }
    };

    // Literal expression
    pub const LiteralExpr = struct {
        value: Value,

        // Create a new Literal expression
        pub fn create(allocator: Allocator, value: Value) !*Expr {
            const expr = try allocator.create(LiteralExpr);
            expr.* = LiteralExpr{
                .value = value,
            };

            const result = try allocator.create(Expr);
            result.* = Expr{ .literal = expr };
            return result;
        }
    };

    // Unary expression
    pub const UnaryExpr = struct {
        operator: Token,
        right: *Expr,

        // Create a new Unary expression
        pub fn create(allocator: Allocator, operator: Token, right: *Expr) !*Expr {
            const expr = try allocator.create(UnaryExpr);
            expr.* = UnaryExpr{
                .operator = operator,
                .right = right,
            };

            const result = try allocator.create(Expr);
            result.* = Expr{ .unary = expr };
            return result;
        }
    };

    // Variable expression
    pub const VariableExpr = struct {
        name: Token,

        // Create a new Variable expression
        pub fn create(allocator: Allocator, name: Token) !*Expr {
            const expr = try allocator.create(VariableExpr);
            expr.* = VariableExpr{
                .name = name,
            };

            const result = try allocator.create(Expr);
            result.* = Expr{ .variable = expr };
            return result;
        }
    };

    // Assignment expression
    pub const AssignExpr = struct {
        name: Token,
        value: *Expr,

        // Create a new Assignment expression
        pub fn create(allocator: Allocator, name: Token, value: *Expr) !*Expr {
            const expr = try allocator.create(AssignExpr);
            expr.* = AssignExpr{
                .name = name,
                .value = value,
            };

            const result = try allocator.create(Expr);
            result.* = Expr{ .assign = expr };
            return result;
        }
    };

    pub const LogicalExpr = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,

        pub fn create(allocator: Allocator, left: *Expr, operator: Token, right: *Expr) !*Expr {
            const expr = try allocator.create(LogicalExpr);
            expr.* = .{
                .left = left,
                .operator = operator,
                .right = right,
            };
            const result = try allocator.create(Expr);
            result.* = .{ .logical = expr };
            return result;
        }
    };

    // Call expression
    pub const CallExpr = struct {
        callee: *Expr,
        paren: Token, // The closing parenthesis token
        arguments: []*Expr,

        // Create a new Call expression
        pub fn create(allocator: Allocator, callee: *Expr, paren: Token, arguments: []*Expr) !*Expr {
            const expr = try allocator.create(CallExpr);
            expr.* = CallExpr{
                .callee = callee,
                .paren = paren,
                .arguments = arguments,
            };

            const result = try allocator.create(Expr);
            result.* = Expr{ .call = expr };
            return result;
        }
    };
};

fn is_equal(a: Value, b: Value) bool {
    if (@as(std.meta.Tag(ValueType), a.data) != @as(std.meta.Tag(ValueType), b.data)) return false;
    return switch (a.data) {
        .nil => true,
        .boolean => |a_bool| a_bool == b.getBoolean(),
        .double => |a_num| a_num == b.getNumber(),
        .string => |a_str| std.mem.eql(u8, a_str, b.getString()),
        .callable => |a_func| a_func.declaration == b.getFunction(),
        .none => false,
    };
}
