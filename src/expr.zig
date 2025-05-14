const std = @import("std");
const Token = @import("./scanner.zig").Token;
const Allocator = std.mem.Allocator;

// Value types that can be produced by the interpreter
pub const Value = union(enum) {
    nil: void,
    boolean: bool,
    double: f64,
    string: []const u8,
    none: void,
};

// Base expression interface
pub const Expr = union(enum) {
    binary: *BinaryExpr,
    grouping: *GroupingExpr,
    literal: *LiteralExpr,
    unary: *UnaryExpr,
    variable: *VariableExpr,

    // Free memory recursively
    pub fn deinit(self: *Expr, allocator: Allocator) void {
        switch (self.*) {
            .binary => |b| {
                b.left.deinit(allocator);
                b.right.deinit(allocator);
                allocator.destroy(b);
            },
            .grouping => |g| {
                g.expression.deinit(allocator);
                allocator.destroy(g);
            },
            .literal => |l| {
                allocator.destroy(l);
            },
            .unary => |u| {
                u.right.deinit(allocator);
                allocator.destroy(u);
            },
            .variable => |v| {
                allocator.destroy(v);
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
};
