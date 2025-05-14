const std = @import("std");
const Expr = @import("./expr.zig").Expr;
const Allocator = std.mem.Allocator;

pub const Stmt = union(enum) {
    print: *PrintStmt,
    expression: *ExpressionStmt,

    pub fn deinit(self: *Stmt, allocator: Allocator) void {
        switch (self.*) {
            .print => |p| {
                p.expression.deinit(allocator);
                allocator.destroy(p);
            },
            .expression => |e| {
                e.expression.deinit(allocator);
                allocator.destroy(e);
            },
        }
    }

    pub const PrintStmt = struct {
        expression: *Expr,

        pub fn create(allocator: Allocator, expression: *Expr) !*Stmt {
            const stmt = try allocator.create(PrintStmt);
            stmt.* = PrintStmt{
                .expression = expression,
            };

            const result = try allocator.create(Stmt);
            result.* = Stmt{ .print = stmt };
            return result;
        }
    };

    pub const ExpressionStmt = struct {
        expression: *Expr,

        pub fn create(allocator: Allocator, expression: *Expr) !*Stmt {
            const stmt = try allocator.create(ExpressionStmt);
            stmt.* = ExpressionStmt{
                .expression = expression,
            };

            const result = try allocator.create(Stmt);
            result.* = Stmt{ .expression = stmt };
            return result;
        }
    };
};
