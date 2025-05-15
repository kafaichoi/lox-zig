const std = @import("std");
const Expr = @import("./expr.zig").Expr;
const Allocator = std.mem.Allocator;

pub const Stmt = union(enum) {
    print: *PrintStmt,
    expression: *ExpressionStmt,
    block: *BlockStmt,

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
            .block => |b| {
                b.deinit(allocator);
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

    pub const BlockStmt = struct {
        statements: []*Declaration,

        pub fn create(allocator: Allocator, statements: []*Declaration) !*Stmt {
            const stmt = try allocator.create(BlockStmt);
            stmt.* = BlockStmt{ .statements = statements };
            const result = try allocator.create(Stmt);
            result.* = Stmt{ .block = stmt };
            return result;
        }

        pub fn deinit(self: *BlockStmt, allocator: Allocator) void {
            for (self.statements) |decl| {
                decl.deinit(allocator);
            }
            allocator.free(self.statements);
            allocator.destroy(self);
        }
    };
};

pub const Declaration = union(enum) {
    stmt: *Stmt,
    var_decl: *VarDecl,

    pub fn deinit(self: *Declaration, allocator: Allocator) void {
        switch (self.*) {
            .stmt => |s| {
                s.deinit(allocator);
                allocator.destroy(s);
            },
            .var_decl => |v| {
                v.deinit(allocator);
                allocator.destroy(v);
            },
        }
        allocator.destroy(self);
    }
};

pub const VarDecl = struct {
    name: []const u8,
    initializer: ?*Expr,

    pub fn create(allocator: Allocator, name: []const u8, initializer: ?*Expr) !*Declaration {
        const decl = try allocator.create(VarDecl);
        decl.* = VarDecl{
            .name = name,
            .initializer = initializer,
        };

        const result = try allocator.create(Declaration);
        result.* = Declaration{ .var_decl = decl };
        return result;
    }

    pub fn deinit(self: *VarDecl, allocator: Allocator) void {
        if (self.initializer) |init| {
            init.deinit(allocator);
        }
    }
};
