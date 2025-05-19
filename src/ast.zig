const std = @import("std");
const Expr = @import("./expr.zig").Expr;
const Allocator = std.mem.Allocator;
const Token = @import("./scanner.zig").Token;

pub const Stmt = union(enum) {
    print: *PrintStmt,
    expression: *ExpressionStmt,
    block: *BlockStmt,
    if_stmt: *IfStmt,
    while_stmt: *WhileStmt,
    break_stmt: *BreakStmt,
    return_stmt: *ReturnStmt,

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
            .if_stmt => |i| i.deinit(allocator),
            .while_stmt => |w| w.deinit(allocator),
            .break_stmt => |b| {
                allocator.destroy(b);
            },
            .return_stmt => |r| {
                if (r.value) |value| {
                    value.deinit(allocator);
                }
                allocator.destroy(r);
            },
        }
        allocator.destroy(self);
    }

    pub const PrintStmt = struct {
        expression: *Expr,

        pub fn create(allocator: Allocator, expression: *Expr) !*Stmt {
            const stmt = try allocator.create(PrintStmt);
            stmt.* = .{ .expression = expression };
            const result = try allocator.create(Stmt);
            result.* = .{ .print = stmt };
            return result;
        }
    };

    pub const ExpressionStmt = struct {
        expression: *Expr,

        pub fn create(allocator: Allocator, expression: *Expr) !*Stmt {
            const stmt = try allocator.create(ExpressionStmt);
            stmt.* = .{ .expression = expression };
            const result = try allocator.create(Stmt);
            result.* = .{ .expression = stmt };
            return result;
        }
    };

    pub const BlockStmt = struct {
        statements: []*Declaration,

        pub fn create(allocator: Allocator, statements: []*Declaration) !*Stmt {
            const stmt = try allocator.create(BlockStmt);
            stmt.* = .{ .statements = statements };
            const result = try allocator.create(Stmt);
            result.* = .{ .block = stmt };
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

    pub const IfStmt = struct {
        condition: *Expr,
        then_branch: *Stmt,
        else_branch: ?*Stmt,

        pub fn create(allocator: Allocator, condition: *Expr, then_branch: *Stmt, else_branch: ?*Stmt) !*Stmt {
            const stmt = try allocator.create(IfStmt);
            stmt.* = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            };
            const result = try allocator.create(Stmt);
            result.* = .{ .if_stmt = stmt };
            return result;
        }

        pub fn deinit(self: *IfStmt, allocator: Allocator) void {
            self.condition.deinit(allocator);
            self.then_branch.deinit(allocator);
            if (self.else_branch) |else_stmt| {
                else_stmt.deinit(allocator);
            }
            allocator.destroy(self);
        }
    };

    pub const WhileStmt = struct {
        condition: *Expr,
        body: *Stmt,

        pub fn create(allocator: Allocator, condition: *Expr, body: *Stmt) !*Stmt {
            const stmt = try allocator.create(WhileStmt);
            stmt.* = .{
                .condition = condition,
                .body = body,
            };
            const result = try allocator.create(Stmt);
            result.* = .{ .while_stmt = stmt };
            return result;
        }

        pub fn deinit(self: *WhileStmt, allocator: Allocator) void {
            self.condition.deinit(allocator);
            self.body.deinit(allocator);
            allocator.destroy(self);
        }
    };

    pub const BreakStmt = struct {
        pub fn create(allocator: Allocator) !*Stmt {
            const stmt = try allocator.create(BreakStmt);
            stmt.* = .{};
            const result = try allocator.create(Stmt);
            result.* = .{ .break_stmt = stmt };
            return result;
        }
    };

    pub const ReturnStmt = struct {
        keyword: Token,
        value: ?*Expr,

        pub fn create(allocator: Allocator, keyword: Token, value: ?*Expr) !*Stmt {
            const stmt = try allocator.create(ReturnStmt);
            stmt.* = .{
                .keyword = keyword,
                .value = value,
            };
            const result = try allocator.create(Stmt);
            result.* = .{ .return_stmt = stmt };
            return result;
        }
    };
};

pub const Declaration = union(enum) {
    stmt: *Stmt,
    var_decl: *VarDecl,
    func_decl: *FuncDecl,

    pub fn deinit(self: *Declaration, allocator: Allocator) void {
        switch (self.*) {
            .stmt => |s| s.deinit(allocator),
            .var_decl => |v| v.deinit(allocator),
            .func_decl => |f| f.deinit(allocator),
        }
        allocator.destroy(self);
    }
};

pub const VarDecl = struct {
    name: []const u8,
    initializer: ?*Expr,

    pub fn create(allocator: Allocator, name: []const u8, initializer: ?*Expr) !*Declaration {
        const decl = try allocator.create(VarDecl);
        decl.* = .{
            .name = name,
            .initializer = initializer,
        };
        const result = try allocator.create(Declaration);
        result.* = .{ .var_decl = decl };
        return result;
    }

    pub fn deinit(self: *VarDecl, allocator: Allocator) void {
        if (self.initializer) |init| {
            init.deinit(allocator);
        }
        allocator.destroy(self);
    }
};

pub const FuncDecl = struct {
    name: []const u8,
    params: []const Token,
    body: *Stmt,

    pub fn create(allocator: Allocator, name: []const u8, params: []const Token, body: *Stmt) !*Declaration {
        const decl = try allocator.create(FuncDecl);
        decl.* = .{
            .name = name,
            .params = params,
            .body = body,
        };
        const result = try allocator.create(Declaration);
        result.* = .{ .func_decl = decl };
        return result;
    }

    pub fn deinit(self: *FuncDecl, allocator: Allocator) void {
        self.body.deinit(allocator);
        allocator.free(self.params);
        allocator.destroy(self);
    }
};
