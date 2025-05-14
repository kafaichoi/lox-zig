const std = @import("std");
const Expr = @import("./expr.zig").Expr;
const Stmt = @import("./stmt.zig").Stmt;
const Allocator = std.mem.Allocator;

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
