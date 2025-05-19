const std = @import("std");
const Value = @import("./expr.zig").Value;
const ValueType = @import("./expr.zig").ValueType;

pub const VarState = struct {
    value: Value,
    initialized: bool,
};

pub const Environment = struct {
    variables: std.StringHashMap(VarState),
    enclosing: ?*Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Environment {
        return Environment{
            .variables = std.StringHashMap(VarState).init(allocator),
            .enclosing = enclosing,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.value.deinit();
        }
        self.variables.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.variables.put(name, .{ .value = value, .initialized = true });
    }

    pub fn define_uninitialized(self: *Environment, name: []const u8) !void {
        try self.variables.put(name, .{ .value = Value.init(.{ .nil = {} }, null), .initialized = false });
    }

    pub fn get(self: *Environment, name: []const u8) ?Value {
        if (self.variables.get(name)) |state| {
            if (!state.initialized) return null;
            return state.value;
        } else if (self.enclosing) |parent| {
            return parent.get(name);
        } else {
            return null;
        }
    }

    pub fn assign(self: *Environment, name: []const u8, value: Value) !void {
        if (self.variables.getPtr(name)) |state| {
            state.value.deinit();
            state.* = .{ .value = value, .initialized = true };
        } else if (self.enclosing) |parent| {
            try parent.assign(name, value);
        } else {
            return error.UndefinedVariable;
        }
    }
};
