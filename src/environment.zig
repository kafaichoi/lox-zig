const std = @import("std");
const Value = @import("./expr.zig").Value;

pub const Environment = struct {
    values: std.StringHashMap(Value),
    initialized: std.StringHashMap(bool),
    enclosing: ?*Environment,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Environment {
        return Environment{
            .values = std.StringHashMap(Value).init(allocator),
            .initialized = std.StringHashMap(bool).init(allocator),
            .enclosing = enclosing,
        };
    }

    pub fn deinit(self: *Environment) void {
        var it = self.values.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.values.deinit();
        self.initialized.deinit();
        // Do not deinit enclosing; it's owned elsewhere
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
        try self.initialized.put(name, true);
    }

    pub fn define_uninitialized(self: *Environment, name: []const u8) !void {
        // Do not put a value in values map for uninitialized variables
        try self.initialized.put(name, false);
    }

    pub fn get(self: *Environment, name: []const u8) ?Value {
        if (self.initialized.get(name)) |is_init| {
            if (!is_init) return null;
            // Only return a value if it exists and is initialized
            if (self.values.get(name)) |v| {
                return v;
            } else {
                return null;
            }
        } else if (self.enclosing) |parent| {
            return parent.get(name);
        } else {
            return null;
        }
    }

    pub fn assign(self: *Environment, name: []const u8, value: Value) !void {
        if (self.initialized.get(name)) |exists| {
            if (!exists) {
                try self.initialized.put(name, true);
            }
            if (self.values.getPtr(name)) |old_value| {
                old_value.deinit();
            }
            try self.values.put(name, value);
        } else if (self.enclosing) |parent| {
            try parent.assign(name, value);
        } else {
            return error.UndefinedVariable;
        }
    }
};
