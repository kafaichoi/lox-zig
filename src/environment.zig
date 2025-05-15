const std = @import("std");
const Value = @import("./expr.zig").Value;

pub const Environment = struct {
    values: std.StringHashMap(Value),
    enclosing: ?*Environment,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Environment {
        return Environment{
            .values = std.StringHashMap(Value).init(allocator),
            .enclosing = enclosing,
        };
    }

    pub fn deinit(self: *Environment) void {
        var it = self.values.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.values.deinit();
        // Do not deinit enclosing; it's owned elsewhere
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
    }

    pub fn get(self: *Environment, name: []const u8) ?Value {
        if (self.values.get(name)) |v| {
            return v;
        } else if (self.enclosing) |parent| {
            return parent.get(name);
        } else {
            return null;
        }
    }
};
