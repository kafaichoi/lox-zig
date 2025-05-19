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

    // Create a deep copy of the environment for closures
    pub fn deepCopy(self: *Environment) *Environment {
        var new_env = self.allocator.create(Environment) catch unreachable;
        new_env.* = Environment.init(self.allocator, self.enclosing);

        // Copy all variables
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            const state = entry.value_ptr.*;

            // Copy the value
            var value_copy = state.value;
            if (@as(std.meta.Tag(ValueType), state.value.data) == .string and
                state.value.allocator != null)
            {
                const s = state.value.data.string;
                const heap_str = state.value.allocator.?.dupe(u8, s) catch unreachable;
                value_copy = Value{ .data = .{ .string = heap_str }, .allocator = state.value.allocator };
            }

            new_env.variables.put(key, .{ .value = value_copy, .initialized = state.initialized }) catch unreachable;
        }

        return new_env;
    }

    // Create a fresh environment with the given enclosing environment
    pub fn createEnvWithEnclosing(allocator: std.mem.Allocator, enclosing: ?*Environment) !*Environment {
        const env = try allocator.create(Environment);
        env.* = Environment.init(allocator, enclosing);
        return env;
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

    // Check if this environment or any enclosing environment contains a variable
    pub fn containsVariable(self: *Environment, name: []const u8) bool {
        if (self.variables.contains(name)) {
            return true;
        } else if (self.enclosing) |parent| {
            return parent.containsVariable(name);
        } else {
            return false;
        }
    }

    // Get variable from a specific ancestor environment
    pub fn getAt(self: *Environment, distance: usize, name: []const u8) Value {
        const environment = self.ancestor(distance);
        if (environment.variables.get(name)) |state| {
            if (!state.initialized) {
                // This would be handled by the resolver, so would not occur here
                return Value.init(.{ .nil = {} }, null);
            }
            return state.value;
        } else {
            // This should not happen if the resolver did its job
            return Value.init(.{ .nil = {} }, null);
        }
    }

    // Assign variable in a specific ancestor environment
    pub fn assignAt(self: *Environment, distance: usize, name: []const u8, value: Value) !void {
        const environment = self.ancestor(distance);
        if (environment.variables.getPtr(name)) |state| {
            state.value.deinit();
            state.* = .{ .value = value, .initialized = true };
        } else {
            // This should not happen if the resolver did its job
            return error.UndefinedVariable;
        }
    }

    // Find an ancestor environment at a given distance
    fn ancestor(self: *Environment, distance: usize) *Environment {
        var environment: *Environment = self;
        var i: usize = 0;
        while (i < distance) : (i += 1) {
            environment = environment.enclosing.?;
        }
        return environment;
    }
};
