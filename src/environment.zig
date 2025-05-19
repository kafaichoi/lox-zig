const std = @import("std");
const Value = @import("./expr.zig").Value;
const ValueType = @import("./expr.zig").ValueType;

/// Represents the state of a variable in an environment
pub const VarState = struct {
    value: Value,
    initialized: bool,
};

/// Provides a hierarchical environment for variable storage and lookup
/// Each environment can have an enclosing (parent) environment forming a chain
pub const Environment = struct {
    variables: std.StringHashMap(VarState),
    enclosing: ?*Environment,
    allocator: std.mem.Allocator,

    /// Initialize a new environment with an optional parent
    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Environment {
        return Environment{
            .variables = std.StringHashMap(VarState).init(allocator),
            .enclosing = enclosing,
            .allocator = allocator,
        };
    }

    /// Clean up resources owned by this environment
    pub fn deinit(self: *Environment) void {
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.value.deinit();
        }
        self.variables.deinit();
    }

    /// Create a deep copy of this environment for closures
    /// This captures the complete state of the environment chain at a point in time
    /// Returns: A heap-allocated copy of this environment
    pub fn deepCopy(self: *Environment) *Environment {
        // Create new environment with same parent
        var new_env = self.allocator.create(Environment) catch unreachable;
        new_env.* = Environment.init(self.allocator, self.enclosing);

        // Copy all variables
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            const name = entry.key_ptr.*;
            const state = entry.value_ptr.*;

            // Copy string values to avoid sharing ownership
            var value_copy = state.value;
            if (value_copy.isString() and value_copy.allocator != null) {
                const str = value_copy.getString();
                const heap_str = value_copy.allocator.?.dupe(u8, str) catch unreachable;
                value_copy = Value{ .data = .{ .string = heap_str }, .allocator = value_copy.allocator };
            }

            // Add to the new environment
            new_env.variables.put(name, .{ .value = value_copy, .initialized = state.initialized }) catch unreachable;
        }

        return new_env;
    }

    /// Define a new variable with a value in this environment
    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.variables.put(name, .{ .value = value, .initialized = true });
    }

    /// Define a new variable without initialization
    pub fn define_uninitialized(self: *Environment, name: []const u8) !void {
        try self.variables.put(name, .{ .value = Value.init(.{ .nil = {} }, null), .initialized = false });
    }

    /// Get a variable's value from this environment or any parent
    /// Returns null if the variable doesn't exist or is uninitialized
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

    /// Assign a value to an existing variable in this environment or any parent
    /// Returns an error if the variable doesn't exist anywhere in the chain
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

    /// Check if this environment or any parent contains a variable
    pub fn containsVariable(self: *Environment, name: []const u8) bool {
        if (self.variables.contains(name)) {
            return true;
        } else if (self.enclosing) |parent| {
            return parent.containsVariable(name);
        } else {
            return false;
        }
    }
};
