const std = @import("std");
const FuncDecl = @import("./ast.zig").FuncDecl;
const Environment = @import("./environment.zig").Environment;
const Value = @import("./expr.zig").Value;

pub const Callable = union(enum) {
    function: FunctionObject,
    native: NativeFunction,

    pub fn arity(self: Callable) usize {
        return switch (self) {
            .function => |f| f.arity(),
            .native => |n| n.arity,
        };
    }
};

pub const FunctionObject = struct {
    declaration: *FuncDecl,
    closure: *Environment,

    pub fn init(declaration: *FuncDecl, closure: *Environment) FunctionObject {
        // Make a duplicate of the closure environment to avoid use-after-free
        return FunctionObject{
            .declaration = declaration,
            .closure = closure,
        };
    }

    pub fn arity(self: FunctionObject) usize {
        return self.declaration.params.len;
    }
};

pub const NativeFunction = struct {
    function: *const fn (allocator: std.mem.Allocator, arguments: []Value) anyerror!Value,
    arity: usize,

    pub fn init(function: *const fn (allocator: std.mem.Allocator, arguments: []Value) anyerror!Value, arity: usize) NativeFunction {
        return NativeFunction{
            .function = function,
            .arity = arity,
        };
    }
};
