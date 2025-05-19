const std = @import("std");
const FuncDecl = @import("./ast.zig").FuncDecl;
const Environment = @import("./environment.zig").Environment;
const Value = @import("./expr.zig").Value;

/// A callable object that can be invoked with arguments
pub const Callable = union(enum) {
    function: FunctionObject,
    native: NativeFunction,

    /// Returns the number of arguments this callable expects
    pub fn arity(self: Callable) usize {
        return switch (self) {
            .function => |f| f.arity(),
            .native => |n| n.arity,
        };
    }
};

/// Represents a user-defined function with its lexical environment (closure)
pub const FunctionObject = struct {
    /// The AST node representing the function declaration
    declaration: *FuncDecl,

    /// The captured environment at the time of function definition
    /// This is what enables closures to access variables from their defining scope
    closure: *Environment,

    /// Creates a new function with its lexical environment
    pub fn init(declaration: *FuncDecl, closure: *Environment) FunctionObject {
        // We store a reference to the current environment (not a copy)
        // This correctly captures the lexical scope
        return FunctionObject{
            .declaration = declaration,
            .closure = closure,
        };
    }

    /// Returns the number of parameters this function expects
    pub fn arity(self: FunctionObject) usize {
        return self.declaration.params.len;
    }
};

/// Represents a built-in function provided by the interpreter
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
