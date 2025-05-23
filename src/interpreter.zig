const std = @import("std");
const Expr = @import("./expr.zig").Expr;
const Value = @import("./expr.zig").Value;
const Stmt = @import("./ast.zig").Stmt;
const Declaration = @import("./ast.zig").Declaration;
const VarDecl = @import("./ast.zig").VarDecl;
const FuncDecl = @import("./ast.zig").FuncDecl;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const ValueType = @import("./expr.zig").ValueType;
const RuntimeError = @import("./error.zig").RuntimeError;
const Environment = @import("./environment.zig").Environment;
const FunctionObject = @import("./callable.zig").FunctionObject;
const Callable = @import("./callable.zig").Callable;
const NativeFunction = @import("./callable.zig").NativeFunction;

pub const ReturnValue = struct {
    value: Value,
};

// Runtime error type
pub const Interpreter = struct {
    runtime_error: ?RuntimeError,
    had_error: bool,
    allocator: std.mem.Allocator,
    writer: ?*std.ArrayList(u8),
    environment: *Environment,
    return_value: ?ReturnValue,

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        const env = allocator.create(Environment) catch unreachable;
        env.* = Environment.init(allocator, null);

        var interpreter = Interpreter{
            .runtime_error = null,
            .had_error = false,
            .allocator = allocator,
            .writer = null,
            .environment = env,
            .return_value = null,
        };

        // Define native functions
        interpreter.define_native_functions();

        return interpreter;
    }

    pub fn deinit(self: *Interpreter) void {
        // Clean up any error message
        if (self.runtime_error) |*err| {
            self.allocator.free(err.message);
        }

        // Clean up any return values
        if (self.return_value) |*ret_val| {
            ret_val.value.deinit();
        }

        // Clean up the global environment
        self.environment.deinit();
        self.allocator.destroy(self.environment);
    }

    pub fn interpret(self: *Interpreter, declarations: []*Declaration) !void {
        for (declarations) |decl| {
            try self.execute_declaration(decl);
        }
    }

    fn execute_declaration(self: *Interpreter, decl: *Declaration) !void {
        switch (decl.*) {
            .stmt => |stmt| try self.execute(stmt),
            .var_decl => |var_decl| try self.execute_var_decl(var_decl),
            .func_decl => |func_decl| try self.execute_func_decl(func_decl),
        }
    }

    fn execute_var_decl(self: *Interpreter, decl: *VarDecl) !void {
        if (decl.initializer) |init_expr| {
            const value = try self.evaluate(init_expr);
            try self.environment.define(decl.name, value);
        } else {
            // Do not initialize the variable
            try self.environment.define_uninitialized(decl.name);
        }
    }

    /// Defines a function in the current environment
    /// Creates a deep copy of the current environment to capture the lexical scope
    fn execute_func_decl(self: *Interpreter, decl: *FuncDecl) !void {
        // Create a deep copy of the current environment for the closure
        // This ensures the function captures its lexical environment at declaration time
        const closure_env = self.environment.deepCopy();
        const function = FunctionObject.init(decl, closure_env);

        // Define the function in the current environment
        try self.environment.define(decl.name, Value.init(.{ .callable = .{ .function = function } }, null));
    }

    /// Executes a function with the given arguments
    /// Returns the function's return value, or nil if none is provided
    fn execute_function(self: *Interpreter, function: FunctionObject, arguments: []Value) !Value {
        // Create a new environment for this function call, with the closure as parent
        var function_env = self.allocator.create(Environment) catch unreachable;
        function_env.* = Environment.init(self.allocator, function.closure);

        // Bind parameters to arguments
        const param_count = function.declaration.params.len;
        const arg_count = arguments.len;
        const bind_count = @min(param_count, arg_count);

        for (0..bind_count) |i| {
            const param = function.declaration.params[i];
            const arg = arguments[i];
            try function_env.define(param.lexeme, arg);
        }

        // Switch to the function's environment
        const previous_env = self.environment;
        self.environment = function_env;

        // NOTE ON MEMORY MANAGEMENT:
        // We deliberately don't free the function's environment when we're done.
        // In a production language, this would be a memory leak, but it allows closures
        // to work correctly by preserving their environments. A real implementation would
        // use garbage collection or reference counting to clean up environments when they're
        // no longer needed.
        defer self.environment = previous_env;

        // Execute the function body
        const body = function.declaration.body;
        if (body.* != .block) {
            const message = try self.allocator.dupe(u8, "Function body must be a block statement.");
            self.runtime_error = RuntimeError{
                .message = message,
                .token = function.declaration.params[0],
            };
            self.had_error = true;
            return error.RuntimeError;
        }

        // Try to execute the function body and handle returns
        const block_stmt = body.block;
        self.execute_block(block_stmt.statements) catch |err| {
            switch (err) {
                error.Return => {
                    // Return value was stored in the return_value field
                    if (self.return_value) |ret_value| {
                        const result = ret_value.value;
                        self.return_value = null;
                        return result;
                    }
                    return Value.init(.{ .nil = {} }, null);
                },
                else => return err,
            }
        };

        // No explicit return, so return nil
        return Value.init(.{ .nil = {} }, null);
    }

    fn execute_block(self: *Interpreter, statements: []*Declaration) anyerror!void {
        for (statements) |stmt| {
            try self.execute_declaration(stmt);
        }
    }

    fn call_function(self: *Interpreter, callable: Callable, arguments: []Value) !Value {
        switch (callable) {
            .function => |function| {
                return self.execute_function(function, arguments);
            },
            .native => |native| {
                return native.function(self.allocator, arguments);
            },
        }
    }

    fn execute(self: *Interpreter, stmt: *Stmt) anyerror!void {
        switch (stmt.*) {
            .print => |p| {
                var value = try self.evaluate(p.expression);
                defer value.deinit();
                const str = self.stringify(value);
                defer if (@as(std.meta.Tag(ValueType), value.data) == .double) self.allocator.free(str);
                if (self.writer) |writer| {
                    try writer.appendSlice(str);
                    try writer.appendSlice("\n");
                } else {
                    const stdout = std.io.getStdOut().writer();
                    try stdout.print("{s}\n", .{str});
                }
            },
            .expression => |e| {
                var value = try self.evaluate(e.expression);
                defer value.deinit();
            },
            .block => |b| {
                // Create a new environment for the block
                var block_env = self.allocator.create(Environment) catch unreachable;
                block_env.* = Environment.init(self.allocator, self.environment);
                const previous = self.environment;
                self.environment = block_env;
                defer {
                    self.environment = previous;
                    block_env.deinit();
                    self.allocator.destroy(block_env);
                }
                for (b.statements) |decl| {
                    self.execute_declaration(decl) catch |err| switch (err) {
                        error.Break, error.Return => return err,
                        else => return err,
                    };
                }
            },
            .if_stmt => |i| {
                var cond_value = try self.evaluate(i.condition);
                defer cond_value.deinit();
                if (is_truthy(cond_value)) {
                    try self.execute(i.then_branch);
                } else if (i.else_branch) |else_stmt| {
                    try self.execute(else_stmt);
                }
            },
            .while_stmt => |w| {
                while (is_truthy(try self.evaluate(w.condition))) {
                    self.execute(w.body) catch |err| switch (err) {
                        error.Break => break,
                        error.Return => return error.Return,
                        else => return err,
                    };
                }
            },
            .break_stmt => |_| {
                return error.Break;
            },
            .return_stmt => |r| {
                var value = Value.init(.{ .nil = {} }, null);
                if (r.value) |value_expr| {
                    value = try self.evaluate(value_expr);
                }

                // Store the return value for retrieval in execute_function
                self.return_value = ReturnValue{ .value = value };

                return error.Return;
            },
        }
    }

    fn evaluate(self: *Interpreter, expr: *Expr) !Value {
        return switch (expr.*) {
            .binary => |b| try self.evaluate_binary(b),
            .unary => |u| try self.evaluate_unary(u),
            .grouping => |g| try self.evaluate(g.expression),
            .literal => |l| l.value,
            .variable => |var_expr| {
                const name = var_expr.name.lexeme;
                if (self.environment.get(name)) |value| {
                    return value;
                } else {
                    const message = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'.", .{name});
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = var_expr.name,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
            },
            .assign => |a| {
                const value = try self.evaluate(a.value);

                self.environment.assign(a.name.lexeme, value) catch {
                    const message = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'.", .{a.name.lexeme});
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = a.name,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                };

                return value;
            },
            .logical => |l| try self.evaluate_logical(l),
            .call => |call| try self.evaluate_call(call),
        };
    }

    fn evaluate_binary(self: *Interpreter, expr: *Expr.BinaryExpr) anyerror!Value {
        var left = try self.evaluate(expr.left);
        defer left.deinit();
        var right = try self.evaluate(expr.right);
        defer right.deinit();

        switch (expr.operator.type) {
            .MINUS => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .double = left.getNumber() - right.getNumber() }, null);
            },
            .SLASH => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                if (right.getNumber() == 0) {
                    const message = try self.allocator.dupe(u8, "Division by zero.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .double = left.getNumber() / right.getNumber() }, null);
            },
            .STAR => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .double = left.getNumber() * right.getNumber() }, null);
            },
            .PLUS => {
                if (left.isNumber() and right.isNumber()) {
                    return Value.init(.{ .double = left.getNumber() + right.getNumber() }, null);
                }
                if (left.isString() and right.isString()) {
                    var result = std.ArrayList(u8).init(self.allocator);
                    try result.appendSlice(left.getString());
                    try result.appendSlice(right.getString());
                    const concatenated = try result.toOwnedSlice();
                    return Value{
                        .data = .{ .string = concatenated },
                        .allocator = self.allocator,
                    };
                }
                const message = try self.allocator.dupe(u8, "Operands must be two numbers or two strings.");
                self.runtime_error = RuntimeError{
                    .message = message,
                    .token = expr.operator,
                };
                self.had_error = true;
                return error.RuntimeError;
            },
            .GREATER => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .boolean = left.getNumber() > right.getNumber() }, null);
            },
            .GREATER_EQUAL => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .boolean = left.getNumber() >= right.getNumber() }, null);
            },
            .LESS => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .boolean = left.getNumber() < right.getNumber() }, null);
            },
            .LESS_EQUAL => {
                if (!left.isNumber() or !right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operands must be numbers.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .boolean = left.getNumber() <= right.getNumber() }, null);
            },
            .BANG_EQUAL => return Value.init(.{ .boolean = !is_equal(left, right) }, null),
            .EQUAL_EQUAL => return Value.init(.{ .boolean = is_equal(left, right) }, null),
            else => {
                const message = try self.allocator.dupe(u8, "Invalid binary operator.");
                self.runtime_error = RuntimeError{
                    .message = message,
                    .token = expr.operator,
                };
                self.had_error = true;
                return error.RuntimeError;
            },
        }
    }

    fn evaluate_unary(self: *Interpreter, expr: *Expr.UnaryExpr) anyerror!Value {
        var right = try self.evaluate(expr.right);
        defer right.deinit();

        switch (expr.operator.type) {
            .MINUS => {
                if (!right.isNumber()) {
                    const message = try self.allocator.dupe(u8, "Operand must be a number.");
                    self.runtime_error = RuntimeError{
                        .message = message,
                        .token = expr.operator,
                    };
                    self.had_error = true;
                    return error.RuntimeError;
                }
                return Value.init(.{ .double = -right.getNumber() }, null);
            },
            .BANG => return Value.init(.{ .boolean = !is_truthy(right) }, null),
            else => {
                const message = try self.allocator.dupe(u8, "Invalid unary operator.");
                self.runtime_error = RuntimeError{
                    .message = message,
                    .token = expr.operator,
                };
                self.had_error = true;
                return error.RuntimeError;
            },
        }
    }

    fn evaluate_logical(self: *Interpreter, expr: *Expr.LogicalExpr) anyerror!Value {
        var left = try self.evaluate(expr.left);
        defer left.deinit();

        switch (expr.operator.type) {
            .OR => {
                if (is_truthy(left)) return left;
            },
            .AND => {
                if (!is_truthy(left)) return left;
            },
            else => unreachable,
        }

        return try self.evaluate(expr.right);
    }

    fn is_truthy(value: Value) bool {
        return switch (value.data) {
            .nil => false,
            .boolean => |b| b,
            else => true,
        };
    }

    fn is_equal(a: Value, b: Value) bool {
        if (@as(std.meta.Tag(ValueType), a.data) != @as(std.meta.Tag(ValueType), b.data)) return false;
        return switch (a.data) {
            .nil => true,
            .boolean => |a_bool| a_bool == b.getBoolean(),
            .double => |a_num| a_num == b.getNumber(),
            .string => |a_str| std.mem.eql(u8, a_str, b.getString()),
            .callable => |a_func| switch (a_func) {
                .function => |func| func.declaration == b.getFunction(),
                .native => false, // Native functions are never equal to other functions
            },
            .none => false,
        };
    }

    fn stringify(self: *Interpreter, value: Value) []const u8 {
        return switch (value.data) {
            .nil => "nil",
            .boolean => |b| if (b) "true" else "false",
            .double => |n| blk: {
                const str = std.fmt.allocPrint(self.allocator, "{d}", .{n}) catch "number";
                break :blk str;
            },
            .string => |s| s,
            .callable => |f| blk: {
                const name = switch (f) {
                    .function => |func| func.declaration.name,
                    .native => "native_function",
                };
                const str = std.fmt.allocPrint(self.allocator, "<fn {s}>", .{name}) catch "<function>";
                break :blk str;
            },
            .none => "none",
        };
    }

    fn free_value(self: *Interpreter, value: Value) void {
        switch (value.data) {
            .string => |s| self.allocator.free(s),
            else => {},
        }
    }

    fn evaluate_call(self: *Interpreter, expr: *Expr.CallExpr) anyerror!Value {
        // Evaluate the callee
        var callee_value = try self.evaluate(expr.callee);
        defer callee_value.deinit();

        // Evaluate all arguments
        var arguments = std.ArrayList(Value).init(self.allocator);
        defer {
            for (arguments.items) |*arg| {
                arg.deinit();
            }
            arguments.deinit();
        }

        for (expr.arguments) |arg_expr| {
            const arg_value = try self.evaluate(arg_expr);
            try arguments.append(arg_value);
        }

        // Check if the callee is callable
        if (!callee_value.isFunction()) {
            const message = try self.allocator.dupe(u8, "Can only call functions and classes.");
            self.runtime_error = RuntimeError{
                .message = message,
                .token = expr.paren,
            };
            self.had_error = true;
            return error.RuntimeError;
        }

        // Get the callable
        var callable = callee_value.data.callable;

        // Check arity
        if (arguments.items.len != callable.arity()) {
            const message = try std.fmt.allocPrint(self.allocator, "Expected {d} arguments but got {d}.", .{ callable.arity(), arguments.items.len });
            self.runtime_error = RuntimeError{
                .message = message,
                .token = expr.paren,
            };
            self.had_error = true;
            return error.RuntimeError;
        }

        // Call the function
        return self.call_function(callable, arguments.items);
    }

    // Define built-in native functions
    fn define_native_functions(self: *Interpreter) void {
        // clock() - Returns the current time in seconds
        const clock_fn = struct {
            fn clockFn(allocator: std.mem.Allocator, arguments: []Value) anyerror!Value {
                _ = arguments; // Unused
                const seconds = @as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0;
                return Value.init(.{ .double = seconds }, allocator);
            }
        }.clockFn;

        const clock_native = NativeFunction.init(clock_fn, 0);
        const clock_callable = Callable{ .native = clock_native };
        self.environment.define("clock", Value.init(.{ .callable = clock_callable }, null)) catch unreachable;
    }

    // Debug helper to print details of an Environment
    fn debugEnvironment(self: *Interpreter, env: *Environment, prefix: []const u8) void {
        std.debug.print("{s} Env: {*}\n", .{ prefix, env });
        if (env.enclosing) |enclosing| {
            std.debug.print("{s} Enclosing: {*}\n", .{ prefix, enclosing });
        } else {
            std.debug.print("{s} No enclosing\n", .{prefix});
        }

        // Print some variables if available
        _ = env.get("i"); // Try to get a variable for testing
        _ = self; // To avoid unused parameter warning
    }

    // For debugging only
    fn printEnvironments(self: *Interpreter) void {
        std.debug.print("===== ENVIRONMENTS =====\n", .{});
        var env_ptr: ?*Environment = self.environment;
        var i: usize = 0;
        while (env_ptr != null) : (i += 1) {
            std.debug.print("Environment {d} at address {*}\n", .{ i, env_ptr });

            // Print variables in this environment
            var it = env_ptr.?.variables.iterator();
            while (it.next()) |entry| {
                std.debug.print("  Variable: {s}\n", .{entry.key_ptr.*});
            }

            // Go to next environment
            env_ptr = env_ptr.?.enclosing;
        }
        std.debug.print("=======================\n", .{});
    }
};

test "stringify" {
    var interpreter = Interpreter.init(std.testing.allocator);
    defer interpreter.deinit();
    // Test nil
    try std.testing.expectEqualStrings("nil", interpreter.stringify(Value.init(.{ .nil = {} }, null)));

    // Test booleans
    try std.testing.expectEqualStrings("true", interpreter.stringify(Value.init(.{ .boolean = true }, null)));
    try std.testing.expectEqualStrings("false", interpreter.stringify(Value.init(.{ .boolean = false }, null)));

    // Test numbers - note that integers are printed without decimal point
    const str_42 = interpreter.stringify(Value.init(.{ .double = 42 }, null));
    try std.testing.expectEqualStrings("42", str_42);
    std.testing.allocator.free(str_42);
    const str_314 = interpreter.stringify(Value.init(.{ .double = 3.14 }, null));
    try std.testing.expectEqualStrings("3.14", str_314);
    std.testing.allocator.free(str_314);
    const str_0 = interpreter.stringify(Value.init(.{ .double = 0 }, null));
    try std.testing.expectEqualStrings("0", str_0);
    std.testing.allocator.free(str_0);
    const str_neg1 = interpreter.stringify(Value.init(.{ .double = -1 }, null));
    try std.testing.expectEqualStrings("-1", str_neg1);
    std.testing.allocator.free(str_neg1);

    // Test strings - use init_borrowed to avoid ownership issues in the test
    const hello_str = Value.init_borrowed(.{ .string = "hello" });
    try std.testing.expectEqualStrings("hello", interpreter.stringify(hello_str));

    // Test none
    try std.testing.expectEqualStrings("none", interpreter.stringify(Value.init(.{ .none = {} }, null)));
}

test "variable declaration and access" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a variable declaration
    const name = Token{ .type = .IDENTIFIER, .lexeme = "x", .literal = .{ .none = {} }, .line = 1 };
    const value = try Expr.LiteralExpr.create(allocator, Value.init(.{ .double = 42.0 }, null));
    const var_decl = try VarDecl.create(allocator, name.lexeme, value);
    defer var_decl.deinit(allocator);

    // Execute the declaration
    try interpreter.execute_var_decl(var_decl.var_decl);

    // Create a variable expression to access it
    const var_expr = try Expr.VariableExpr.create(allocator, name);
    defer var_expr.deinit(allocator);

    // Evaluate the variable
    const result = try interpreter.evaluate(var_expr);
    try std.testing.expectEqual(Value.init(.{ .double = 42.0 }, null), result);
}

test "undefined variable error" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a variable expression for an undefined variable
    const name = Token{ .type = .IDENTIFIER, .lexeme = "undefined", .literal = .{ .none = {} }, .line = 1 };
    const var_expr = try Expr.VariableExpr.create(allocator, name);
    defer var_expr.deinit(allocator);

    // Try to evaluate the undefined variable
    const result = interpreter.evaluate(var_expr);
    try std.testing.expectError(error.RuntimeError, result);
    try std.testing.expect(interpreter.had_error);
    try std.testing.expect(interpreter.runtime_error != null);
    if (interpreter.runtime_error) |err| {
        try std.testing.expectEqualStrings("Undefined variable 'undefined'.", err.message);
    }
}

test "string concatenation" {
    var interpreter = Interpreter.init(std.testing.allocator);
    defer interpreter.deinit();

    const plus_op = Token{ .type = .PLUS, .lexeme = "+", .literal = .{ .none = {} }, .line = 1 };
    const left_str = Value.init_borrowed(.{ .string = "hello " });
    const right_str = Value.init_borrowed(.{ .string = "world" });
    const concat_expr = try Expr.BinaryExpr.create(
        std.testing.allocator,
        try Expr.LiteralExpr.create(std.testing.allocator, left_str),
        plus_op,
        try Expr.LiteralExpr.create(std.testing.allocator, right_str),
    );
    defer concat_expr.deinit(std.testing.allocator);

    var result = try interpreter.evaluate(concat_expr);
    defer result.deinit();

    try std.testing.expectEqualStrings("hello world", result.getString());
}

test "type error handling" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a number and a string
    const number = try Expr.LiteralExpr.create(allocator, Value.init(.{ .double = 42.0 }, null));
    const string = try Expr.LiteralExpr.create(allocator, Value.init_borrowed(.{ .string = "hello" }));

    // Try to add them
    const operator = Token{ .type = .PLUS, .lexeme = "+", .literal = .{ .none = {} }, .line = 1 };
    const binary = try Expr.BinaryExpr.create(allocator, number, operator, string);
    defer binary.deinit(allocator);

    // Evaluate should fail with a type error
    const result = interpreter.evaluate(binary);
    try std.testing.expectError(error.RuntimeError, result);
    try std.testing.expect(interpreter.had_error);
    try std.testing.expect(interpreter.runtime_error != null);
    if (interpreter.runtime_error) |err| {
        try std.testing.expectEqualStrings("Operands must be two numbers or two strings.", err.message);
    }
}

test "boolean operations" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Test true and false literals
    const true_lit = try Expr.LiteralExpr.create(allocator, Value.init(.{ .boolean = true }, null));
    const false_lit = try Expr.LiteralExpr.create(allocator, Value.init(.{ .boolean = false }, null));

    // Test equality
    const eq_op = Token{ .type = .EQUAL_EQUAL, .lexeme = "==", .literal = .{ .none = {} }, .line = 1 };
    const eq_expr = try Expr.BinaryExpr.create(allocator, true_lit, eq_op, false_lit);
    defer eq_expr.deinit(allocator);

    const eq_result = try interpreter.evaluate(eq_expr);
    try std.testing.expectEqual(Value.init(.{ .boolean = false }, null), eq_result);

    // Test not equal
    const neq_op = Token{ .type = .BANG_EQUAL, .lexeme = "!=", .literal = .{ .none = {} }, .line = 1 };
    const neq_expr = try Expr.BinaryExpr.create(allocator, try Expr.LiteralExpr.create(allocator, Value.init(.{ .boolean = true }, null)), neq_op, try Expr.LiteralExpr.create(allocator, Value.init(.{ .boolean = false }, null)));
    defer neq_expr.deinit(allocator);

    const neq_result = try interpreter.evaluate(neq_expr);
    try std.testing.expectEqual(Value.init(.{ .boolean = true }, null), neq_result);
}

test "nil handling" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create nil literals
    const nil1 = try Expr.LiteralExpr.create(allocator, Value.init(.{ .nil = {} }, null));
    const nil2 = try Expr.LiteralExpr.create(allocator, Value.init(.{ .nil = {} }, null));
    // Note: We don't need to free nil1 and nil2 as they are owned by the binary expressions

    // Test nil equality
    const eq_op = Token{ .type = .EQUAL_EQUAL, .lexeme = "==", .literal = .{ .none = {} }, .line = 1 };
    const eq_expr = try Expr.BinaryExpr.create(allocator, nil1, eq_op, nil2);
    defer eq_expr.deinit(allocator);

    const eq_result = try interpreter.evaluate(eq_expr);
    try std.testing.expectEqual(Value.init(.{ .boolean = true }, null), eq_result);

    // Test nil with non-nil
    const number = try Expr.LiteralExpr.create(allocator, Value.init(.{ .double = 42.0 }, null));
    const nil1_2 = try Expr.LiteralExpr.create(allocator, Value.init(.{ .nil = {} }, null));
    // Note: We don't need to free number or nil1_2 as they are owned by neq_expr

    const neq_expr = try Expr.BinaryExpr.create(allocator, nil1_2, eq_op, number);
    defer neq_expr.deinit(allocator);

    const neq_result = try interpreter.evaluate(neq_expr);
    try std.testing.expectEqual(Value.init(.{ .boolean = false }, null), neq_result);
}
