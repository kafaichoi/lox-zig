const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const Parser = @import("./parser.zig").Parser;
const Interpreter = @import("./interpreter.zig").Interpreter;
const Value = @import("./expr.zig").Value;
const Expr = @import("./expr.zig").Expr;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var interpreter = Interpreter.init();

    if (args.len > 2) {
        std.debug.print("Usage: lox [script]\n", .{});
        std.process.exit(64);
    } else if (args.len == 2) {
        try runFile(allocator, &interpreter, args[1]);
    } else {
        try runPrompt(allocator, &interpreter);
    }
}

fn runFile(allocator: std.mem.Allocator, interpreter: *Interpreter, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    try run(allocator, interpreter, source);

    if (interpreter.had_error) std.process.exit(65);
    if (interpreter.runtime_error != null) std.process.exit(70);
}

fn runPrompt(allocator: std.mem.Allocator, interpreter: *Interpreter) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input_buffer: [1024]u8 = undefined;

    while (true) {
        try stdout.print("> ", .{});
        const line = stdin.readUntilDelimiterOrEof(&input_buffer, '\n') catch |err| {
            std.debug.print("Error reading input: {}\n", .{err});
            continue;
        };

        if (line == null or line.?.len == 0) {
            break;
        }

        run(allocator, interpreter, line.?) catch |err| {
            std.debug.print("Error: {}\n", .{err});
        };
        interpreter.had_error = false;
        interpreter.runtime_error = null;
    }
}

fn run(allocator: std.mem.Allocator, interpreter: *Interpreter, source: []const u8) !void {
    var scanner = try Scanner.init(source);
    var tokens_list = try scanner.scan_tokens();
    defer tokens_list.deinit();

    const tokens = tokens_list.items;
    var parser = Parser.init(tokens, allocator);
    const expr = try parser.parse();
    defer expr.deinit(allocator);

    const value = try interpreter.interpret(expr);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{Interpreter.stringify(value)});
}

// Test routine
test "simple test" {
    const allocator = std.testing.allocator;
    const source = "1 + 2 * 3";
    var scanner = try Scanner.init(source);
    var tokens_list = try scanner.scan_tokens();
    defer tokens_list.deinit();

    const tokens = tokens_list.items;
    var parser = Parser.init(tokens, allocator);
    const expr = try parser.parse();
    defer expr.deinit(allocator);

    var interpreter = Interpreter.init();
    const value = try interpreter.interpret(expr);

    try std.testing.expectEqual(Value{ .double = 7.0 }, value);
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
