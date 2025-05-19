const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const Parser = @import("./parser.zig").Parser;
const Interpreter = @import("./interpreter.zig").Interpreter;
const Declaration = @import("./ast.zig").Declaration;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut().writer();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Check if we're running in REPL mode or file mode
    if (args.len > 2) {
        try stdout.print("Usage: lox [script]\n", .{});
        std.process.exit(64);
    } else if (args.len == 2) {
        // File mode
        try run_file(allocator, args[1]);
    } else {
        // REPL mode
        try run_repl(allocator);
    }
}

fn run_file(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(source);

    try run(allocator, source);
}

fn run_repl(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    var buffer: [1024]u8 = undefined;

    try stdout.print("Lox REPL (press Ctrl+D to exit)\n", .{});

    while (true) {
        try stdout.print("> ", .{});
        const input = stdin.readUntilDelimiter(&buffer, '\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        if (input.len == 0) continue;
        try run(allocator, input);
    }
}

fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    var scanner = try Scanner.init(source);
    var tokens_list = try scanner.scan_tokens();
    defer tokens_list.deinit();

    var parser = Parser.init(tokens_list.items, allocator);
    const declarations = parser.parse() catch {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("Error parsing input.\n", .{});
        return;
    };
    defer {
        for (declarations) |decl| {
            decl.deinit(allocator);
        }
        allocator.free(declarations);
    }

    if (parser.had_error) {
        return;
    }

    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    interpreter.interpret(declarations) catch {
        if (interpreter.runtime_error) |err| {
            const stdout = std.io.getStdOut().writer();
            try stdout.print("Runtime error: {s}\n", .{err.message});
        }
    };
}

// Test routine
test "simple test" {
    const allocator = std.testing.allocator;
    const source = "print 1 + 2 * 3;";
    var scanner = try Scanner.init(source);
    var tokens_list = try scanner.scan_tokens();
    defer tokens_list.deinit();

    const tokens = tokens_list.items;
    var parser = Parser.init(tokens, allocator);
    const declarations = try parser.parse();
    defer {
        for (declarations) |decl| {
            decl.deinit(allocator);
        }
        allocator.free(declarations);
    }

    // Create a buffer to capture output
    var output_buffer = std.ArrayList(u8).init(allocator);
    defer output_buffer.deinit();

    // Create a custom interpreter that writes to our buffer
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();
    interpreter.writer = &output_buffer;

    try interpreter.interpret(declarations);

    // Verify the output - check content without newline
    const trimmed = std.mem.trimRight(u8, output_buffer.items, "\r\n");
    try std.testing.expectEqualStrings("7", trimmed);
}

test "fuzz example" {
    const Context = struct {
        fn test_one(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.test_one, .{});
}
