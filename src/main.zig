fn runFile(path: []const u8) !void {
    std.debug.print("Running script: {s}\n", .{path});
    // signal command is valid
    std.process.exit(0);
}

fn repl() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var line: [256]u8 = undefined;
    while (true) {
        try stdout.print("> ", .{});
        const optional_str = try stdin.readUntilDelimiterOrEof(line[0..], '\n');
        // when it's null, it means we received kill signal like SIGINT
        if (optional_str) |str| {
            try run(str);
        } else {
            break;
        }
    }
}

fn run(source: []const u8) !void {
    var scanner = try Scanner.init(source);
    const tokens = try scanner.scanTokens();
    defer tokens.deinit();
    for (tokens.items) |token| {
        std.debug.print("Token: {any}\n", .{token.type});
    }
}

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    const allocator = std.heap.smp_allocator;
    const args = try process.argsAlloc(allocator);
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    // there will be always at least one element in args
    // which is the executable path
    if (args.len > 2) {
        try stdout.print("Usage: jlox [script]\n", .{});
        // signal command is invalid
        std.process.exit(64);
    } else if (args.len == 2) {
        std.debug.print("args: {s}\n", .{args});
        try runFile(args[0]);
    } else {
        try repl();
    }

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // Don't forget to flush!
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
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

const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const process = std.process;
