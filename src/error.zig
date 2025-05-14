const Token = @import("./scanner.zig").Token;

pub const RuntimeError = struct {
    message: []const u8,
    token: Token,
};
