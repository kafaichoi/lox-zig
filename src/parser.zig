const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
pub const Parser = struct {
    tokens: []const Token,
    current: usize,

    // Rule for parsing
    const ParseRule = struct {
        prefix: ?*const ParseFn = null,
        infix: ?*const ParseFn = null,
        precedence: Precedence = .None,
    };

    pub fn init(tokens: []const Token) Parser {
        return Parser{ .tokens = tokens, .current = 0 };
    }

    fn parse(self: *Parser) !void {
        try self.expression();
    }

    fn expression(self: *Parser) !void {
        try self.equality();
    }

    fn synchronize(self: *Parser) void {
        self.advance();

        while (!self.is_at_end()) {
            if (self.previous().type == .SEMICOLON) return;

            switch (self.peek().type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
            }
        }
    }

    fn advance(self: *Parser) void {
        self.current += 1;
    }

    fn check(self: *Parser, t: TokenType) bool {
        if (self.is_at_end()) return false;
        return self.peek().type == t;
    }

    fn is_at_end(self: *Parser) bool {
        return self.peek().type == .EOF;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }
};

const testing = @import("std").testing;

test "parser handles comments" {
    const tokens = [_]Token{
        .{ .type = .NUMBER, .lexeme = "1", .literal = .{ .double = 1.0 }, .line = 1 },
        .{ .type = .NUMBER, .lexeme = "2", .literal = .{ .double = 2.0 }, .line = 1 },
        .{ .type = .EOF, .lexeme = "", .literal = .{ .none = {} }, .line = 1 },
    };
    var parser = Parser.init(tokens);
    parser.parse();
}
