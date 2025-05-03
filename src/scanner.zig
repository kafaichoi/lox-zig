const std = @import("std");

pub const TokenType = enum { LEFT_PAREN, RIGHT_PARENS, EOF, ERROR };

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,

    pub fn init(token_type: TokenType) Token {
        return Token{ .type = token_type, .lexeme = "", .line = 0 };
    }
};

pub const Scanner = struct {
    source: []const u8,
    curr: usize,

    pub fn init(source: []const u8) !Scanner {
        return Scanner{
            .source = source,
            .curr = 0,
        };
    }

    pub fn scanTokens(self: *Scanner) !std.ArrayList(Token) {
        var tokens = std.ArrayList(Token).init(std.heap.page_allocator);

        while (!self.isAtEnd()) {
            const token = self.scanToken();
            try tokens.append(token);
        }

        try tokens.append(Token.init(TokenType.EOF));
        return tokens;
    }

    pub fn scanToken(self: *Scanner) Token {
        defer self.curr += 1;

        if (self.source[self.curr] == '(') {
            return Token.init(TokenType.LEFT_PAREN);
        }

        if (self.source[self.curr] == ')') {
            return Token.init(TokenType.RIGHT_PARENS);
        }

        return Token.init(TokenType.ERROR);
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.curr >= self.source.len;
    }
};
