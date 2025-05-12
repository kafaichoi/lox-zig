const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens
    LEFT_PAREN,
    RIGHT_PARENS,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
    ERROR,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,

    pub fn init(token_type: TokenType, lexeme: []const u8, line: usize) Token {
        return Token{ .type = token_type, .lexeme = lexeme, .line = line };
    }
};

pub const Scanner = struct {
    source: []const u8,
    start: usize,
    curr: usize,
    line: usize,

    pub fn init(source: []const u8) !Scanner {
        return Scanner{
            .source = source,
            .start = 0,
            .curr = 0,
            .line = 1,
        };
    }

    pub fn scanTokens(self: *Scanner) !std.ArrayList(Token) {
        var tokens = std.ArrayList(Token).init(std.heap.page_allocator);

        while (!self.isAtEnd()) {
            // we start at the beginning of the current token
            self.start = self.curr;
            const token = self.scanToken();
            try tokens.append(token);
        }

        return tokens;
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();

        if (self.isAtEnd()) return self.createToken(TokenType.EOF);
        const c = self.advance();

        const token = switch (c) {
            '(' => self.createToken(TokenType.LEFT_PAREN),
            ')' => self.createToken(TokenType.RIGHT_PARENS),
            '{' => self.createToken(TokenType.LEFT_BRACE),
            '}' => self.createToken(TokenType.RIGHT_BRACE),
            ',' => self.createToken(TokenType.COMMA),
            '.' => self.createToken(TokenType.DOT),
            '-' => self.createToken(TokenType.MINUS),
            '+' => self.createToken(TokenType.PLUS),
            ';' => self.createToken(TokenType.SEMICOLON),
            '*' => self.createToken(TokenType.STAR),
            '!' => if (self.match('=')) self.createToken(TokenType.BANG_EQUAL) else self.createToken(TokenType.BANG),
            '=' => if (self.match('=')) self.createToken(TokenType.EQUAL_EQUAL) else self.createToken(TokenType.EQUAL),
            '<' => if (self.match('=')) self.createToken(TokenType.LESS_EQUAL) else self.createToken(TokenType.LESS),
            '>' => if (self.match('=')) self.createToken(TokenType.GREATER_EQUAL) else self.createToken(TokenType.GREATER),
            // comments are handled in skipWhitespace
            '/' => self.createToken(TokenType.SLASH),
            else => self.createToken(TokenType.ERROR),
        };

        return token;
    }

    fn skipWhitespace(self: *Scanner) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.match('/')) {
                        while (!self.isAtEnd() and self.peek() != '\n')
                            _ = self.advance();
                    } else {
                        return;
                    }
                },
                else => break,
            }
        }
    }

    fn createToken(self: *Scanner, token_type: TokenType) Token {
        const lexeme = self.source[self.start..self.curr];
        return Token.init(token_type, lexeme, self.line);
    }

    fn advance(self: *Scanner) u8 {
        const c = self.source[self.curr];
        self.curr += 1;
        return c;
    }

    fn peek(self: *Scanner) u8 {
        return self.source[self.curr];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.curr] != expected) return false;
        self.curr += 1;
        return true;
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.curr >= self.source.len;
    }
};

const testing = std.testing;

test "scanner handles comments" {
    const source = "// this is a comment";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scanTokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 1), tokens.items.len);
    std.debug.print("token: {any}\n", .{tokens.items[0]});
    try testing.expect(std.meta.eql(Token.init(TokenType.EOF, "// this is a comment", 1), tokens.items[0]));
}
