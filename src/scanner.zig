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

        try tokens.append(Token.init(TokenType.EOF, "", self.line));
        return tokens;
    }

    pub fn scanToken(self: *Scanner) Token {
        const c = self.advance();

        const token = switch (c) {
            '(' => self.addToken(TokenType.LEFT_PAREN),
            ')' => self.addToken(TokenType.RIGHT_PARENS),
            '{' => self.addToken(TokenType.LEFT_BRACE),
            '}' => self.addToken(TokenType.RIGHT_BRACE),
            ',' => self.addToken(TokenType.COMMA),
            '.' => self.addToken(TokenType.DOT),
            '-' => self.addToken(TokenType.MINUS),
            '+' => self.addToken(TokenType.PLUS),
            ';' => self.addToken(TokenType.SEMICOLON),
            '*' => self.addToken(TokenType.STAR),
            '!' => self.addToken(TokenType.BANG),
            '=' => self.addToken(TokenType.EQUAL),
            else => self.addToken(TokenType.ERROR),
        };

        return token;
    }

    fn addToken(self: *Scanner, token_type: TokenType) Token {
        const lexeme = self.source[self.start..self.curr];
        return Token.init(token_type, lexeme, self.line);
    }

    fn advance(self: *Scanner) u8 {
        const c = self.source[self.curr];
        self.curr += 1;
        return c;
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
