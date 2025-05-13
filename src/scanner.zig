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

pub const TokenLiteral = union(enum) {
    none: void,
    double: f64,
    string: []const u8,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: TokenLiteral,
    line: usize,

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: usize) Token {
        return Token{ .type = token_type, .lexeme = lexeme, .literal = literal, .line = line };
    }
};

fn is_digit(c: u8) bool {
    return c >= '0' and c <= '9';
}

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

        while (true) {
            // we start at the beginning of the current token
            const token = self.scanToken();
            try tokens.append(token);
            if (token.type == TokenType.EOF) break;
        }

        return tokens;
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();

        self.start = self.curr;
        if (self.isAtEnd()) {
            std.debug.print("EOF\n", .{});
            return self.createToken(TokenType.EOF, .{ .none = {} });
        }

        const c = self.advance();

        const token = switch (c) {
            '(' => self.createToken(TokenType.LEFT_PAREN, .{ .none = {} }),
            ')' => self.createToken(TokenType.RIGHT_PARENS, .{ .none = {} }),
            '{' => self.createToken(TokenType.LEFT_BRACE, .{ .none = {} }),
            '}' => self.createToken(TokenType.RIGHT_BRACE, .{ .none = {} }),
            ',' => self.createToken(TokenType.COMMA, .{ .none = {} }),
            '.' => self.createToken(TokenType.DOT, .{ .none = {} }),
            '-' => self.createToken(TokenType.MINUS, .{ .none = {} }),
            '+' => self.createToken(TokenType.PLUS, .{ .none = {} }),
            ';' => self.createToken(TokenType.SEMICOLON, .{ .none = {} }),
            '*' => self.createToken(TokenType.STAR, .{ .none = {} }),
            '!' => if (self.match('=')) self.createToken(TokenType.BANG_EQUAL, .{ .none = {} }) else self.createToken(TokenType.BANG, .{ .none = {} }),
            '=' => if (self.match('=')) self.createToken(TokenType.EQUAL_EQUAL, .{ .none = {} }) else self.createToken(TokenType.EQUAL, .{ .none = {} }),
            '<' => if (self.match('=')) self.createToken(TokenType.LESS_EQUAL, .{ .none = {} }) else self.createToken(TokenType.LESS, .{ .none = {} }),
            '>' => if (self.match('=')) self.createToken(TokenType.GREATER_EQUAL, .{ .none = {} }) else self.createToken(TokenType.GREATER, .{ .none = {} }),
            // comments are handled in skipWhitespace
            '/' => self.createToken(TokenType.SLASH, .{ .none = {} }),
            '"' => self.string(),
            else => {
                if (is_digit(c)) {
                    return self.number();
                }
                return self.createToken(TokenType.ERROR, .{ .none = {} });
            },
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

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            std.debug.print("Unterminated string.\n", .{});
            return self.createToken(TokenType.ERROR, .{ .none = {} });
        }

        // closing it
        _ = self.advance();

        const literal = self.source[self.start + 1 .. self.curr - 1];
        return self.createToken(TokenType.STRING, .{ .string = literal });
    }

    fn number(self: *Scanner) Token {
        while (is_digit(self.peek())) _ = self.advance();

        // look for a fractional part
        if (self.peek() == '.' and is_digit(self.peek_next())) {
            // consume the '.'
            _ = self.advance();
            while (is_digit(self.peek())) _ = self.advance();
        }

        const literal = self.source[self.start..self.curr];
        const float_value = std.fmt.parseFloat(f64, literal) catch 0;
        return self.createToken(TokenType.NUMBER, .{ .double = float_value });
    }

    fn createToken(self: *Scanner, token_type: TokenType, literal: TokenLiteral) Token {
        const lexeme = self.source[self.start..self.curr];
        return Token.init(token_type, lexeme, literal, self.line);
    }

    fn advance(self: *Scanner) u8 {
        const c = self.source[self.curr];
        self.curr += 1;
        return c;
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.curr];
    }

    fn peek_next(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.curr + 1];
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
    const source = "//";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scanTokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 1), tokens.items.len);
    std.debug.print("token: {s}\n", .{tokens.items[0].lexeme});

    try testing.expectEqual(TokenType.EOF, tokens.items[0].type);
    try testing.expectEqualStrings("", tokens.items[0].lexeme);
    try testing.expectEqual(@as(usize, 1), tokens.items[0].line);
}

test "handle grouping stuff" {
    const source = "(( )){}";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scanTokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 7), tokens.items.len);
    try testing.expectEqual(TokenType.LEFT_PAREN, tokens.items[0].type);
    try testing.expectEqualStrings("(", tokens.items[0].lexeme);
    try testing.expectEqual(TokenType.LEFT_PAREN, tokens.items[1].type);
    try testing.expectEqualStrings("(", tokens.items[1].lexeme);
    try testing.expectEqual(TokenType.RIGHT_PARENS, tokens.items[2].type);
    try testing.expectEqualStrings(")", tokens.items[2].lexeme);
    try testing.expectEqual(TokenType.RIGHT_PARENS, tokens.items[3].type);
    try testing.expectEqualStrings(")", tokens.items[3].lexeme);
    try testing.expectEqual(TokenType.LEFT_BRACE, tokens.items[4].type);
    try testing.expectEqualStrings("{", tokens.items[4].lexeme);
    try testing.expectEqual(TokenType.RIGHT_BRACE, tokens.items[5].type);
    try testing.expectEqualStrings("}", tokens.items[5].lexeme);
    try testing.expectEqual(TokenType.EOF, tokens.items[6].type);
    try testing.expectEqualStrings("", tokens.items[6].lexeme);
    for (tokens.items) |token| {
        try testing.expectEqual(token.line, 1);
    }
}

test "scanner handles single-line string" {
    const source = "\"hello\"";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scanTokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 2), tokens.items.len);
    try testing.expectEqual(TokenType.STRING, tokens.items[0].type);
    try testing.expectEqualStrings("\"hello\"", tokens.items[0].lexeme);
    try testing.expectEqual(TokenType.EOF, tokens.items[1].type);
    try testing.expectEqualStrings("", tokens.items[1].lexeme);
}

test "scanner handles multi-line string" {
    const source = "\"hello\nworld\"";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scanTokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 2), tokens.items.len);
    try testing.expectEqual(TokenType.STRING, tokens.items[0].type);
    try testing.expectEqualStrings("\"hello\nworld\"", tokens.items[0].lexeme);
    try testing.expectEqual(TokenType.EOF, tokens.items[1].type);
    try testing.expectEqualStrings("", tokens.items[1].lexeme);
}

test "scanner handles decimal numbers" {
    const source = "123.456 19";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scanTokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 3), tokens.items.len);
    try testing.expectEqual(TokenType.NUMBER, tokens.items[0].type);
    try testing.expectEqualStrings("123.456", tokens.items[0].lexeme);
    try testing.expectEqual(TokenType.NUMBER, tokens.items[1].type);
    try testing.expectEqualStrings("19", tokens.items[1].lexeme);
    try testing.expectEqual(TokenType.EOF, tokens.items[2].type);
    try testing.expectEqualStrings("", tokens.items[2].lexeme);
}
