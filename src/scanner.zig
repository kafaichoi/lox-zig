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
    BREAK,

    EOF,
    ERROR,
};

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", .AND },
    .{ "class", .CLASS },
    .{ "else", .ELSE },
    .{ "false", .FALSE },
    .{ "for", .FOR },
    .{ "fun", .FUN },
    .{ "if", .IF },
    .{ "nil", .NIL },
    .{ "or", .OR },
    .{ "print", .PRINT },
    .{ "return", .RETURN },
    .{ "super", .SUPER },
    .{ "this", .THIS },
    .{ "true", .TRUE },
    .{ "var", .VAR },
    .{ "while", .WHILE },
    .{ "break", .BREAK },
});

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

fn is_alpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn is_alphanumeric(c: u8) bool {
    return is_alpha(c) or is_digit(c);
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

    pub fn scan_tokens(self: *Scanner) !std.ArrayList(Token) {
        var tokens = std.ArrayList(Token).init(std.heap.page_allocator);

        while (true) {
            // we start at the beginning of the current token
            const token = self.scan_token();
            try tokens.append(token);
            if (token.type == TokenType.EOF) break;
        }

        return tokens;
    }

    pub fn scan_token(self: *Scanner) Token {
        self.skip_whitespace();

        self.start = self.curr;
        if (self.is_at_end()) {
            return self.create_token(TokenType.EOF, .{ .none = {} });
        }

        const c = self.advance();

        const token = switch (c) {
            '(' => self.create_token(TokenType.LEFT_PAREN, .{ .none = {} }),
            ')' => self.create_token(TokenType.RIGHT_PARENS, .{ .none = {} }),
            '{' => self.create_token(TokenType.LEFT_BRACE, .{ .none = {} }),
            '}' => self.create_token(TokenType.RIGHT_BRACE, .{ .none = {} }),
            ',' => self.create_token(TokenType.COMMA, .{ .none = {} }),
            '.' => self.create_token(TokenType.DOT, .{ .none = {} }),
            '-' => self.create_token(TokenType.MINUS, .{ .none = {} }),
            '+' => self.create_token(TokenType.PLUS, .{ .none = {} }),
            ';' => self.create_token(TokenType.SEMICOLON, .{ .none = {} }),
            '*' => self.create_token(TokenType.STAR, .{ .none = {} }),
            '!' => if (self.match('=')) self.create_token(TokenType.BANG_EQUAL, .{ .none = {} }) else self.create_token(TokenType.BANG, .{ .none = {} }),
            '=' => if (self.match('=')) self.create_token(TokenType.EQUAL_EQUAL, .{ .none = {} }) else self.create_token(TokenType.EQUAL, .{ .none = {} }),
            '<' => if (self.match('=')) self.create_token(TokenType.LESS_EQUAL, .{ .none = {} }) else self.create_token(TokenType.LESS, .{ .none = {} }),
            '>' => if (self.match('=')) self.create_token(TokenType.GREATER_EQUAL, .{ .none = {} }) else self.create_token(TokenType.GREATER, .{ .none = {} }),
            // comments are handled in skip_whitespace
            '/' => self.create_token(TokenType.SLASH, .{ .none = {} }),
            '"' => self.string(),
            else => {
                if (is_digit(c)) {
                    return self.number();
                }
                if (is_alpha(c)) {
                    return self.identifier();
                }
                return self.create_token(TokenType.ERROR, .{ .none = {} });
            },
        };

        return token;
    }

    fn skip_whitespace(self: *Scanner) void {
        while (!self.is_at_end()) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.match('/')) {
                        while (!self.is_at_end() and self.peek() != '\n')
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
        while (self.peek() != '"' and !self.is_at_end()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.is_at_end()) {
            std.debug.print("Unterminated string.\n", .{});
            return self.create_token(TokenType.ERROR, .{ .none = {} });
        }

        // closing it
        _ = self.advance();

        const literal = self.source[self.start + 1 .. self.curr - 1];
        return self.create_token(TokenType.STRING, .{ .string = literal });
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
        return self.create_token(TokenType.NUMBER, .{ .double = float_value });
    }

    fn identifier(self: *Scanner) Token {
        while (is_alphanumeric(self.peek()) or self.peek() == '_') _ = self.advance();

        const text = self.source[self.start..self.curr];
        const token_type = keywords.get(text) orelse TokenType.IDENTIFIER;

        return self.create_token(token_type, .{ .none = {} });
    }

    fn create_token(self: *Scanner, token_type: TokenType, literal: TokenLiteral) Token {
        const lexeme = self.source[self.start..self.curr];
        return Token.init(token_type, lexeme, literal, self.line);
    }

    fn advance(self: *Scanner) u8 {
        const c = self.source[self.curr];
        self.curr += 1;
        return c;
    }

    fn peek(self: *Scanner) u8 {
        if (self.is_at_end()) return 0;
        return self.source[self.curr];
    }

    fn peek_next(self: *Scanner) u8 {
        if (self.is_at_end()) return 0;
        return self.source[self.curr + 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.is_at_end()) return false;
        if (self.source[self.curr] != expected) return false;
        self.curr += 1;
        return true;
    }

    fn is_at_end(self: *Scanner) bool {
        return self.curr >= self.source.len;
    }
};

const testing = std.testing;

test "scanner handles comments" {
    const source = "//";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scan_tokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 1), tokens.items.len);

    try testing.expectEqual(TokenType.EOF, tokens.items[0].type);
    try testing.expectEqualStrings("", tokens.items[0].lexeme);
    try testing.expectEqual(@as(usize, 1), tokens.items[0].line);
}

test "handle grouping stuff" {
    const source = "(( )){}";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scan_tokens();
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
    var tokens = try scanner.scan_tokens();
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
    var tokens = try scanner.scan_tokens();
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
    var tokens = try scanner.scan_tokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 3), tokens.items.len);
    try testing.expectEqual(TokenType.NUMBER, tokens.items[0].type);
    try testing.expectEqualStrings("123.456", tokens.items[0].lexeme);
    try testing.expectEqual(TokenType.NUMBER, tokens.items[1].type);
    try testing.expectEqualStrings("19", tokens.items[1].lexeme);
    try testing.expectEqual(TokenType.EOF, tokens.items[2].type);
    try testing.expectEqualStrings("", tokens.items[2].lexeme);
}

test "scanner handles identifiers" {
    const source = "camelCase underscore_case A1c";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scan_tokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 4), tokens.items.len);
    try testing.expectEqual(TokenType.IDENTIFIER, tokens.items[0].type);
    try testing.expectEqualStrings("camelCase", tokens.items[0].lexeme);
    try testing.expectEqual(TokenType.IDENTIFIER, tokens.items[1].type);
    try testing.expectEqualStrings("underscore_case", tokens.items[1].lexeme);
    try testing.expectEqual(TokenType.IDENTIFIER, tokens.items[2].type);
    try testing.expectEqualStrings("A1c", tokens.items[2].lexeme);
    try testing.expectEqual(TokenType.EOF, tokens.items[3].type);
    try testing.expectEqualStrings("", tokens.items[3].lexeme);
}

test "scanner handles keywords" {
    const source = "and class else false for fun if nil or print return super this true var while";

    var scanner = try Scanner.init(source);
    var tokens = try scanner.scan_tokens();
    defer tokens.deinit();

    try testing.expectEqual(@as(usize, 17), tokens.items.len);
    try testing.expectEqual(TokenType.AND, tokens.items[0].type);
    try testing.expectEqual(TokenType.CLASS, tokens.items[1].type);
    try testing.expectEqual(TokenType.ELSE, tokens.items[2].type);
    try testing.expectEqual(TokenType.FALSE, tokens.items[3].type);
    try testing.expectEqual(TokenType.FOR, tokens.items[4].type);
    try testing.expectEqual(TokenType.FUN, tokens.items[5].type);
    try testing.expectEqual(TokenType.IF, tokens.items[6].type);
    try testing.expectEqual(TokenType.NIL, tokens.items[7].type);
    try testing.expectEqual(TokenType.OR, tokens.items[8].type);
    try testing.expectEqual(TokenType.PRINT, tokens.items[9].type);
    try testing.expectEqual(TokenType.RETURN, tokens.items[10].type);
    try testing.expectEqual(TokenType.SUPER, tokens.items[11].type);
    try testing.expectEqual(TokenType.THIS, tokens.items[12].type);
    try testing.expectEqual(TokenType.TRUE, tokens.items[13].type);
    try testing.expectEqual(TokenType.VAR, tokens.items[14].type);
    try testing.expectEqual(TokenType.WHILE, tokens.items[15].type);
    try testing.expectEqual(TokenType.EOF, tokens.items[16].type);
    try testing.expectEqualStrings("", tokens.items[16].lexeme);
}
