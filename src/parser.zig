const std = @import("std");
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Expr = @import("./expr.zig").Expr;
const Value = @import("./expr.zig").Value;
const Allocator = std.mem.Allocator;

// Parser error type
pub const ParseError = error{
    ParseError,
};

// Add this error set that includes ParseError and Allocator errors
pub const ParserError = error{
    ParseError,
    OutOfMemory,
};

pub const Parser = struct {
    tokens: []const Token,
    current: usize,
    allocator: Allocator,
    had_error: bool,

    // Precedence levels for expressions
    const Precedence = enum(u8) {
        None,
        Assignment, // =
        Or, // or
        And, // and
        Equality, // == !=
        Comparison, // < > <= >=
        Term, // + -
        Factor, // * /
        Unary, // ! -
        Call, // . ()
        Primary,
    };

    pub fn init(tokens: []const Token, allocator: Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
            .had_error = false,
        };
    }

    pub fn parse(self: *Parser) ParserError!*Expr {
        return self.expression() catch |err| {
            if (err == error.ParseError) {
                self.synchronize();
                return error.ParseError;
            }
            return err;
        };
    }

    fn expression(self: *Parser) ParserError!*Expr {
        return try self.equality();
    }

    fn equality(self: *Parser) ParserError!*Expr {
        var expr = try self.comparison();

        while (self.match(&.{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            const operator = self.previous();
            const right = try self.comparison();
            expr = try Expr.BinaryExpr.create(self.allocator, expr, operator, right);
        }

        return expr;
    }

    fn comparison(self: *Parser) ParserError!*Expr {
        var expr = try self.term();

        while (self.match(&.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const operator = self.previous();
            const right = try self.term();
            expr = try Expr.BinaryExpr.create(self.allocator, expr, operator, right);
        }

        return expr;
    }

    fn term(self: *Parser) ParserError!*Expr {
        var expr = try self.factor();

        while (self.match(&.{ .MINUS, .PLUS })) {
            const operator = self.previous();
            const right = try self.factor();
            expr = try Expr.BinaryExpr.create(self.allocator, expr, operator, right);
        }

        return expr;
    }

    fn factor(self: *Parser) ParserError!*Expr {
        var expr = try self.unary();

        while (self.match(&.{ .SLASH, .STAR })) {
            const operator = self.previous();
            const right = try self.unary();
            expr = try Expr.BinaryExpr.create(self.allocator, expr, operator, right);
        }

        return expr;
    }

    fn unary(self: *Parser) ParserError!*Expr {
        if (self.match(&.{ .BANG, .MINUS })) {
            const operator = self.previous();
            const right = try self.unary();
            return try Expr.UnaryExpr.create(self.allocator, operator, right);
        }

        return self.primary();
    }

    fn primary(self: *Parser) ParserError!*Expr {
        if (self.match(&.{.FALSE})) return try Expr.LiteralExpr.create(self.allocator, .{ .boolean = false });
        if (self.match(&.{.TRUE})) return try Expr.LiteralExpr.create(self.allocator, .{ .boolean = true });
        if (self.match(&.{.NIL})) return try Expr.LiteralExpr.create(self.allocator, .{ .nil = {} });

        if (self.match(&.{ .NUMBER, .STRING })) {
            const token_literal = self.previous().literal;
            // Convert from TokenLiteral to Value
            const value: Value = switch (token_literal) {
                .double => |d| Value{ .double = d },
                .string => |s| Value{ .string = s },
                else => Value{ .nil = {} },
            };
            return try Expr.LiteralExpr.create(self.allocator, value);
        }

        if (self.match(&.{.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = try self.consume(.RIGHT_PARENS, "Expect ')' after expression.");
            return try Expr.GroupingExpr.create(self.allocator, expr);
        }

        return self.errorExpr("Expect expression.");
    }

    fn match(self: *Parser, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                _ = self.advance(); // Store the result to avoid the ignored value error
                return true;
            }
        }
        return false;
    }

    fn consume(self: *Parser, t: TokenType, message: []const u8) ParseError!Token {
        if (self.check(t)) {
            _ = self.advance(); // Store the result to avoid the ignored value error
            return self.previous();
        }

        return self.errorToken(message);
    }

    fn errorExpr(self: *Parser, message: []const u8) ParseError {
        self.had_error = true;
        // Report error at current token
        std.debug.print("Error at {s}: {s}\n", .{ self.peek().lexeme, message });
        return ParseError.ParseError;
    }

    fn errorToken(self: *Parser, message: []const u8) ParseError {
        self.had_error = true;
        // Report error at current token
        std.debug.print("Error at {s}: {s}\n", .{ self.peek().lexeme, message });
        return ParseError.ParseError;
    }

    fn synchronize(self: *Parser) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().type == .SEMICOLON) return;

            switch (self.peek().type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {},
            }

            _ = self.advance();
        }
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn check(self: *Parser, t: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == t;
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == .EOF;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }
};

test "simple parse test" {
    const tokens = [_]Token{
        .{ .type = .NUMBER, .lexeme = "1", .literal = .{ .double = 1.0 }, .line = 1 },
        .{ .type = .PLUS, .lexeme = "+", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .NUMBER, .lexeme = "2", .literal = .{ .double = 2.0 }, .line = 1 },
        .{ .type = .EOF, .lexeme = "", .literal = .{ .none = {} }, .line = 1 },
    };
    var parser = Parser.init(&tokens, std.testing.allocator);
    const expr = try parser.parse();
    defer expr.deinit(std.testing.allocator);

    // Verify this is a binary expression
    switch (expr.*) {
        .binary => |b| {
            // Check left operand is 1
            switch (b.left.*) {
                .literal => |l| {
                    try std.testing.expectEqual(Value{ .double = 1.0 }, l.value);
                },
                else => try std.testing.expect(false),
            }

            // Check operator is +
            try std.testing.expectEqual(TokenType.PLUS, b.operator.type);

            // Check right operand is 2
            switch (b.right.*) {
                .literal => |l| {
                    try std.testing.expectEqual(Value{ .double = 2.0 }, l.value);
                },
                else => try std.testing.expect(false),
            }
        },
        else => try std.testing.expect(false),
    }
}
