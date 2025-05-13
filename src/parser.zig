const std = @import("std");
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Expr = @import("./expr.zig").Expr;
const Value = @import("./expr.zig").Value;
const Stmt = @import("./stmt.zig").Stmt;
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

    pub fn parse(self: *Parser) ParserError![]*Stmt {
        var statements = std.ArrayList(*Stmt).init(self.allocator);

        while (!self.isAtEnd()) {
            const stmt = try self.declaration();
            try statements.append(stmt);
        }

        return statements.toOwnedSlice();
    }

    fn declaration(self: *Parser) ParserError!*Stmt {
        return self.statement();
    }

    fn statement(self: *Parser) ParserError!*Stmt {
        if (self.match(&.{.PRINT})) {
            return self.printStatement();
        }

        return self.expressionStatement();
    }

    fn printStatement(self: *Parser) ParserError!*Stmt {
        const value = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after value.");
        return try Stmt.PrintStmt.create(self.allocator, value);
    }

    fn expressionStatement(self: *Parser) ParserError!*Stmt {
        const expr = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after expression.");
        return try Stmt.ExpressionStmt.create(self.allocator, expr);
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

test "simple parse test 1+2;" {
    const tokens = [_]Token{
        .{ .type = .NUMBER, .lexeme = "1", .literal = .{ .double = 1.0 }, .line = 1 },
        .{ .type = .PLUS, .lexeme = "+", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .NUMBER, .lexeme = "2", .literal = .{ .double = 2.0 }, .line = 1 },
        .{ .type = .SEMICOLON, .lexeme = ";", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .EOF, .lexeme = "", .literal = .{ .none = {} }, .line = 1 },
    };
    var parser = Parser.init(&tokens, std.testing.allocator);
    const statements = try parser.parse();
    defer {
        for (statements) |stmt| {
            stmt.deinit(std.testing.allocator);
        }
        std.testing.allocator.free(statements);
    }

    // Verify we have exactly one statement
    try std.testing.expectEqual(@as(usize, 1), statements.len);

    // Verify this is an expression statement
    const stmt = statements[0];
    try std.testing.expectEqual(Stmt.expression, @as(std.meta.Tag(Stmt), stmt.*));
    const expr_stmt = stmt.expression;
    const expr = expr_stmt.expression;

    // Verify this is a binary expression
    try std.testing.expectEqual(Expr.binary, @as(std.meta.Tag(Expr), expr.*));
    const binary = expr.binary;

    // Check left operand is 1
    try std.testing.expectEqual(Expr.literal, @as(std.meta.Tag(Expr), binary.left.*));
    try std.testing.expectEqual(Value{ .double = 1.0 }, binary.left.literal.value);

    // Check operator is +
    try std.testing.expectEqual(TokenType.PLUS, binary.operator.type);

    // Check right operand is 2
    try std.testing.expectEqual(Expr.literal, @as(std.meta.Tag(Expr), binary.right.*));
    try std.testing.expectEqual(Value{ .double = 2.0 }, binary.right.literal.value);
}
