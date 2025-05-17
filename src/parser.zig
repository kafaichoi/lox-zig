const std = @import("std");
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Expr = @import("./expr.zig").Expr;
const Value = @import("./expr.zig").Value;
const Stmt = @import("./ast.zig").Stmt;
const Declaration = @import("./ast.zig").Declaration;
const VarDecl = @import("./ast.zig").VarDecl;
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

    pub fn parse(self: *Parser) ParserError![]*Declaration {
        var declarations = std.ArrayList(*Declaration).init(self.allocator);

        while (!self.isAtEnd()) {
            const stmt = try self.declaration();
            try declarations.append(stmt);
        }

        return declarations.toOwnedSlice();
    }

    fn declaration(self: *Parser) ParserError!*Declaration {
        if (self.match(&.{.VAR})) {
            return self.var_declaration();
        }

        const stmt = try self.statement();
        const decl = try self.allocator.create(Declaration);
        decl.* = Declaration{ .stmt = stmt };
        return decl;
    }

    fn var_declaration(self: *Parser) ParserError!*Declaration {
        const name = try self.consume(.IDENTIFIER, "Expect variable name.");

        var initializer: ?*Expr = null;
        if (self.match(&.{.EQUAL})) {
            initializer = try self.expression();
        }

        _ = try self.consume(.SEMICOLON, "Expect ';' after variable declaration.");
        return try VarDecl.create(self.allocator, name.lexeme, initializer);
    }

    fn statement(self: *Parser) ParserError!*Stmt {
        if (self.match(&.{.IF})) {
            return self.if_statement();
        }
        if (self.match(&.{.PRINT})) {
            return self.print_statement();
        }

        if (self.match(&.{.WHILE})) {
            return self.match_statement();
        }

        if (self.match(&.{.LEFT_BRACE})) {
            return self.block_statement();
        }
        return self.expression_statement();
    }

    fn if_statement(self: *Parser) ParserError!*Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
        const condition = try self.expression();
        _ = try self.consume(.RIGHT_PARENS, "Expect ')' after if condition.");
        const then_branch = try self.statement();
        var else_branch: ?*Stmt = null;
        if (self.match(&.{.ELSE})) {
            else_branch = try self.statement();
        }
        return try Stmt.IfStmt.create(self.allocator, condition, then_branch, else_branch);
    }

    fn print_statement(self: *Parser) ParserError!*Stmt {
        const value = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after value.");
        return try Stmt.PrintStmt.create(self.allocator, value);
    }

    fn match_statement(self: *Parser) ParserError!*Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
        const condition = try self.expression();
        _ = try self.consume(.RIGHT_PARENS, "Expect ')' after if condition.");
        const body = try self.statement();
        return try Stmt.WhileStmt.create(self.allocator, condition, body);
    }

    fn expression_statement(self: *Parser) ParserError!*Stmt {
        const expr = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after expression.");
        return try Stmt.ExpressionStmt.create(self.allocator, expr);
    }

    fn block_statement(self: *Parser) ParserError!*Stmt {
        var declarations = std.ArrayList(*Declaration).init(self.allocator);
        defer declarations.deinit();

        while (!self.check(.RIGHT_BRACE) and !self.isAtEnd()) {
            const decl = try self.declaration();
            try declarations.append(decl);
        }
        _ = try self.consume(.RIGHT_BRACE, "Expect '}' after block.");
        return try Stmt.BlockStmt.create(self.allocator, try declarations.toOwnedSlice());
    }

    fn expression(self: *Parser) ParserError!*Expr {
        return try self.assignment();
    }

    fn assignment(self: *Parser) ParserError!*Expr {
        const expr = try self.or_expr();

        if (self.match(&.{.EQUAL})) {
            const value = try self.assignment();

            if (expr.* == .variable) {
                const name = expr.variable.name;
                return try Expr.AssignExpr.create(self.allocator, name, value);
            }

            return self.errorExpr("Invalid assignment target.");
        }

        return expr;
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
        if (self.match(&.{.FALSE})) return try Expr.LiteralExpr.create(self.allocator, Value.init(.{ .boolean = false }, null));
        if (self.match(&.{.TRUE})) return try Expr.LiteralExpr.create(self.allocator, Value.init(.{ .boolean = true }, null));
        if (self.match(&.{.NIL})) return try Expr.LiteralExpr.create(self.allocator, Value.init(.{ .nil = {} }, null));

        if (self.match(&.{ .NUMBER, .STRING })) {
            const token_literal = self.previous().literal;
            // Convert from TokenLiteral to Value
            const value: Value = switch (token_literal) {
                .double => |d| Value.init(.{ .double = d }, null),
                .string => |s| Value.init_borrowed(.{ .string = s }),
                else => Value.init(.{ .nil = {} }, null),
            };
            return try Expr.LiteralExpr.create(self.allocator, value);
        }

        if (self.match(&.{.IDENTIFIER})) {
            return try Expr.VariableExpr.create(self.allocator, self.previous());
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

    fn or_expr(self: *Parser) ParserError!*Expr {
        var expr = try self.and_expr();

        while (self.match(&.{.OR})) {
            const operator = self.previous();
            const right = try self.and_expr();
            expr = try Expr.LogicalExpr.create(self.allocator, expr, operator, right);
        }

        return expr;
    }

    fn and_expr(self: *Parser) ParserError!*Expr {
        var expr = try self.equality();

        while (self.match(&.{.AND})) {
            const operator = self.previous();
            const right = try self.equality();
            expr = try Expr.LogicalExpr.create(self.allocator, expr, operator, right);
        }

        return expr;
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
    const declarations = try parser.parse();
    defer {
        for (declarations) |decl| {
            decl.deinit(std.testing.allocator);
        }
        std.testing.allocator.free(declarations);
    }

    // Verify we have exactly one declaration
    try std.testing.expectEqual(@as(usize, 1), declarations.len);

    // Verify this is a statement declaration
    const decl = declarations[0];
    try std.testing.expectEqual(Declaration.stmt, @as(std.meta.Tag(Declaration), decl.*));
    const stmt = decl.stmt;

    // Verify this is an expression statement
    try std.testing.expectEqual(Stmt.expression, @as(std.meta.Tag(Stmt), stmt.*));
    const expr_stmt = stmt.expression;
    const expr = expr_stmt.expression;

    // Verify this is a binary expression
    try std.testing.expectEqual(Expr.binary, @as(std.meta.Tag(Expr), expr.*));
    const binary = expr.binary;

    // Check left operand is 1
    try std.testing.expectEqual(Expr.literal, @as(std.meta.Tag(Expr), binary.left.*));
    try std.testing.expectEqual(Value.init(.{ .double = 1.0 }, null), binary.left.literal.value);

    // Check operator is +
    try std.testing.expectEqual(TokenType.PLUS, binary.operator.type);

    // Check right operand is 2
    try std.testing.expectEqual(Expr.literal, @as(std.meta.Tag(Expr), binary.right.*));
    try std.testing.expectEqual(Value.init(.{ .double = 2.0 }, null), binary.right.literal.value);
}

test "variable declaration test" {
    const tokens = [_]Token{
        .{ .type = .VAR, .lexeme = "var", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .IDENTIFIER, .lexeme = "x", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .EQUAL, .lexeme = "=", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .NUMBER, .lexeme = "42", .literal = .{ .double = 42.0 }, .line = 1 },
        .{ .type = .SEMICOLON, .lexeme = ";", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .EOF, .lexeme = "", .literal = .{ .none = {} }, .line = 1 },
    };
    var parser = Parser.init(&tokens, std.testing.allocator);
    const declarations = try parser.parse();
    defer {
        for (declarations) |decl| {
            decl.deinit(std.testing.allocator);
        }
        std.testing.allocator.free(declarations);
    }

    // Verify we have exactly one declaration
    try std.testing.expectEqual(@as(usize, 1), declarations.len);

    // Verify this is a variable declaration
    const decl = declarations[0];
    try std.testing.expectEqual(Declaration.var_decl, @as(std.meta.Tag(Declaration), decl.*));
    const var_decl = decl.var_decl;

    // Verify variable name
    try std.testing.expectEqualStrings("x", var_decl.name);

    // Verify initializer
    try std.testing.expect(var_decl.initializer != null);
    const init = var_decl.initializer.?;
    try std.testing.expectEqual(Expr.literal, @as(std.meta.Tag(Expr), init.*));
    try std.testing.expectEqual(Value.init(.{ .double = 42.0 }, null), init.literal.value);
}

test "multiple declarations test" {
    const tokens = [_]Token{
        // var x = 1;
        .{ .type = .VAR, .lexeme = "var", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .IDENTIFIER, .lexeme = "x", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .EQUAL, .lexeme = "=", .literal = .{ .none = {} }, .line = 1 },
        .{ .type = .NUMBER, .lexeme = "1", .literal = .{ .double = 1.0 }, .line = 1 },
        .{ .type = .SEMICOLON, .lexeme = ";", .literal = .{ .none = {} }, .line = 1 },
        // print x + 2;
        .{ .type = .PRINT, .lexeme = "print", .literal = .{ .none = {} }, .line = 2 },
        .{ .type = .IDENTIFIER, .lexeme = "x", .literal = .{ .none = {} }, .line = 2 },
        .{ .type = .PLUS, .lexeme = "+", .literal = .{ .none = {} }, .line = 2 },
        .{ .type = .NUMBER, .lexeme = "2", .literal = .{ .double = 2.0 }, .line = 2 },
        .{ .type = .SEMICOLON, .lexeme = ";", .literal = .{ .none = {} }, .line = 2 },
        .{ .type = .EOF, .lexeme = "", .literal = .{ .none = {} }, .line = 2 },
    };
    var parser = Parser.init(&tokens, std.testing.allocator);
    const declarations = try parser.parse();
    defer {
        for (declarations) |decl| {
            decl.deinit(std.testing.allocator);
        }
        std.testing.allocator.free(declarations);
    }

    // Verify we have exactly two declarations
    try std.testing.expectEqual(@as(usize, 2), declarations.len);

    // First declaration should be a variable declaration
    try std.testing.expectEqual(Declaration.var_decl, @as(std.meta.Tag(Declaration), declarations[0].*));
    const var_decl = declarations[0].var_decl;
    try std.testing.expectEqualStrings("x", var_decl.name);
    try std.testing.expectEqual(Value.init(.{ .double = 1.0 }, null), var_decl.initializer.?.literal.value);

    // Second declaration should be a print statement
    try std.testing.expectEqual(Declaration.stmt, @as(std.meta.Tag(Declaration), declarations[1].*));
    const stmt = declarations[1].stmt;
    try std.testing.expectEqual(Stmt.print, @as(std.meta.Tag(Stmt), stmt.*));
    const print_stmt = stmt.print;
    const expr = print_stmt.expression;
    try std.testing.expectEqual(Expr.binary, @as(std.meta.Tag(Expr), expr.*));
    const binary = expr.binary;
    try std.testing.expectEqual(TokenType.PLUS, binary.operator.type);
}
