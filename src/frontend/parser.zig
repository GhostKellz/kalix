const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

pub const ParseError = lexer.LexerError || error{
    UnexpectedToken,
    ExpectedIdentifier,
    ExpectedType,
    ExpectedLeftBrace,
    ExpectedRightBrace,
    ExpectedLeftParen,
    ExpectedRightParen,
    ExpectedRightBracket,
    ExpectedSemicolon,
    ExpectedColon,
    ExpectedExpression,
    DuplicateQualifier,
    InvalidIntegerLiteral,
    InvalidStringEscape,
    TrailingTokens,
    OutOfMemory,
};

pub fn parseModule(allocator: std.mem.Allocator, source: []const u8) ParseError!ast.Tree {
    const tokens = try tokenize(allocator, source);
    defer allocator.free(tokens);

    var tree = ast.Tree.init(allocator);
    errdefer tree.deinit();

    var parser = Parser{
        .allocator = allocator,
        .tokens = tokens,
        .index = 0,
        .tree = &tree,
        .source = source,
    };

    const module_ptr = try parser.parseModule();
    if (!parser.check(token.TokenKind.eof)) return error.TrailingTokens;

    tree.setModule(module_ptr);
    return tree;
}

fn tokenize(allocator: std.mem.Allocator, source: []const u8) ParseError![]token.Token {
    var lex = lexer.Lexer.init(source);
    var list = std.ArrayListUnmanaged(token.Token){};
    errdefer list.deinit(allocator);

    while (true) {
        const tok = try lex.next();
        try list.append(allocator, tok);
        if (tok.kind == .eof) break;
    }

    return try list.toOwnedSlice(allocator);
}

const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const token.Token,
    index: usize,
    tree: *ast.Tree,
    source: []const u8,

    fn parseModule(self: *Parser) ParseError!*ast.Module {
        var contracts = std.ArrayListUnmanaged(ast.Contract){};
        errdefer contracts.deinit(self.tree.allocator());

        while (!self.check(token.TokenKind.eof)) {
            if (!self.match(token.TokenKind.keyword_contract)) return error.UnexpectedToken;
            const contract = try self.parseContract();
            try contracts.append(self.tree.allocator(), contract);
        }

        const contract_slice = try contracts.toOwnedSlice(self.tree.allocator());
        const module_ptr = try self.tree.allocator().create(ast.Module);
        module_ptr.* = .{ .contracts = contract_slice };
        return module_ptr;
    }

    fn parseContract(self: *Parser) ParseError!ast.Contract {
        const name_tok = try self.consumeIdentifier();
        _ = self.expect(token.TokenKind.l_brace) catch return error.ExpectedLeftBrace;

        var items = std.ArrayListUnmanaged(ast.ContractItem){};
        errdefer items.deinit(self.tree.allocator());

        while (!self.check(token.TokenKind.r_brace) and !self.check(token.TokenKind.eof)) {
            var is_public_resource = false;
            if (self.match(token.TokenKind.keyword_pub)) {
                is_public_resource = true;
            }

            if (self.check(token.TokenKind.keyword_state)) {
                _ = self.expect(token.TokenKind.keyword_state) catch return error.UnexpectedToken;
                const state_decl = try self.parseStateDecl(is_public_resource);
                try items.append(self.tree.allocator(), .{ .state = state_decl });
                continue;
            }

            if (self.check(token.TokenKind.keyword_table)) {
                _ = self.expect(token.TokenKind.keyword_table) catch return error.UnexpectedToken;
                const table_decl = try self.parseTableDecl(is_public_resource);
                try items.append(self.tree.allocator(), .{ .table = table_decl });
                continue;
            }

            if (self.check(token.TokenKind.keyword_event)) {
                _ = self.expect(token.TokenKind.keyword_event) catch return error.UnexpectedToken;
                const event_decl = try self.parseEventDecl(is_public_resource);
                try items.append(self.tree.allocator(), .{ .event = event_decl });
                continue;
            }

            var qualifiers = ast.Qualifiers{ .is_public = false, .view = false, .payable = false };
            if (is_public_resource) {
                qualifiers.is_public = true;
            }
            try self.consumeFunctionQualifiers(&qualifiers);
            _ = self.expect(token.TokenKind.keyword_fn) catch return error.UnexpectedToken;
            const fn_decl = try self.parseFunction(qualifiers);
            try items.append(self.tree.allocator(), .{ .function = fn_decl });
        }

        _ = self.expect(token.TokenKind.r_brace) catch return error.ExpectedRightBrace;

        const owned_items = try items.toOwnedSlice(self.tree.allocator());
        return .{
            .name = name_tok.lexeme,
            .items = owned_items,
        };
    }

    fn parseStateDecl(self: *Parser, is_public: bool) ParseError!ast.StateDecl {
        const name_tok = try self.consumeIdentifier();
        _ = self.expect(token.TokenKind.colon) catch return error.ExpectedColon;
        const ty = try self.parseTypeExpr();
        _ = self.expect(token.TokenKind.semicolon) catch return error.ExpectedSemicolon;
        return .{
            .name = name_tok.lexeme,
            .ty = ty,
            .is_public = is_public,
        };
    }

    fn parseTableDecl(self: *Parser, is_public: bool) ParseError!ast.TableDecl {
        const name_tok = try self.consumeIdentifier();
        _ = self.expect(token.TokenKind.colon) catch return error.ExpectedColon;
        const ty = try self.parseTypeExpr();
        _ = self.expect(token.TokenKind.semicolon) catch return error.ExpectedSemicolon;
        return .{
            .name = name_tok.lexeme,
            .ty = ty,
            .is_public = is_public,
        };
    }

    fn parseEventDecl(self: *Parser, is_public: bool) ParseError!ast.EventDecl {
        const name_tok = try self.consumeIdentifier();
        _ = self.expect(token.TokenKind.l_paren) catch return error.ExpectedLeftParen;
        const params = try self.parseEventParamList();
        _ = self.expect(token.TokenKind.r_paren) catch return error.ExpectedRightParen;
        _ = self.expect(token.TokenKind.semicolon) catch return error.ExpectedSemicolon;
        return .{
            .name = name_tok.lexeme,
            .params = params,
            .is_public = is_public,
        };
    }

    fn parseEventParamList(self: *Parser) ParseError![]const ast.Param {
        var params = std.ArrayListUnmanaged(ast.Param){};
        errdefer params.deinit(self.tree.allocator());

        if (self.check(token.TokenKind.r_paren)) {
            return try params.toOwnedSlice(self.tree.allocator());
        }

        while (true) {
            const name_tok = try self.consumeIdentifier();
            _ = self.expect(token.TokenKind.colon) catch return error.ExpectedColon;
            const ty = try self.parseTypeExpr();
            try params.append(self.tree.allocator(), .{
                .name = name_tok.lexeme,
                .ty = ty,
                .is_mutable = false,
                .is_const = false,
            });

            if (!self.match(token.TokenKind.comma)) break;
        }

        return try params.toOwnedSlice(self.tree.allocator());
    }

    fn consumeFunctionQualifiers(self: *Parser, qualifiers: *ast.Qualifiers) ParseError!void {
        while (true) {
            if (self.match(token.TokenKind.keyword_view)) {
                if (qualifiers.view) return error.DuplicateQualifier;
                qualifiers.view = true;
                continue;
            }
            if (self.match(token.TokenKind.keyword_payable)) {
                if (qualifiers.payable) return error.DuplicateQualifier;
                qualifiers.payable = true;
                continue;
            }
            if (self.match(token.TokenKind.keyword_pub)) {
                if (qualifiers.is_public) return error.DuplicateQualifier;
                qualifiers.is_public = true;
                continue;
            }
            break;
        }
    }

    fn parseFunction(self: *Parser, qualifiers: ast.Qualifiers) ParseError!ast.FnDecl {
        const name_tok = try self.consumeIdentifier();
        _ = self.expect(token.TokenKind.l_paren) catch return error.ExpectedLeftParen;
        const params = try self.parseParamList();
        _ = self.expect(token.TokenKind.r_paren) catch return error.ExpectedRightParen;

        var return_type: ?ast.TypeExpr = null;
        if (self.match(token.TokenKind.arrow)) {
            return_type = try self.parseTypeExpr();
        }

        const body = try self.parseBlock();
        return .{
            .name = name_tok.lexeme,
            .params = params,
            .return_type = return_type,
            .qualifiers = qualifiers,
            .body = body,
        };
    }

    fn parseParamList(self: *Parser) ParseError![]const ast.Param {
        var params = std.ArrayListUnmanaged(ast.Param){};
        errdefer params.deinit(self.tree.allocator());

        if (self.check(token.TokenKind.r_paren)) {
            return try params.toOwnedSlice(self.tree.allocator());
        }

        while (true) {
            var is_mutable = false;
            var is_const = false;
            if (self.match(token.TokenKind.keyword_mut)) {
                is_mutable = true;
            } else if (self.match(token.TokenKind.keyword_const)) {
                is_const = true;
            }

            const name_tok = try self.consumeIdentifier();
            _ = self.expect(token.TokenKind.colon) catch return error.ExpectedColon;
            const ty = try self.parseTypeExpr();
            try params.append(self.tree.allocator(), .{
                .name = name_tok.lexeme,
                .ty = ty,
                .is_mutable = is_mutable,
                .is_const = is_const,
            });

            if (!self.match(token.TokenKind.comma)) break;
        }

        return try params.toOwnedSlice(self.tree.allocator());
    }

    fn parseTypeExpr(self: *Parser) ParseError!ast.TypeExpr {
        const name_tok = try self.consumeIdentifier();
        var type_expr = ast.TypeExpr{ .name = name_tok.lexeme, .generics = &[_]ast.TypeExpr{} };

        if (self.match(token.TokenKind.less)) {
            var generics = std.ArrayListUnmanaged(ast.TypeExpr){};
            errdefer generics.deinit(self.tree.allocator());

            while (true) {
                const arg = try self.parseTypeExpr();
                try generics.append(self.tree.allocator(), arg);
                if (self.match(token.TokenKind.greater)) break;
                _ = self.expect(token.TokenKind.comma) catch return error.UnexpectedToken;
            }

            type_expr.generics = try generics.toOwnedSlice(self.tree.allocator());
        }

        return type_expr;
    }

    fn parseBlock(self: *Parser) ParseError!ast.Block {
        _ = self.expect(token.TokenKind.l_brace) catch return error.ExpectedLeftBrace;

        var statements = std.ArrayListUnmanaged(ast.Statement){};
        errdefer statements.deinit(self.tree.allocator());

        while (!self.check(token.TokenKind.r_brace) and !self.check(token.TokenKind.eof)) {
            const stmt = try self.parseStatement();
            try statements.append(self.tree.allocator(), stmt);
        }

        _ = self.expect(token.TokenKind.r_brace) catch return error.ExpectedRightBrace;
        return .{ .statements = try statements.toOwnedSlice(self.tree.allocator()) };
    }

    fn parseStatement(self: *Parser) ParseError!ast.Statement {
        if (self.match(token.TokenKind.keyword_let)) {
            const stmt = try self.parseLetStatement();
            return .{ .let_binding = stmt };
        }
        if (self.match(token.TokenKind.keyword_return)) {
            const ret_stmt = try self.parseReturnStatement();
            return .{ .return_stmt = ret_stmt };
        }
        if (self.match(token.TokenKind.keyword_if)) {
            const if_stmt = try self.parseIfStatement();
            return .{ .if_stmt = if_stmt };
        }
        if (self.match(token.TokenKind.keyword_while)) {
            const while_stmt = try self.parseWhileStatement();
            return .{ .while_stmt = while_stmt };
        }
        if (self.check(token.TokenKind.l_brace)) {
            const block = try self.parseBlock();
            return .{ .block = block };
        }

        const expr = try self.parseExpression();
        _ = self.expect(token.TokenKind.semicolon) catch return error.ExpectedSemicolon;
        return .{ .expression = expr };
    }

    fn parseLetStatement(self: *Parser) ParseError!ast.LetStmt {
        var is_mutable = false;
        var is_const = false;
        if (self.match(token.TokenKind.keyword_mut)) {
            is_mutable = true;
        } else if (self.match(token.TokenKind.keyword_const)) {
            is_const = true;
        }
        const name_tok = try self.consumeIdentifier();
        _ = self.expect(token.TokenKind.equal) catch return error.ExpectedExpression;
        const rhs = try self.parseExpression();
        _ = self.expect(token.TokenKind.semicolon) catch return error.ExpectedSemicolon;
        return .{
            .name = name_tok.lexeme,
            .is_mutable = is_mutable,
            .is_const = is_const,
            .rhs = rhs,
        };
    }

    fn parseReturnStatement(self: *Parser) ParseError!ast.ReturnStmt {
        if (self.match(token.TokenKind.semicolon)) {
            return .{ .value = null };
        }
        const value_expr = try self.parseExpression();
        _ = self.expect(token.TokenKind.semicolon) catch return error.ExpectedSemicolon;
        return .{ .value = value_expr };
    }

    fn parseIfStatement(self: *Parser) ParseError!ast.IfStmt {
        const condition = try self.parseExpression();
        const then_block = try self.parseBlock();

        var else_block: ?ast.Block = null;
        if (self.match(token.TokenKind.keyword_else)) {
            if (self.check(token.TokenKind.keyword_if)) {
                _ = self.advance();
                const nested = try self.parseIfStatement();
                var chain = [_]ast.Statement{.{ .if_stmt = nested }};
                else_block = .{
                    .statements = try self.tree.createStatements(chain[0..]),
                };
            } else {
                else_block = try self.parseBlock();
            }
        }

        return .{ .condition = condition, .then_block = then_block, .else_block = else_block };
    }

    fn parseWhileStatement(self: *Parser) ParseError!ast.WhileStmt {
        const condition = try self.parseExpression();
        const body = try self.parseBlock();
        return .{ .condition = condition, .body = body };
    }

    fn parseExpression(self: *Parser) ParseError!*ast.Expr {
        return self.parseAssignment();
    }

    fn parseAssignment(self: *Parser) ParseError!*ast.Expr {
        var left = try self.parseLogicalOr();
        if (self.match(token.TokenKind.equal)) {
            const value = try self.parseAssignment();
            left = try self.tree.createExpr(.{ .assignment = .{ .target = left, .value = value } });
        }
        return left;
    }

    fn parseLogicalOr(self: *Parser) ParseError!*ast.Expr {
        var expr = try self.parseLogicalAnd();
        while (self.match(token.TokenKind.or_or)) {
            const right = try self.parseLogicalAnd();
            expr = try self.makeBinary(expr, token.TokenKind.or_or, right);
        }
        return expr;
    }

    fn parseLogicalAnd(self: *Parser) ParseError!*ast.Expr {
        var expr = try self.parseEquality();
        while (self.match(token.TokenKind.and_and)) {
            const right = try self.parseEquality();
            expr = try self.makeBinary(expr, token.TokenKind.and_and, right);
        }
        return expr;
    }

    fn parseEquality(self: *Parser) ParseError!*ast.Expr {
        var expr = try self.parseComparison();
        while (true) {
            if (self.match(token.TokenKind.equal_equal)) {
                const right = try self.parseComparison();
                expr = try self.makeBinary(expr, token.TokenKind.equal_equal, right);
            } else if (self.match(token.TokenKind.bang_equal)) {
                const right = try self.parseComparison();
                expr = try self.makeBinary(expr, token.TokenKind.bang_equal, right);
            } else {
                break;
            }
        }
        return expr;
    }

    fn parseComparison(self: *Parser) ParseError!*ast.Expr {
        var expr = try self.parseTerm();
        while (true) {
            if (self.match(token.TokenKind.less)) {
                expr = try self.makeBinary(expr, token.TokenKind.less, try self.parseTerm());
            } else if (self.match(token.TokenKind.less_equal)) {
                expr = try self.makeBinary(expr, token.TokenKind.less_equal, try self.parseTerm());
            } else if (self.match(token.TokenKind.greater)) {
                expr = try self.makeBinary(expr, token.TokenKind.greater, try self.parseTerm());
            } else if (self.match(token.TokenKind.greater_equal)) {
                expr = try self.makeBinary(expr, token.TokenKind.greater_equal, try self.parseTerm());
            } else {
                break;
            }
        }
        return expr;
    }

    fn parseTerm(self: *Parser) ParseError!*ast.Expr {
        var expr = try self.parseFactor();
        while (true) {
            if (self.match(token.TokenKind.plus)) {
                expr = try self.makeBinary(expr, token.TokenKind.plus, try self.parseFactor());
            } else if (self.match(token.TokenKind.minus)) {
                expr = try self.makeBinary(expr, token.TokenKind.minus, try self.parseFactor());
            } else {
                break;
            }
        }
        return expr;
    }

    fn parseFactor(self: *Parser) ParseError!*ast.Expr {
        var expr = try self.parseUnary();
        while (true) {
            if (self.match(token.TokenKind.star)) {
                expr = try self.makeBinary(expr, token.TokenKind.star, try self.parseUnary());
            } else if (self.match(token.TokenKind.slash)) {
                expr = try self.makeBinary(expr, token.TokenKind.slash, try self.parseUnary());
            } else if (self.match(token.TokenKind.percent)) {
                expr = try self.makeBinary(expr, token.TokenKind.percent, try self.parseUnary());
            } else {
                break;
            }
        }
        return expr;
    }

    fn parseUnary(self: *Parser) ParseError!*ast.Expr {
        if (self.match(token.TokenKind.bang)) {
            const operand = try self.parseUnary();
            return self.tree.createExpr(.{ .unary = .{ .op = token.TokenKind.bang, .operand = operand } });
        }
        if (self.match(token.TokenKind.minus)) {
            const operand = try self.parseUnary();
            return self.tree.createExpr(.{ .unary = .{ .op = token.TokenKind.minus, .operand = operand } });
        }
        return self.parseCall();
    }

    fn parseCall(self: *Parser) ParseError!*ast.Expr {
        var expr = try self.parsePrimary();

        while (true) {
            if (self.match(token.TokenKind.l_paren)) {
                var args = std.ArrayListUnmanaged(*ast.Expr){};
                errdefer args.deinit(self.tree.allocator());

                if (!self.check(token.TokenKind.r_paren)) {
                    while (true) {
                        const arg = try self.parseExpression();
                        try args.append(self.tree.allocator(), arg);
                        if (!self.match(token.TokenKind.comma)) break;
                    }
                }

                _ = self.expect(token.TokenKind.r_paren) catch return error.ExpectedRightParen;
                const owned_args = try args.toOwnedSlice(self.tree.allocator());
                expr = try self.tree.createExpr(.{ .call = .{ .callee = expr, .arguments = owned_args } });
            } else if (self.match(token.TokenKind.dot)) {
                const name_tok = try self.consumeIdentifier();
                expr = try self.tree.createExpr(.{ .member = .{ .base = expr, .name = name_tok.lexeme } });
            } else if (self.match(token.TokenKind.l_bracket)) {
                const index_expr = try self.parseExpression();
                _ = self.expect(token.TokenKind.r_bracket) catch return error.ExpectedRightBracket;
                expr = try self.tree.createExpr(.{ .index = .{ .target = expr, .index = index_expr } });
            } else {
                break;
            }
        }

        return expr;
    }

    fn parsePrimary(self: *Parser) ParseError!*ast.Expr {
        if (self.isAtEnd()) return error.ExpectedExpression;
        const tok = self.advance();
        return switch (tok.kind) {
            .identifier => self.tree.createExpr(.{ .identifier = .{ .name = tok.lexeme } }),
            .keyword_state => self.tree.createExpr(.{ .identifier = .{ .name = tok.lexeme } }),
            .integer => blk: {
                const value = try parseInteger(self.tree.allocator(), tok.lexeme);
                break :blk self.tree.createExpr(.{ .integer_literal = .{ .value = value } });
            },
            .bool_true => self.tree.createExpr(.{ .bool_literal = .{ .value = true } }),
            .bool_false => self.tree.createExpr(.{ .bool_literal = .{ .value = false } }),
            .string => blk: {
                const decoded = try decodeString(self.tree.allocator(), tok.lexeme);
                break :blk self.tree.createExpr(.{ .string_literal = .{ .value = decoded } });
            },
            .l_paren => blk: {
                const inner = try self.parseExpression();
                _ = self.expect(token.TokenKind.r_paren) catch return error.ExpectedRightParen;
                break :blk inner;
            },
            else => error.ExpectedExpression,
        };
    }

    fn makeBinary(self: *Parser, left: *ast.Expr, op: token.TokenKind, right: *ast.Expr) ParseError!*ast.Expr {
        return self.tree.createExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
    }

    fn consumeIdentifier(self: *Parser) ParseError!token.Token {
        if (self.check(token.TokenKind.identifier)) {
            return self.advance();
        }
        return error.ExpectedIdentifier;
    }

    fn expect(self: *Parser, kind: token.TokenKind) ParseError!token.Token {
        if (self.check(kind)) return self.advance();
        return error.UnexpectedToken;
    }

    fn advance(self: *Parser) token.Token {
        if (!self.isAtEnd()) self.index += 1;
        return self.tokens[self.index - 1];
    }

    fn check(self: *Parser, kind: token.TokenKind) bool {
        if (self.isAtEnd()) return kind == .eof;
        return self.peek().kind == kind;
    }

    fn match(self: *Parser, kind: token.TokenKind) bool {
        if (self.check(kind)) {
            _ = self.advance();
            return true;
        }
        return false;
    }

    fn peek(self: *Parser) token.Token {
        return self.tokens[self.index];
    }

    fn isAtEnd(self: *Parser) bool {
        return self.tokens[self.index].kind == .eof;
    }
};

fn parseInteger(allocator: std.mem.Allocator, lexeme: []const u8) ParseError!i128 {
    var buf = std.ArrayListUnmanaged(u8){};
    defer buf.deinit(allocator);
    for (lexeme) |c| {
        if (c == '_') continue;
        try buf.append(allocator, c);
    }
    const cleaned = buf.items;
    if (cleaned.len == 0) return error.InvalidIntegerLiteral;

    if (cleaned.len >= 2 and cleaned[0] == '0' and (cleaned[1] == 'x' or cleaned[1] == 'X')) {
        return std.fmt.parseInt(i128, cleaned[2..], 16) catch error.InvalidIntegerLiteral;
    }
    if (cleaned.len >= 2 and cleaned[0] == '0' and (cleaned[1] == 'b' or cleaned[1] == 'B')) {
        return std.fmt.parseInt(i128, cleaned[2..], 2) catch error.InvalidIntegerLiteral;
    }
    return std.fmt.parseInt(i128, cleaned, 10) catch error.InvalidIntegerLiteral;
}

fn decodeString(allocator: std.mem.Allocator, lexeme: []const u8) ParseError![]const u8 {
    var builder = std.ArrayListUnmanaged(u8){};
    errdefer builder.deinit(allocator);

    var i: usize = 0;
    while (i < lexeme.len) : (i += 1) {
        const c = lexeme[i];
        if (c == '\\') {
            i += 1;
            if (i >= lexeme.len) return error.InvalidStringEscape;
            const esc = lexeme[i];
            const decoded: u8 = switch (esc) {
                'n' => @as(u8, '\n'),
                'r' => @as(u8, '\r'),
                't' => @as(u8, '\t'),
                '"' => @as(u8, '"'),
                '\\' => @as(u8, '\\'),
                else => return error.InvalidStringEscape,
            };
            try builder.append(allocator, decoded);
        } else {
            try builder.append(allocator, c);
        }
    }

    return try builder.toOwnedSlice(allocator);
}

const testing = std.testing;

test "parse simple contract" {
    const source =
        "contract Treasury {\n" ++
        "    state balance: u64;\n" ++
        "    table balances: Map<Address, u64>;\n" ++
        "    event Transfer(from: Address, amount: u64);\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseModule(testing.allocator, source);
    defer tree.deinit();

    const module = tree.getModule();
    try testing.expectEqual(@as(usize, 1), module.contracts.len);
    const contract = module.contracts[0];
    try testing.expectEqualStrings("Treasury", contract.name);
    try testing.expectEqual(@as(usize, 4), contract.items.len);
}
