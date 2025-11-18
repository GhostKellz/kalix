const std = @import("std");

pub const TokenKind = enum {
    eof,
    identifier,
    integer,
    string,
    bool_true,
    bool_false,

    keyword_contract,
    keyword_state,
    keyword_table,
    keyword_event,
    keyword_fn,
    keyword_let,
    keyword_const,
    keyword_mut,
    keyword_return,
    keyword_view,
    keyword_payable,
    keyword_pub,
    keyword_if,
    keyword_else,
    keyword_while,

    l_paren,
    r_paren,
    l_brace,
    r_brace,
    l_bracket,
    r_bracket,

    comma,
    colon,
    semicolon,
    dot,
    arrow,

    plus,
    minus,
    star,
    slash,
    percent,

    bang,
    bang_equal,
    equal,
    equal_equal,
    less,
    less_equal,
    greater,
    greater_equal,
    and_and,
    or_or,
};

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    start: usize,
    end: usize,
    line: usize,
    column: usize,

    pub fn isKeyword(self: Token) bool {
        return switch (self.kind) {
            .keyword_contract, .keyword_state, .keyword_table, .keyword_event, .keyword_fn, .keyword_let, .keyword_const, .keyword_mut, .keyword_return, .keyword_view, .keyword_payable, .keyword_pub, .keyword_if, .keyword_else, .keyword_while => true,
            else => false,
        };
    }
};

const keyword_entries = .{
    .{ "contract", TokenKind.keyword_contract },
    .{ "state", TokenKind.keyword_state },
    .{ "table", TokenKind.keyword_table },
    .{ "event", TokenKind.keyword_event },
    .{ "fn", TokenKind.keyword_fn },
    .{ "let", TokenKind.keyword_let },
    .{ "const", TokenKind.keyword_const },
    .{ "mut", TokenKind.keyword_mut },
    .{ "return", TokenKind.keyword_return },
    .{ "view", TokenKind.keyword_view },
    .{ "payable", TokenKind.keyword_payable },
    .{ "pub", TokenKind.keyword_pub },
    .{ "if", TokenKind.keyword_if },
    .{ "else", TokenKind.keyword_else },
    .{ "while", TokenKind.keyword_while },
    .{ "true", TokenKind.bool_true },
    .{ "false", TokenKind.bool_false },
};

pub const KeywordMap = std.StaticStringMap(TokenKind).initComptime(keyword_entries);

pub fn classifyIdentifier(ident: []const u8) TokenKind {
    return KeywordMap.get(ident) orelse TokenKind.identifier;
}

pub fn describe(kind: TokenKind) []const u8 {
    return switch (kind) {
        .eof => "end of file",
        .identifier => "identifier",
        .integer => "integer literal",
        .string => "string literal",
        .bool_true, .bool_false => "boolean literal",
        .keyword_contract => "`contract`",
        .keyword_state => "`state`",
        .keyword_table => "`table`",
        .keyword_event => "`event`",
        .keyword_fn => "`fn`",
        .keyword_let => "`let`",
        .keyword_const => "`const`",
        .keyword_mut => "`mut`",
        .keyword_return => "`return`",
        .keyword_view => "`view`",
        .keyword_payable => "`payable`",
        .keyword_pub => "`pub`",
        .keyword_if => "`if`",
        .keyword_else => "`else`",
        .keyword_while => "`while`",
        .l_paren => "`(`",
        .r_paren => "`)`",
        .l_brace => "`{`",
        .r_brace => "`}`",
        .l_bracket => "`[`",
        .r_bracket => "`]`",
        .comma => "`,`",
        .colon => "`:`",
        .semicolon => "`;`",
        .dot => "\x60.\x60",
        .arrow => "`->`",
        .plus => "`+`",
        .minus => "`-`",
        .star => "`*`",
        .slash => "`/`",
        .percent => "`%`",
        .bang => "`!`",
        .bang_equal => "`!=`",
        .equal => "`=`",
        .equal_equal => "`==`",
        .less => "`<`",
        .less_equal => "`<=`",
        .greater => "`>`",
        .greater_equal => "`>=`",
        .and_and => "`&&`",
        .or_or => "`||`",
    };
}

pub fn isLiteral(kind: TokenKind) bool {
    return switch (kind) {
        .integer, .string, .bool_true, .bool_false => true,
        else => false,
    };
}

const testing = std.testing;

test "keyword classification" {
    try testing.expect(classifyIdentifier("contract") == .keyword_contract);
    try testing.expect(classifyIdentifier("view") == .keyword_view);
    try testing.expect(classifyIdentifier("identifier") == .identifier);
}
