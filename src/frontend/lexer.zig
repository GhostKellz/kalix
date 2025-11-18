const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const TokenKind = token.TokenKind;

pub const Lexer = struct {
    source: []const u8,
    index: usize,
    line: usize,
    column: usize,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .index = 0,
            .line = 1,
            .column = 1,
        };
    }

    pub fn next(self: *Lexer) LexerError!Token {
        try self.skipTrivia();
        const start_index = self.index;
        const start_line = self.line;
        const start_column = self.column;

        const c = self.peek() orelse {
            return Token{
                .kind = .eof,
                .lexeme = "",
                .start = self.index,
                .end = self.index,
                .line = self.line,
                .column = self.column,
            };
        };

        if (isIdentifierStart(c)) {
            self.advance();
            self.scanIdentifierTail();
            const lexeme = self.source[start_index..self.index];
            const kind = token.classifyIdentifier(lexeme);
            return Token{
                .kind = kind,
                .lexeme = lexeme,
                .start = start_index,
                .end = self.index,
                .line = start_line,
                .column = start_column,
            };
        }

        if (std.ascii.isDigit(c)) {
            self.advance();
            try self.scanNumberTail(c);
            return Token{
                .kind = .integer,
                .lexeme = self.source[start_index..self.index],
                .start = start_index,
                .end = self.index,
                .line = start_line,
                .column = start_column,
            };
        }

        switch (c) {
            '"' => {
                try self.scanStringLiteral();
                return Token{
                    .kind = .string,
                    .lexeme = self.source[start_index + 1 .. self.index - 1],
                    .start = start_index,
                    .end = self.index,
                    .line = start_line,
                    .column = start_column,
                };
            },
            '(' => return self.simpleToken(.l_paren, start_index, start_line, start_column),
            ')' => return self.simpleToken(.r_paren, start_index, start_line, start_column),
            '{' => return self.simpleToken(.l_brace, start_index, start_line, start_column),
            '}' => return self.simpleToken(.r_brace, start_index, start_line, start_column),
            '[' => return self.simpleToken(.l_bracket, start_index, start_line, start_column),
            ']' => return self.simpleToken(.r_bracket, start_index, start_line, start_column),
            '<' => {
                self.advance();
                var kind: TokenKind = .less;
                if (self.peek()) |next_char| {
                    if (next_char == '=') {
                        self.advance();
                        kind = .less_equal;
                    }
                }
                return Token{
                    .kind = kind,
                    .lexeme = self.source[start_index..self.index],
                    .start = start_index,
                    .end = self.index,
                    .line = start_line,
                    .column = start_column,
                };
            },
            '>' => {
                self.advance();
                var kind: TokenKind = .greater;
                if (self.peek()) |next_char| {
                    if (next_char == '=') {
                        self.advance();
                        kind = .greater_equal;
                    }
                }
                return Token{
                    .kind = kind,
                    .lexeme = self.source[start_index..self.index],
                    .start = start_index,
                    .end = self.index,
                    .line = start_line,
                    .column = start_column,
                };
            },
            '+' => return self.simpleToken(.plus, start_index, start_line, start_column),
            '-' => {
                self.advance();
                if (self.peek()) |next_char| {
                    if (next_char == '>') {
                        self.advance();
                        return Token{
                            .kind = .arrow,
                            .lexeme = self.source[start_index..self.index],
                            .start = start_index,
                            .end = self.index,
                            .line = start_line,
                            .column = start_column,
                        };
                    }
                }
                return Token{
                    .kind = .minus,
                    .lexeme = self.source[start_index..self.index],
                    .start = start_index,
                    .end = self.index,
                    .line = start_line,
                    .column = start_column,
                };
            },
            '*' => return self.simpleToken(.star, start_index, start_line, start_column),
            '/' => return self.simpleToken(.slash, start_index, start_line, start_column),
            '%' => return self.simpleToken(.percent, start_index, start_line, start_column),
            ':' => return self.simpleToken(.colon, start_index, start_line, start_column),
            ';' => return self.simpleToken(.semicolon, start_index, start_line, start_column),
            ',' => return self.simpleToken(.comma, start_index, start_line, start_column),
            '.' => return self.simpleToken(.dot, start_index, start_line, start_column),
            '=' => {
                self.advance();
                var kind: TokenKind = .equal;
                if (self.peek()) |next_char| {
                    if (next_char == '=') {
                        self.advance();
                        kind = .equal_equal;
                    }
                }
                return Token{
                    .kind = kind,
                    .lexeme = self.source[start_index..self.index],
                    .start = start_index,
                    .end = self.index,
                    .line = start_line,
                    .column = start_column,
                };
            },
            '!' => {
                self.advance();
                if (self.peek()) |next_char| {
                    if (next_char == '=') {
                        self.advance();
                        return Token{
                            .kind = .bang_equal,
                            .lexeme = self.source[start_index..self.index],
                            .start = start_index,
                            .end = self.index,
                            .line = start_line,
                            .column = start_column,
                        };
                    }
                }
                return Token{
                    .kind = .bang,
                    .lexeme = self.source[start_index..self.index],
                    .start = start_index,
                    .end = self.index,
                    .line = start_line,
                    .column = start_column,
                };
            },
            '&' => {
                self.advance();
                if (self.peek()) |next_char| {
                    if (next_char == '&') {
                        self.advance();
                        return Token{
                            .kind = .and_and,
                            .lexeme = self.source[start_index..self.index],
                            .start = start_index,
                            .end = self.index,
                            .line = start_line,
                            .column = start_column,
                        };
                    }
                }
                return error.InvalidCharacter;
            },
            '|' => {
                self.advance();
                if (self.peek()) |next_char| {
                    if (next_char == '|') {
                        self.advance();
                        return Token{
                            .kind = .or_or,
                            .lexeme = self.source[start_index..self.index],
                            .start = start_index,
                            .end = self.index,
                            .line = start_line,
                            .column = start_column,
                        };
                    }
                }
                return error.InvalidCharacter;
            },
            else => return error.InvalidCharacter,
        }
    }

    fn simpleToken(self: *Lexer, kind: TokenKind, start: usize, line: usize, column: usize) Token {
        self.advance();
        return Token{
            .kind = kind,
            .lexeme = self.source[start..self.index],
            .start = start,
            .end = self.index,
            .line = line,
            .column = column,
        };
    }

    fn scanIdentifierTail(self: *Lexer) void {
        while (self.peek()) |c| {
            if (!isIdentifierPart(c)) break;
            self.advance();
        }
    }

    fn scanNumberTail(self: *Lexer, first: u8) LexerError!void {
        if (first == '0') {
            if (self.peek()) |c| {
                if (c == 'x' or c == 'X') {
                    self.advance();
                    try self.consumeDigits(isHexDigit, true);
                    return;
                } else if (c == 'b' or c == 'B') {
                    self.advance();
                    try self.consumeDigits(isBinaryDigit, true);
                    return;
                }
            }
        }
        try self.consumeDigits(isDecimalDigit, false);
    }

    fn consumeDigits(self: *Lexer, predicate: fn (u8) bool, require_digit: bool) LexerError!void {
        var saw_digit = !require_digit;
        while (self.peek()) |c| {
            if (c == '_') {
                if (!saw_digit) return error.InvalidNumber;
                self.advance();
                saw_digit = false;
                continue;
            }
            if (!predicate(c)) break;
            saw_digit = true;
            self.advance();
        }
        if (!saw_digit) {
            return error.InvalidNumber;
        }
    }

    fn scanStringLiteral(self: *Lexer) LexerError!void {
        self.advance(); // skip opening quote
        while (self.peek()) |c| {
            switch (c) {
                '\\' => {
                    self.advance();
                    const esc = self.peek() orelse return error.UnterminatedString;
                    if (!isValidEscape(esc)) return error.InvalidEscape;
                    self.advance();
                },
                '"' => {
                    self.advance();
                    return;
                },
                '\n' => return error.UnterminatedString,
                else => self.advance(),
            }
        }
        return error.UnterminatedString;
    }

    fn skipTrivia(self: *Lexer) LexerError!void {
        while (self.peek()) |c| {
            switch (c) {
                ' ', '\t', '\r' => {
                    self.advance();
                },
                '\n' => {
                    self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        self.advance();
                        self.advance();
                        while (self.peek()) |line_c| {
                            if (line_c == '\n') break;
                            self.advance();
                        }
                    } else if (self.peekNext() == '*') {
                        self.advance();
                        self.advance();
                        try self.skipBlockComment();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn skipBlockComment(self: *Lexer) LexerError!void {
        var depth: usize = 1;
        while (self.peek()) |c| {
            self.advance();
            switch (c) {
                '/' => if (self.peek()) |ahead| {
                    if (ahead == '*') {
                        self.advance();
                        depth += 1;
                    }
                },
                '*' => if (self.peek()) |ahead| {
                    if (ahead == '/') {
                        self.advance();
                        depth -= 1;
                        if (depth == 0) return;
                    }
                },
                '\n' => {},
                else => {},
            }
        }
        return error.UnterminatedBlockComment;
    }

    fn advance(self: *Lexer) void {
        if (self.index >= self.source.len) return;
        const c = self.source[self.index];
        self.index += 1;
        if (c == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }

    fn peek(self: *Lexer) ?u8 {
        if (self.index >= self.source.len) return null;
        return self.source[self.index];
    }

    fn peekNext(self: *Lexer) ?u8 {
        const next_index = self.index + 1;
        if (next_index >= self.source.len) return null;
        return self.source[next_index];
    }
};

fn isIdentifierStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

fn isIdentifierPart(c: u8) bool {
    return isIdentifierStart(c) or std.ascii.isDigit(c);
}

fn isHexDigit(c: u8) bool {
    return std.ascii.isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn isBinaryDigit(c: u8) bool {
    return c == '0' or c == '1';
}

fn isDecimalDigit(c: u8) bool {
    return std.ascii.isDigit(c);
}

fn isValidEscape(c: u8) bool {
    return switch (c) {
        '"', '\\', 'n', 'r', 't' => true,
        else => false,
    };
}

pub const LexerError = error{
    InvalidCharacter,
    UnterminatedString,
    UnterminatedBlockComment,
    InvalidEscape,
    InvalidNumber,
};

const testing = std.testing;

test "lex simple contract" {
    var lexer = Lexer.init(
        "contract Test {\n" ++
            "    state balance: u64;\n" ++
            "    fn deposit(amount: u64) {\n" ++
            "        return;\n" ++
            "    }\n" ++
            "}\n",
    );

    const expected_kinds = [_]TokenKind{
        .keyword_contract,
        .identifier,
        .l_brace,
        .keyword_state,
        .identifier,
        .colon,
        .identifier,
        .semicolon,
        .keyword_fn,
        .identifier,
        .l_paren,
        .identifier,
        .colon,
        .identifier,
        .r_paren,
        .l_brace,
        .keyword_return,
        .semicolon,
        .r_brace,
        .r_brace,
        .eof,
    };

    for (expected_kinds) |expected| {
        const tok = try lexer.next();
        try testing.expectEqual(expected, tok.kind);
    }
}
