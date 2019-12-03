const std = @import("std");
const isAlpha = std.ascii.isAlpha;
const isAlNum = std.ascii.isAlNum;
const isDigit = std.ascii.isDigit;
const isSpace = std.ascii.isSpace;
const eql = std.mem.eql;
const HashMap = std.AutoHashMap;

const testing = std.testing;
const expect = testing.expect;

const Tokens = @import("token.zig");
pub const Token = Tokens.Token;
pub const Tag = Tokens.Tag;

pub const Lexer = struct {
    input: []const u8,
    line: usize,
    col: usize = 0, // TODO
    file_id: usize, // Could be the hash, index, etc

    const WordPair = struct {
        word: []const u8,
        tag: Tag,
    };
    const word_map = [_]WordPair{
        .{ .word = "and", .tag = .And },
        .{ .word = "block", .tag = .Block },
        .{ .word = "break", .tag = .Break },
        .{ .word = "caseof", .tag = .CaseOf },
        .{ .word = "comptime", .tag = .Comptime },
        .{ .word = "concept", .tag = .Concept },
        .{ .word = "const", .tag = .Const },
        .{ .word = "elif", .tag = .ElIf },
        .{ .word = "else", .tag = .Else },
        .{ .word = "enum", .tag = .Enum },
        .{ .word = "field", .tag = .Field },
        .{ .word = "field", .tag = .Field },
        .{ .word = "finally", .tag = .Finally },
        .{ .word = "fn", .tag = .Fn },
        .{ .word = "for", .tag = .For },
        .{ .word = "if", .tag = .If },
        .{ .word = "let", .tag = .Let },
        .{ .word = "loop", .tag = .Loop },
        .{ .word = "null", .tag = .NullLit },
        .{ .word = "or", .tag = .Or },
        .{ .word = "purefn", .tag = .PureFn },
        .{ .word = "return", .tag = .Return },
        .{ .word = "struct", .tag = .Struct },
        .{ .word = "undef", .tag = .Undef },
        .{ .word = "var", .tag = .Var },
        .{ .word = "while", .tag = .While },
        .{ .word = "xor", .tag = .Xor },
        .{ .word = "_", .tag = .NoLoc },
    };

    inline fn curChar(self: Lexer) u8 {
        if (self.input.len == 0) return 0;
        return self.input[0];
    }

    inline fn advance(self: *Lexer) u8 {
        if (self.input.len == 0) {
            return 0;
        }
        var cur = self.curChar();
        self.input = self.input[1..];
        self.col += 1;
        return cur;
    }
    inline fn advanceBy(self: *Lexer, n: usize) []const u8 {
        if (self.input.len < n) return "No way Hose";
        var res = self.input[0..n];
        self.input = self.input[n..];
        self.col += n;
        return res;
    }

    inline fn tokenOfLen(self: *Lexer, n: usize, tag: Tag) Token {
        return Token{
            .tag = tag,
            .lexeme = self.advanceBy(n),
            .pos = .{
                .file = self.file_id,
                .line = self.line,
                .col = self.col,
            },
        };
    }

    fn atoi(str: []const u8) i128 {
        var res: i128 = 0;

        for (str) |c| {
            res *= 10; // shl by 1 in base 10
            res += @intCast(i128, c - '0');
        }

        return res;
    }

    fn nextEql(self: Lexer, str: []const u8) bool {
        if (str.len > self.input.len)
            return false;
        return eql(u8, str, self.input[0..str.len]);
    }

    pub fn scan(self: *Lexer) Token {
        var res: Token = undefined;

        var done_skipping = false;
        while (!done_skipping) {
            done_skipping = true;
            while (isSpace(self.curChar())) : (_ = self.advance()) {
                self.col += 1;
                if (self.curChar() == '\n') {
                    self.line += 1;
                    self.col = 1;
                }
            }

            if (self.nextEql("(:")) {
                _ = self.advanceBy(2);
                done_skipping = false;
                while (!self.nextEql(":)") and !self.nextEql("\n")) : (_ = self.advance()) {}
                if (self.nextEql(":)")) _ = self.advanceBy(2);
            } else if (self.nextEql("(=")) {
                _ = self.advanceBy(2);
                done_skipping = false;
                // Note: Eventually we'll want to parse doc comments as tokens. This can wait for another day, however.(:
                while (!self.nextEql("\n")) : (_ = self.advance()) {}
            }
        }

        if (self.curChar() == 0) {
            res.tag = .EOF;
            return res;
        }

        // @ begins a compiler variable.
        // underscores are perfectly usable by the user.
        if (isAlpha(self.curChar()) or self.curChar() == '@' or self.curChar() == '_') {
            var i: usize = 1;
            while (isAlNum(self.input[i]) or self.input[i] == '_') : (i += 1) {}

            res = self.tokenOfLen(i, .Symbol);

            // Handle reserved keywords
            for (Lexer.word_map) |pair| {
                if (eql(u8, res.lexeme, pair.word))
                    res.tag = pair.tag;
            }
        } else if (isDigit(self.curChar())) {
            var i: usize = 0;

            // TODO: FloatLit
            while (isDigit(self.input[i])) : (i += 1) {}

            res = self.tokenOfLen(i, .IntLit);
        } else { // true operators
            switch (self.curChar()) {
                '+' => {

                    // NOTE: We must compare in descending length
                    // Otherwise nextEql("++") is also true for input = "+++"
                    // Note: This relies on being able to have arbitrary lookahead.
                    if (self.nextEql("+++")) {
                        res = self.tokenOfLen(3, .IncNow);
                    } else if (self.nextEql("++")) {
                        res = self.tokenOfLen(2, .Inc);
                    } else if (self.nextEql("+=")) {
                        res = self.tokenOfLen(2, .AddAssign);
                    } else {
                        res = self.tokenOfLen(1, .Add);
                    }
                },
                '-' => {
                    if (self.nextEql("---")) {
                        res = self.tokenOfLen(3, .DecNow);
                    } else if (self.nextEql("--")) {
                        res = self.tokenOfLen(2, .Dec);
                    } else if (self.nextEql("-=")) {
                        res = self.tokenOfLen(2, .SubAssign);
                    } else {
                        res = self.tokenOfLen(1, .Sub);
                    }
                },
                '*' => {
                    if (self.nextEql("*=")) {
                        res = self.tokenOfLen(2, .MulAssign);
                    } else {
                        res = self.tokenOfLen(1, .Mul);
                    }
                },
                '/' => {
                    if (self.nextEql("/=")) {
                        res = self.tokenOfLen(2, .DivAssign);
                    } else {
                        res = self.tokenOfLen(1, .Div);
                    }
                },
                '<' => {
                    if (self.nextEql("<<")) {
                        res = self.tokenOfLen(2, .Shl);
                    } else if (self.nextEql("<=")) {
                        res = self.tokenOfLen(2, .LessEq);
                    } else {
                        res = self.tokenOfLen(1, .Greater);
                    }
                },
                '>' => {
                    if (self.nextEql(">>")) {
                        res = self.tokenOfLen(2, .Shr);
                    } else if (self.nextEql(">=")) {
                        res = self.tokenOfLen(2, .GreaterEq);
                    } else {
                        res = self.tokenOfLen(1, .Greater);
                    }
                },
                '!' => {
                    if (self.nextEql("!=")) {
                        res = self.tokenOfLen(2, .NotEqual);
                    } else {
                        res = self.tokenOfLen(1, .Not);
                    }
                },
                '=' => {
                    if (self.nextEql("===")) {
                        res = self.tokenOfLen(3, .Assert);
                    } else if (self.nextEql("==")) {
                        res = self.tokenOfLen(2, .Equal);
                    } else {
                        res = self.tokenOfLen(1, .Assign);
                    }
                },
                '?' => res = self.tokenOfLen(1, .Optional),
                '.' => res = self.tokenOfLen(1, .Dot),
                ',' => res = self.tokenOfLen(1, .Comma),
                ':' => res = self.tokenOfLen(1, .Colon),
                ';' => res = self.tokenOfLen(1, .Semicolon),
                '|' => res = self.tokenOfLen(1, .BitOr),
                // We took care of comments with skipping whitespace
                '(' => res = self.tokenOfLen(1, .LParen),
                ')' => res = self.tokenOfLen(1, .RParen),
                '[' => res = self.tokenOfLen(1, .LBracket),
                ']' => res = self.tokenOfLen(1, .RBracket),
                '{' => res = self.tokenOfLen(1, .LBrace),
                '}' => res = self.tokenOfLen(1, .RBrace),
                '%' => res = self.tokenOfLen(1, .Mod),
                '~' => res = self.tokenOfLen(1, .BitNot),
                '&' => res = self.tokenOfLen(1, .BitAnd),

                // TODO: Escape codes
                '"' => {
                    var i: usize = 1; // 1 to explicitly skip the existing "
                    while (self.input[i] != '"') : (i += 1) {}
                    i += 1; // To include the '"'

                    res = self.tokenOfLen(i, .StringLit);
                },

                // TODO: This doesn't currently handle escaping properly (like '\'')
                '\'' => {
                    var i: usize = 1; // 1 to explicitly skip the existing "
                    while (self.input[i] != '\'') : (i += 1) {}
                    i += 1; // To include the '"'

                    res = self.tokenOfLen(i, .CharLit);
                },
                '`' => {
                    var i: usize = 1;
                    while (isAlNum(self.input[i])) : (i += 1) {}
                    res = self.tokenOfLen(i, .Label);
                },

                else => {
                    std.debug.warn("Invalid character in lexer: {c}\n", self.curChar());
                },
            }
        }
        return res;
    }
};

test "lexer" {
    var input =
        \\ (: This is a comment that shouldn't be seen
        \\ let i: u32 = 0;
        \\ i = 1 + 4 * (: mul by 5 to test inline comments :) 5;
        \\ let a: ?i32 = 10;
        \\ var count: usize = 0;
        \\ while a : |count, val| {
        \\   print("a is {}\n", val);
        \\   a -= 1;
        \\   if a == 0 {
        \\    a = null;
        \\   }
        \\ }
        \\ print("we looped {} times!", count);
    ;

    var lexer = Lexer{
        .input = input,
        .line = 0,
        .file_id = 0,
    };

    var tags = [_]Tag{
        .Let,       .Symbol,    .Colon,     .Symbol,    .Assign,    .IntLit,    .Semicolon,
        .Symbol,    .Assign,    .IntLit,    .Add,       .IntLit,    .Mul,       .IntLit,
        .Semicolon, .Let,       .Symbol,    .Colon,     .Optional,  .Symbol,    .Assign,
        .IntLit,    .Semicolon, .Var,       .Symbol,    .Colon,     .Symbol,    .Assign,
        .IntLit,    .Semicolon, .While,     .Symbol,    .Colon,     .BitOr,     .Symbol,
        .Comma,     .Symbol,    .BitOr,     .LBrace,    .Symbol,    .LParen,    .StringLit,
        .Comma,     .Symbol,    .RParen,    .Semicolon, .Symbol,    .SubAssign, .IntLit,
        .Semicolon, .If,        .Symbol,    .Equal,     .IntLit,    .LBrace,    .Symbol,
        .Assign,    .NullLit,   .Semicolon, .RBrace,    .RBrace,    .Symbol,    .LParen,
        .StringLit, .Comma,     .Symbol,    .RParen,    .Semicolon,
    };

    //std.debug.warn("\n");

    var prevLine = lexer.line;
    for (tags) |tag| {
        var cur = lexer.scan();
        if (lexer.line != prevLine) {
            //std.debug.warn("\n");
            prevLine = lexer.line;
        }
        //std.debug.warn("{} ", cur.tag);
        expect(tag == cur.tag);
    }
}
