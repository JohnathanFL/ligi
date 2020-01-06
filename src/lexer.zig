const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const printf = std.debug.warn;
const assert = std.debug.assert;

pub const tokens = @import("token.zig");
const FilePos = tokens.FilePos;
const Tag = tokens.Tag;
const Token = tokens.Token;

pub const Lexer = struct {
    input: []const u8,
    pos: FilePos,
    strs: ArenaAllocator,
    inline fn cur(self: Lexer) u8 {
        if (self.input.len == 0) {
            return 0;
        } else {
            return self.input[0];
        }
    }

    pub fn init(alloc: *Allocator, file_id: usize, input: []const u8) Lexer {
        return Lexer{
            .input = input,
            .strs = ArenaAllocator.init(alloc),
            .pos = .{
                .file_id = file_id,
                .line = 1,
                .col = 1,
            },
        };
    }

    /// Returns the old current char, using \0 for EOF
    fn advance(self: *Lexer) u8 {
        if (self.input.len == 0) {
            return 0;
        } else {
            const old = self.cur();
            self.input = self.input[1..];
            self.pos.col += 1;
            if (old == '\n') {
                self.pos.line += 1;
                self.pos.col = 1;
                printf("\n", .{});
            }
            return old;
        }
    }

    fn advanceBy(self: *Lexer, num: usize) []const u8 {
        if (self.input.len < num) {
            return "";
        } else {
            var res = self.input[0..num];
            var i = num;
            while (i > 0) : (i -= 1) _ = self.advance();
            return res;
        }
    }

    fn nextEql(self: *Lexer, str: []const u8) bool {
        //printf("\n\tChecking if next is {}...", .{str});
        // Restricting to 3 char lookahead.
        if (str.len > 3) return false;
        if (self.input.len == 0) return false;
        if (self.input.len < str.len) return false;

        return std.mem.eql(u8, str, self.input[0..str.len]);
    }

    // Skip whitespace/comments
    fn skipUnparsed(self: *Lexer) void {
        var done = false;
        while (!done) {
            done = true;
            // Tabs are not supported.
            while (self.cur() == ' ' or self.cur() == '\n') {
                _ = self.advance();
            }
            if (self.nextEql("(:")) {
                done = false;
                while (self.advance() != '\n') {}
            }
        }
    }

    pub fn scan(self: *Lexer) ?Token {
        self.skipUnparsed();
        if (self.cur() == 0) return null;

        switch (self.cur()) {
            '@', 'a'...'z', 'A'...'Z', '_' => return self.parseWord(),
            '0'...'9' => return self.parseNumLit(),
            '`' => return self.parseLabel(),
            '"' => return self.parseStrLit(),
            '\\' => return self.parseLineStrLit(),
            '\'' => return self.parseCharLit(),
            '#' => return self.parseStropping(),
            else => return self.parseOperator(),
        }
    }

    fn parseStrLit(self: *Lexer) Token {
        const pos = self.pos;
        var i: usize = 1;
        while (self.input[i] != '"') {
            if (self.input[i] == '\\') i += 1; // Skip the escaper and its target
            i += 1;
        }
        i += 1; // Include the "

        var lexeme = self.advanceBy(i);
        lexeme = lexeme[1 .. lexeme.len - 1]; // Remove the "
        return .{
            .pos = pos,
            .tag = .{ .StringLit = lexeme },
        };
    }

    fn parseLineStrLit(self: *Lexer) Token {
        // Must either be \\<newline> or \\<space>
        const starter_no_space = "\\\\\n";
        const starter = "\\\\ ";
        assert(starter.len == 3);
        const pos = self.pos;
        assert(self.nextEql("\\\\")); // Note we match the space afterwards
        var holder = std.ArrayList(u8).init(&self.strs.allocator);

        var first = true;
        while (true) {
            self.skipUnparsed();
            if (!self.nextEql(starter)) {
                if (self.nextEql(starter_no_space)) {
                    holder.append('\n') catch unreachable;
                    _ = self.advanceBy(starter_no_space.len);
                } else {
                    break;
                }
            } else {
                _ = self.advanceBy(starter.len); // Skip the '\\ '

                // Only a newline after a \\ lit iff there's another lit after it.
                if (!first) holder.append('\n') catch unreachable;
                first = false;

                while (self.cur() != '\n') holder.append(self.advance()) catch unreachable;
            }
        }

        return Token{
            .pos = pos,
            .tag = .{ .StringLit = holder.toOwnedSlice() },
        };
    }

    /// Parses a word which, ignoring the first character, follows identifier rules
    fn parseIdent(self: *Lexer) []const u8 {
        // We already know at least the current character is fine
        var i: usize = 1;
        while ((self.input.len >= i + 1) and
            (self.input[i] <= 'Z' and self.input[i] >= 'A') // A-Z
            or (self.input[i] <= 'z' and self.input[i] >= 'a') // a-z
            or (self.input[i] <= '9' and self.input[i] >= '0') // 0-9
            or (self.input[i] == '@' or self.input[i] == '_')) // @ _
        {
            //printf("\n\ti={}", .{i});
            i += 1;
        }

        return self.advanceBy(i); // Don't include whatever wasn't part of the ident
    }

    fn parseWord(self: *Lexer) Token {
        //printf("\n\tParsing word!", .{});
        const pos = self.pos;
        var lexeme = self.parseIdent();
        // TODO: Recognize keywords
        for (@This().keywords) |pair| {
            if (std.mem.eql(u8, lexeme, pair.str))
                return Token {
                    .pos = pos,
                    .tag = pair.tag,
                };
        }
        return Token{
            .pos = pos,
            .tag = .{ .Symbol = lexeme },
        };
    }

    fn parseNumLit(self: *Lexer) Token {
        const pos = self.pos;
        var whole: usize = 0;
        while (self.cur() <= '9' and self.cur() >= '0') {
            whole *= 10; // Shl by 1 in base 10
            whole += self.cur() - '0'; // '1' - '0' === 1. Set the new ones place
            _ = self.advance();
        }

        if (self.cur() == '.') {
            _ = self.advance();
            var num = @intToFloat(f64, whole);
            var shifter: usize = 10;
            while (self.cur() <= '9' and self.cur() >= '0') {
                num += @intToFloat(f64, shifter) * @intToFloat(f64, self.cur() - '0');
                _ = self.advance();
            }

            return Token{
                .pos = self.pos,
                .tag = .{ .FloatLit = num },
            };
        } else {
            return Token{
                .pos = pos,
                .tag = .{ .IntLit = whole },
            };
        }
    }

    fn parseLabel(self: *Lexer) Token {
        const pos = self.pos;
        var lexeme = self.parseIdent();
        return Token {
          .pos = pos,
          .tag = .{ .Label = lexeme },
        };
    }

    fn parseOperator(self: *Lexer) Token {
        const pos = self.pos;
        for (@This().operators) |op_pair| {
            if (self.nextEql(op_pair.str)) {
                _ = self.advanceBy(op_pair.str.len);
                return Token{
                    .pos = pos,
                    .tag = op_pair.tag,
                };
            }
        }

        printf("\n\tUnknown op at {}", .{self.pos});
        @panic("\n\tUNKNOWN OP");
    }

    fn parseCharLit(self: *Lexer) Token {
        // TODO: Parse escape sequences and do full unicode
        const pos = self.pos;
        _ = self.advance(); // Skip the '
        const char = @intCast(u32, self.cur());
        _ = self.advanceBy(2); // Skip the char and the '
        return Token{
            .pos = pos,
            .tag = .{ .CharLit = char },
        };
    }

    fn parseStropping(self: *Lexer) Token {
        unreachable;
    }

    const StrTok = struct {
        str: []const u8,
        tag: Tag,
    };
    const operators = [_]StrTok{
        // Take care that these are sorted.
        // This can be done with kakoune's pipe mode and ' sort -t'#' -r -k 2 '
        // I use the comment after each to tell it how to sort
        .{ .str = "~", .tag = .BitNot }, // # ~
        .{ .str = "}", .tag = .RBrace }, // # }
        .{ .str = "|", .tag = .BitOr }, // # |
        .{ .str = "{", .tag = .LBrace }, // # {
        .{ .str = "^", .tag = .BitXor }, // # ^
        .{ .str = "]", .tag = .RBracket }, // # ]
        .{ .str = "[", .tag = .LBracket }, // # [
        .{ .str = ">>=", .tag = .ShrAssign }, // # >>=
        .{ .str = ">=", .tag = .GreaterEq }, // # >=
        .{ .str = ">", .tag = .Greater }, // # >
        .{ .str = "===", .tag = .Assert }, // # ===
        .{ .str = "==", .tag = .Eq }, // # ==
        .{ .str = "=", .tag = .Assign }, // # =
        .{ .str = "<=", .tag = .LessEq }, // # <=
        .{ .str = "<<=", .tag = .ShlAssign }, // # <<=
        .{ .str = "<", .tag = .Less }, // # <
        .{ .str = ":", .tag = .Colon }, // # :
        .{ .str = ";", .tag = .Semicolon }, // # ;
        .{ .str = "/=", .tag = .DivAssign }, // # /=
        .{ .str = "/", .tag = .Div },
        .{ .str = "..=", .tag = .ClosedRange }, // # ..=
        .{ .str = "..", .tag = .OpenRange }, // # ..
        .{ .str = ".", .tag = .Dot }, // # .
        .{ .str = "->", .tag = .Store }, // # ->
        .{ .str = "-=", .tag = .SubAssign }, // # -=
        .{ .str = "-", .tag = .Sub }, // # -
        .{ .str = ",", .tag = .Comma }, // # ,
        .{ .str = "+=", .tag = .AddAssign }, // # +=
        .{ .str = "+", .tag = .Add }, // # +
        .{ .str = "*=", .tag = .MulAssign }, // # *=
        .{ .str = "*", .tag = .Mul }, // # *
        .{ .str = ")", .tag = .RParen }, // # )
        .{ .str = "(", .tag = .LParen }, // # (
        .{ .str = "&", .tag = .BitAnd }, // # &
        .{ .str = "%=", .tag = .ModAssign }, // # %=
        .{ .str = "%", .tag = .Mod }, // # %
        .{ .str = "!==", .tag = .NotAssert }, // # !==
        .{ .str = "!=", .tag = .NotEq }, // # !=
        .{ .str = "?", .tag = .Optional }, // # ?
    };

    const keywords = [_]StrTok{
        .{ .str = "_", .tag = .Sink },
        .{ .str = "not", .tag = .Not },
        .{ .str = "and", .tag = .And },
        .{ .str = "or", .tag = .Or },
        .{ .str = "xor", .tag = .Xor },
        .{ .str = "in", .tag = .In },
        .{ .str = "comptime", .tag = .Comptime },
        .{ .str = "pure", .tag = .Pure },
        .{ .str = "inline", .tag = .Inline },
        .{ .str = "let", .tag = .Let },
        .{ .str = "var", .tag = .Var },
        .{ .str = "cvar", .tag = .CVar },
        .{ .str = "field", .tag = .Field },
        .{ .str = "enum", .tag = .Enum },
        .{ .str = "property", .tag = .Property },
        .{ .str = "alias", .tag = .Alias },
        .{ .str = "structdef", .tag = .StructDef },
        .{ .str = "enumdef", .tag = .EnumDef },
        .{ .str = "if", .tag = .If },
        .{ .str = "elif", .tag = .ElIf },
        .{ .str = "else", .tag = .Else },
        .{ .str = "for", .tag = .For },
        .{ .str = "while", .tag = .While },
        .{ .str = "loop", .tag = .Loop },
        .{ .str = "finally", .tag = .Finally },
        .{ .str = "switch", .tag = .Switch },
        .{ .str = "fn", .tag = .Fn },
        .{ .str = "break", .tag = .Break },
        .{ .str = "return", .tag = .Return },
    };
};

test "lexer" {
    var lex = Lexer.init(std.heap.direct_allocator, 0, @embedFile("grammar-mk5.zag"));

    while (lex.scan()) |tok| {
        printf("\n{}", .{tok});
    }

    printf("\n", .{});
}
