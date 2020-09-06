const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const AutoHashMap = std.AutoHashMap;
const StrMap = std.StringHashMap;
const HashMap = std.HashMap;
const assert = std.debug.assert;

const printf = std.debug.warn;

const MAX_INDENT = 100;

// Stores all strings internally as null-terminated, but exposes them as normal
const StringCache = struct {
    const IDMap = StrMap(StrID);
    alloc: *std.mem.Allocator, // Used for both the maps and copying strings
    ids: IDMap,
    strs: AutoHashMap(StrID, [:0]const u8),
    next: StrID = 0,

    pub fn init(alloc: *std.mem.Allocator) StringCache {
        return .{
            .alloc = alloc,
            .ids = IDMap.init(alloc),
            .strs = AutoHashMap(StrID, [:0]const u8).init(alloc),
        };
    }

    pub fn strID(self: *@This(), str: []const u8) StrID {
        if (!self.ids.contains(str)) {
            // Assume that strs doesn't contain it either
            var copy: [:0]u8 = self.alloc.allocSentinel(u8, str.len, 0) catch unreachable;
            for (str) |c, i| copy[i] = c;

            self.ids.putNoClobber(copy, self.next) catch unreachable;
            self.strs.putNoClobber(self.next, copy) catch unreachable;
            self.next += 1;
            return self.next - 1;
        } else {
            return self.ids.get(str).?.value;
        }
    }

    const empty: [:0]const u8 = "";
    pub fn idStr(self: *@This(), id: StrID) [:0]const u8 {
        const res = self.strs.get(id);
        if (res) |e| {
            return e.value;
        } else {
            return empty;
        }
    }
};

const File = struct {
    cache: *StringCache,

    original: []const u8, // Input before skipping around
    input: []const u8,
    levels: [MAX_INDENT]usize, // [0] is always 0
    indent: usize = 0, // Index into level

    newlined: bool = false, // Did we just newline? Overwritten by Indent/Dedent
    level: usize = 0, // The col of the first of this line

    line: usize = 0,
    col: usize = 0,

    commentor: []const u8 = "--", // The string that can start a line comment

    fn init(cache: *StringCache, input: []const u8) File {
        var res = File{
            .cache = cache,
            .original = input,
            .input = input,
            .levels = undefined,
        };
        for (res.levels) |*l| l.* = 0;
        return res;
    }

    fn curDent(self: *@This()) usize {
        return self.levels[self.indent];
    }
    fn popDent(self: *@This()) void {
        if (self.indent > 0) {
            self.levels[self.indent] = 0;
            self.indent -= 1;
        }
    }
    fn pushDent(self: *@This()) void {
        assert(self.indent < MAX_INDENT);
        self.indent += 1;
        self.levels[self.indent] = self.col;
    }
    fn cur(self: *@This()) u8 {
        if (self.input.len == 0) return 0;
        return self.input[0];
    }
    fn advance(self: *@This(), n: usize) []const u8 {
        if (self.input.len < n) @panic("Can't scan that many!");

        const res = self.input[0..n];
        self.input = self.input[n..];
        return res;
    }
    fn scan(self: *@This()) u8 {
        if (self.input.len == 0) {
            return 0;
        } else {
            const res = self.cur();
            self.input = self.input[1..];
            self.col += 1;
            return res;
        }
    }
    fn nextIs(self: *@This(), str: []const u8) bool {
        if (self.input.len < str.len) return false;
        // printf("Checking `{}` against `{}`...", .{ str, self.input[0..1] });
        const res = std.mem.eql(u8, self.input[0..str.len], str);
        // printf("Was {}!\n", .{res});
        return res;
    }
    fn skipWs(self: *@This()) void {
        var clean = false;
        while (!clean) {
            clean = true;
            while (self.cur() == ' ') _ = self.scan();
            if (self.nextIs(self.commentor)) {
                clean = false;
                while (self.cur() != '\n' and self.cur() != 0) _ = self.scan();
            }

            if (self.cur() == '\n') {
                clean = false;
                _ = self.advance(1);
                self.col = 0;
                self.line += 1;
                self.newlined = true;
            }
        }
        // Col of the first character after all newlines
        if (self.newlined) {
            self.level = self.col;
            // We pushDent in the lex function
        }
    }

    // TODO: Since we're able to scan letter by letter, it's probably better to cache as a trie,
    // since we can iterate that letter by letter.

    // Assumes that input[0] is a word char
    fn scanWord(self: *@This()) StrID {
        var n: usize = 0;
        while (n < self.input.len and isWordChar(self.input[n])) n += 1;
        return self.cache.strID(self.advance(n));
    }
    fn scanSigil(self: *@This()) StrID {
        var n: usize = 0;
        while (n < self.input.len and isSigilChar(self.input[n])) n += 1;
        return self.cache.strID(self.advance(n));
    }
    fn scanQuoted(self: *@This()) StrID {
        if (self.scan() != '"') @panic("Expected a `\"`");

        var n: usize = 0;
        while (self.input[n] != '"') {
            // Escapes just include the `\` and the next character, as long as it's not a newline
            if (self.input[n] == '\\') n += 1;
            if (self.input[n] == '\n') @panic("Can't have newlines in quoted string lits. Use a \\\\ lit");
            n += 1;
        }
        const res = self.cache.strID(self.advance(n));
        if (self.scan() != '"') @panic("Expected a `\"`");
        return res;
    }
    fn scanGenericWord(self: *@This()) StrID {
        if (isSigilChar(self.cur())) {
            return self.scanSigil();
        } else if (isWordChar(self.cur())) {
            return self.scanWord();
        } else if (self.nextIs("\"")) {
            return self.scanQuoted();
        } else {
            @panic("Expected a word, sigil, or quoted string");
        }
    }
    fn lex(self: *File) Token {
        var res = Token{
            .tag = .EOF,
        };

        self.skipWs();
        if (self.newlined) {
            if (self.level > self.curDent()) {
                self.pushDent();
                res.tag = .Indent;
            } else if (self.level < self.curDent()) {
                self.popDent();
                res.tag = .Dedent;
            } else {
                res.tag = .Newline;
            }
            self.newlined = false;
        } else if (self.cur() == 0) {
            // res is already EOF
            // printf("Doing nothin...\n", .{});
        } else if (isWordChar(self.cur())) {
            res.tag = .Word;
            res.str = self.scanWord();
        } else if (isSigilChar(self.cur())) {
            res.tag = .Sigil;
            res.str = self.scanSigil();
        } else if (self.nextIs("\\\\")) {
            // Line string literal
            @panic("Line string lit not yet implemented!");
        } else if (self.nextIs("\"")) {
            // Quoted string literal
            res.tag = .Str;
            res.str = self.scanQuoted();
        } else if (self.nextIs("'")) {
            // Character literal
            @panic("Char lit not yet implemented!");
        } else if (self.nextIs("\\")) {
            _ = self.advance(1);
            res.tag = .Strop;
            res.str = self.scanGenericWord();
            // printf("Took the strop route for {}\n", .{self.cache.idStr(res.str)});
        } else if (self.nextIs("#")) {
            res.tag = .Tag;
            res.str = self.scanGenericWord();
        } else {
            // printf("Got an else for `{}`\n", .{self.cur()});
            @panic("Unrecognized character!");
        }

        return res;
    }
};

const Tag = extern enum(c_int) {
    EOF,

    Newline,
    Indent,
    Dedent,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Semicolon,
    Comma,
    Tag, // #

    Str,
    Word, // A normal word like 1, a, or void, or an escaped sigil like \+ (`\` not included)
    Sigil, // A series of +, -, *, and others
    Strop, // A word that was explicitly stropped (like `\if`)
};

const StrID = usize;

const Token = extern struct {
    tag: Tag,
    str: StrID = 0, // Valid for Word, Sigil, and Strop. 0 otherwise

    fn eql(lhs: @This(), rhs: @This()) bool {
        // printf("Check {} == {}\n", .{ lhs, rhs });
        return lhs.tag == rhs.tag and lhs.str == rhs.str;
    }
};

fn isWordChar(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '0'...'9', '_', '@' => true,
        else => false,
    };
}
fn isSigilChar(c: u8) bool {
    return switch (c) {
        '+', '-', '*', '~', '!', '.', '=', '>', '<', '&', '^', '%', '$', '?', ':', '|' => true,
        else => false,
    };
}

pub fn lex(self: *File) callconv(.C) Token {
    return self.lex();
}

const testing = std.testing;
const expect = testing.expect;

test "interning" {
    // printf("\n", .{});
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    var cache = StringCache.init(&arena.allocator);
    const a_id = cache.strID("a");
    expect(a_id == cache.strID("a"));
    const a = cache.idStr(a_id);
    expect(std.mem.eql(u8, a, "a"));
}

test "word parsing" {
    // printf("\n", .{});
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    var cache = StringCache.init(&arena.allocator);
    var input = File.init(&cache,
        \\ a _ HUT @printf
    );

    const ids = [_]StrID{
        cache.strID("a"), cache.strID("_"), cache.strID("HUT"), cache.strID("@printf"),
    };

    for (ids) |id| {
        expect(input.lex().eql(.{ .tag = .Word, .str = id }));
    }
}

test "sigil parsing" {
    // printf("\n", .{});
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    var cache = StringCache.init(&arena.allocator);
    var input = File.init(&cache,
        \\ + - * ~ ! . = > < & ^ % $ ? : | <= >= != === ===>
    );

    const ids = [_]StrID{
        cache.strID("+"), cache.strID("-"), cache.strID("*"), cache.strID("~"), cache.strID("!"), cache.strID("."), cache.strID("="), cache.strID(">"), cache.strID("<"), cache.strID("&"), cache.strID("^"), cache.strID("%"), cache.strID("$"), cache.strID("?"), cache.strID(":"), cache.strID("|"), cache.strID("<="), cache.strID(">="), cache.strID("!="), cache.strID("==="), cache.strID("===>"),
    };

    for (ids) |id| {
        expect(input.lex().eql(.{ .tag = .Sigil, .str = id }));
    }
}

test "stropped parsing" {
    // printf("\n", .{});
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    var cache = StringCache.init(&arena.allocator);
    var input = File.init(&cache,
        \\ \if \+ \"Testy boi"
    );

    const ids = [_]StrID{
        cache.strID("if"), cache.strID("+"), cache.strID("Testy boi"),
    };

    for (ids) |id| {
        const t = input.lex();
        // printf("\nGot `{}`({}), expected `{}`({}) \n", .{ cache.idStr(t.str), t.str, cache.idStr(id), id });
        expect(t.eql(.{ .tag = .Strop, .str = id }));
    }
}

test "indentation" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    var cache = StringCache.init(&arena.allocator);
    // Newline -> Indent -> Dedent
    var input = File.init(&cache,
        \\0 = 0
        \\a = 1
        \\  b = 2
        \\c = 3
    );
    const tags = [_]Tag{
        .Word, .Sigil, .Word, .Newline,
        .Word, .Sigil, .Word, .Indent,
        .Word, .Sigil, .Word, .Dedent,
        .Word, .Sigil, .Word,
    };

    for (tags) |tag| {
        const t = input.lex();
        // printf("\nGot {}, expected {} \n", .{ t.tag, tag });
        expect(t.tag == tag);
    }
}
