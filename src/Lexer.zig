const Lexer = @This();
const This = @This();

const std = @import("std");
const List = std.ArrayList;
const clamp = std.math.clamp;
const Alloc = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const mem = std.mem;
const printf = std.debug.warn;

const isSpace = std.ascii.isSpace;
const isAlNum = std.ascii.isAlNum;

const assert = std.debug.assert;

const StrCache = @import("StrCache.zig");
const Str = StrCache.Str;
const StrID = StrCache.StrID;

pub const TokenType = enum {
    word,
    sigil,
    str,
    punc,
    comment,
    newline,
    indent,
    dedent,
    /// To be used solely to find bad/unset data
    invalid,
};

/// Convenience type for representing comptime knowable tokens
pub const CToken = union(TokenType) {
    word: Str,
    sigil: Str,
    str: Str,
    punc: Str,
    comment: Str,
    newline: void,
    indent: void,
    dedent: void,
    invalid: void,

    pub fn resolve(self: CToken, cache: *StrCache) !Token {
        return switch (self) {
            .word => |s| .{ .word = try cache.intern(s) },
            .sigil => |s| .{ .sigil = try cache.intern(s) },
            .str => |s| .{ .str = try cache.intern(s) },
            .punc => |s| .{ .punc = try cache.intern(s) },
            .comment => |s| .{ .comment = try cache.intern(s) },
            .newline => .{ .newline = .{} },
            .indent => .{ .indent = .{} },
            .dedent => .{ .dedent = .{} },
            .invalid => .{ .invalid = .{} },
        };
    }
};
pub const Token = union(TokenType) {
    word: StrID,
    sigil: StrID,
    str: StrID,
    punc: StrID,
    comment: StrID,

    newline: void,
    indent: void,
    dedent: void,

    invalid: void,

    pub fn id(self: Token) ?StrID {
        return switch (self) {
            .word, .sigil, .str, .punc, .comment => |i| i,
            else => null,
        };
    }
    pub fn hasID(self: Token) bool {
        return self.id() != null;
    }
    pub fn eql(lhs: Token, rhs: Token) bool {
        return std.meta.eql(lhs, rhs);
    }
    pub fn neql(lhs: Token, rhs: Token) bool {
        return !lhs.eql(rhs);
    }

    pub fn print(self: Token, cache: *const StrCache) void {
        switch (self) {
            .word => |i| printf("word({}): `{s}`", .{ i, cache.resolve(i) }),
            .sigil => |i| printf("sigil({}): `{s}`", .{ i, cache.resolve(i) }),
            .str => |i| printf("str({}): `{s}`", .{ i, cache.resolve(i) }),
            .punc => |i| printf("punc({}): `{s}`", .{ i, cache.resolve(i) }),
            .comment => |i| printf("comment({}): `{s}`", .{ i, cache.resolve(i) }),
            .newline => printf("newline", .{}),
            .indent => printf("indent", .{}),
            .dedent => printf("dedent", .{}),
            else => printf("INVALID!!!", .{}),
        }
    }
};

/// Convenience type for representing comptime knowable punctuation
pub const CPunc = struct {
    str: Str,
    preempts: bool,

    rewrite: ?CToken = null,

    pub fn resolve(self: CPunc, cache: *StrCache) !Punc {
        return Punc{
            .str = self.str,
            .id = try cache.intern(self.str),
            .preempts = self.preempts,
            .rewrite = if (self.rewrite) |tok| try tok.resolve(cache) else null,
        };
    }
};
pub const Punc = struct {
    /// The ID of the punctuation within the cache
    id: StrID,
    /// The str that id resolves to. It's here so we don't re-resolve each time.
    /// Only used when `preempts == true`
    str: Str,
    /// If it doesn't preempt, then we check the word/sigil/etc value of the result
    /// against id. If we do preempt, then we check to see if resolve(id) is the next
    /// span of characters before even attempting normal tokenization.
    preempts: bool,
    /// If present, we should resolve instances of this punc to this token instead of a .{.punc = id}
    rewrite: ?Token,
};

pub const FilePos = struct {
    line: usize = 0,
    col: usize = 0,
};

// ======================================================================================
// Fields
// ======================================================================================
/// If true, then we emit .newline and .indent.
/// A dedent will *always* be emitted if a .indent has been emitted previously, it's just that
/// disabling blocking will delay that until it's re-enabled again.
blocking: bool = true,
/// Column of the first char on the line
level: usize = 0,
/// levels we've stopped at so far
indents: List(usize),
/// Position inside the file
pos: FilePos,
/// Position of the /end/ of the last non-ws token
prev_pos: FilePos,
/// If we encounter this *complete* Token,
/// we assume the rest of the line is a comment
/// For C/C++ styled comments where the mere presence of the characters is enough to
/// trigger a comment, use a `.{ .punc = blah }`. If you want the comment to be
/// triggered by a /full/ token (e.g `--` but not `---`), then use a .word or .sigil.
commentor: Token,
/// List of strs to be counted as punctuation. Will be checked sequentially.
puncs: []const Punc,
/// This should technically be the global one, but we'll keep the lexer generic.
cache: *StrCache,

/// The source. Continually advances as `input = input[1..]`
input: Str,
/// Either input[0] if input.len > 0 or 0 if it's not.
cur: u8,
/// Used as a flag so we always return a newline after hitting the end and before returning null or any final dedents
hit_end: bool = false,

// ======================================================================================
// Pub Funcs
// ======================================================================================
pub fn init(input: Str, puncs: []const Punc, commentor: Token, cache: *StrCache, alloc: *Alloc) Lexer {
    return .{
        .cache = cache,
        .indents = List(usize).init(alloc),
        .level = 0,
        .pos = .{},
        // We set this to a (practically) unreachable column so we don't see the first one as attached.
        .prev_pos = .{ .col = 999999999 },
        .input = input,
        .puncs = puncs,
        .commentor = commentor,
        .cur = input[0],
    };
}

pub const ScanRes = struct {
    tok: Token,
    /// Was this token "attached" (preceeded by no whitespace) to the previous token?
    attached: bool,
};
pub fn scan(self: *This) !?ScanRes {
    const prev_line = self.pos.line;
    // printf("Cur0: `{c}`, ", .{self.cur});
    self.skipWs();
    const attached = self.pos.line == self.prev_pos.line and self.pos.col == self.prev_pos.col;
    // printf("cur1: `{c}`\n", .{self.cur});
    if (self.blocking) {
        if (self.pos.line != prev_line or (self.cur == 0 and !self.hit_end)) {
            // std.debug.warn("Newlined. New col: {}. ", .{self.pos.col});
            self.level = self.pos.col;
            if (self.cur == 0) self.hit_end = true;
            return ScanRes{ .tok = Token.newline, .attached = false };
        }

        // std.debug.warn("Scanning. Level: {}. Expected level: {}!\n", .{ self.level, self.expectedLevel() });

        if (self.level > self.expectedLevel()) {
            try self.indents.append(self.level);
            return ScanRes{ .tok = Token.indent, .attached = false };
        } else if (self.level < self.expectedLevel()) {
            _ = self.indents.pop();
            return ScanRes{ .tok = Token.dedent, .attached = false };
        }
    }

    if (self.cur == 0) return null;

    // printf("Entered scan() and skipped ws. Next is {c}\n", .{self.cur});

    var res: Token = .{ .invalid = .{} };
    var was_punc = false;

    for (self.puncs) |punc| {
        if (!punc.preempts) continue;

        if (self.nextIs(punc.str)) {
            was_punc = true;
            res = if (punc.rewrite) |rewrite| rewrite else .{ .punc = punc.id };
            _ = self.advanceN(punc.str.len);
            break;
        }
    }
    // See now here's a prime use case for having `else` and `finally` on loops
    if (!was_punc) {
        const quoted_str = "\"";
        const triple_str = "\"\"\"";
        const line_str = "\\\\"; // \\
        if (try self.scanSurroundedBy(triple_str)) |str| {
            _ = self.advanceN(3); // skip """
            res = .{ .str = try self.cache.intern(str) };
        } else if (try self.scanSurroundedBy(quoted_str)) |str| {
            _ = self.advance(); // skip "
            res = .{ .str = try self.cache.intern(str) };
        } else if (try self.scanBorderedByAny(&.{line_str}, &.{ "\n", &.{0} })) |bor| {
            // Note that we leave the \n in the input for the next skipWs
            res = .{ .str = try self.cache.intern(bor.str) };
        } else if (isWord(self.cur)) {
            res = .{ .word = try self.cache.intern(self.scanWhileCur(isWord)) };
        } else if (isSigil(self.cur)) {
            res = .{ .sigil = try self.cache.intern(self.scanWhileCur(isSigil)) };
        } else {
            printf(
                "Error at {}:{}: I don't know how to handle the character {c}\n",
                .{
                    self.pos.line,
                    self.pos.col,
                    self.cur,
                },
            );
            return error.Unhandlable;
        }
    }

    if (res.id() == null) {
        // It doesn't have a string repr. No sense checking anything else
    } else if (res.neql(self.commentor)) {
        for (self.puncs) |punc| {
            if (punc.preempts) continue;
            if (res.id() != null and res.id().? == punc.id) {
                res = if (punc.rewrite) |rewrite| rewrite else .{ .punc = punc.id };
                break;
            }
        }
    } else {
        // It's a comment. Return the line as a comment
        var i = @as(usize, 0);
        while (self.nth(i) != '\n') {
            if (self.nth(i) == 0) break;
            i += 1;
        }
        res = .{ .comment = try self.cache.intern(self.advanceN(i)) };
    }
    self.prev_pos = self.pos;
    return ScanRes{ .tok = res, .attached = attached };
}

pub fn isWord(c: u8) bool {
    return isAlNum(c) or switch (c) {
        '_', '@' => true,
        else => false,
    };
}
pub fn isSigil(c: u8) bool {
    return switch (c) {
        '~', '!', '@', '$', '%', '^', '&', '*', '-', '=', '+', '<', '>', '|', '/', '?', ':' => true,
        else => false,
    };
}

/// Returns the index of the matching `whats`
fn nextIsAny(self: *const This, whats: []const Str) ?usize {
    for (whats) |what, i| {
        if (self.nextIs(what)) return i;
    }
    return null;
}
fn nextIs(self: *const This, what: Str) bool {
    return self.nextFromNIs(0, what);
}
/// Returns the index of the matching `whats`
fn nextFromNIsAny(self: *const This, n: usize, whats: []const Str) ?usize {
    for (whats) |what, i| {
        if (self.nextFromNIs(n, what)) return i;
    }
    return null;
}
fn nextFromNIs(self: *const This, n: usize, what: Str) bool {
    // printf(
    //     \\nextFromNIs({},"{}") =
    // , .{ n, what });
    if (n >= self.input.len) {
        return if (what[0] == 0) true else false;
    }

    const res = mem.eql(u8, self.spanFromNOfLen(n, what.len), what);
    // printf(" {}\n", .{res});
    return res;
}
fn nth(self: *const This, n: usize) u8 {
    if (n >= self.input.len) return 0;
    return self.input[n];
}
fn spanFromNOfLen(self: *const This, n: usize, l: usize) Str {
    if (n >= self.input.len) return "";
    return self.input[n..clamp(n + l, 0, self.input.len)];
}
fn advanceN(self: *This, n: usize) Str {
    const res = self.input[0..clamp(n, 0, self.input.len)];
    var i = @as(usize, 0);
    while (i < n) {
        _ = self.advance();
        i += 1;
    }
    return res;
}
/// Advance input. Returns the old self.cur
fn advance(self: *This) u8 {
    const old = self.cur;
    if (self.input.len > 0) {
        self.input = self.input[1..];
    }

    if (self.input.len > 0) {
        self.cur = self.input[0];
    } else {
        self.cur = 0;
    }

    self.pos.col += 1;
    if (old == '\n' or (self.cur == 0 and old != 0)) {
        self.pos.line += 1;
        self.pos.col = 0;
    }
    return old;
}

/// Guarded index
fn at(self: This, i: usize) u8 {
    if (i >= self.input.len) return 0;
    return self.input[i];
}

fn skipWs(self: *This) void {
    while (isSpace(self.cur)) _ = self.advance();
}
/// Scans a bordered string where left and right are the same.
/// See: `scanBorderedBy`
fn scanSurroundedBy(self: *This, surrounding: Str) !?Str {
    return self.scanBorderedBy(surrounding, surrounding);
}
/// Scan a string that starts with `left` and ends with `right`. If `left` matches, then
/// it is an error for `right` not to match before the end of the input.
/// If `left` doesn't match, then returns null.
fn scanBorderedBy(self: *This, left: Str, right: Str) !?Str {
    var res = try self.scanBorderedByAny(&[_]Str{left}, &[_]Str{right});
    if (res) |r| {
        return r.str;
    }
    return null;
}
/// Leaves the rights in the input.
const BorderedStr = struct { str: Str, right: usize };
fn scanBorderedByAny(self: *This, lefts: []const Str, rights: []const Str) !?BorderedStr {
    if (self.nextIsAny(lefts)) |i| {
        _ = self.advanceN(lefts[i].len);
    } else return null;

    var n: usize = 0;
    var str: Str = undefined;
    var right: usize = undefined;
    while (true) {
        if (self.nextFromNIsAny(n, rights)) |i| {
            str = self.advanceN(n);
            right = i;
            break;
        }
        if (n > self.input.len) return error.RightNotFound;
        n += 1;
    }

    // printf(
    //     \\Leaving scanBorderedBy("{s}","{s}") when cur is {c}\n
    // , .{ left, right, self.cur });
    return BorderedStr{ .str = str, .right = right };
}
fn scanWhileCur(self: *This, pred: fn (u8) bool) Str {
    var n: usize = 0;
    while (pred(self.at(n))) n += 1;
    return self.advanceN(n);
}

fn expectedLevel(self: This) usize {
    if (self.indents.items.len == 0) return 0;
    return self.indents.items[self.indents.items.len - 1];
}

test "Lexing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var cache = StrCache.init(&arena.allocator);
    const input =
        \\Test 123 . , .. ! !@#!! : :: "Hello, world!" """Hello, world""" \\ Testy
        \\foo
        \\  ?.bar
        \\      !.baz
        \\  .car
        \\    hah
    ;

    const CScanRes = struct {
        tok: CToken,
        attached: ?bool = null,
    };

    const cexpecteds = [_]CScanRes{
        .{ .tok = .{ .word = "Test" }, .attached = false },
        .{ .tok = .{ .word = "123" }, .attached = false },
        .{ .tok = .{ .sigil = "." }, .attached = false },
        .{ .tok = .{ .punc = "," }, .attached = false },
        .{ .tok = .{ .sigil = ".." }, .attached = false },
        .{ .tok = .{ .sigil = "!" }, .attached = false },
        .{ .tok = .{ .sigil = "!@" }, .attached = false },
        .{ .tok = .{ .punc = "#" }, .attached = true },
        .{ .tok = .{ .sigil = "!!" }, .attached = true },
        .{ .tok = .{ .punc = ":" }, .attached = false },
        .{ .tok = .{ .sigil = "::" }, .attached = false },
        .{ .tok = .{ .str = "Hello, world!" }, .attached = false },
        .{ .tok = .{ .str = "Hello, world" }, .attached = false },
        .{ .tok = .{ .str = " Testy" }, .attached = false },
        .{ .tok = .{ .newline = .{} } },
        .{ .tok = .{ .word = "foo" }, .attached = false },
        .{ .tok = .{ .newline = .{} } },
        .{ .tok = .{ .indent = .{} } },
        .{ .tok = .{ .sigil = "?." }, .attached = false },
        .{ .tok = .{ .word = "bar" }, .attached = true },
        .{ .tok = .{ .newline = .{} } },
        .{ .tok = .{ .indent = .{} } },
        .{ .tok = .{ .sigil = "!." }, .attached = false },
        .{ .tok = .{ .word = "baz" }, .attached = true },
        .{ .tok = .{ .newline = .{} } },
        .{ .tok = .{ .dedent = .{} } },
        .{ .tok = .{ .sigil = "." }, .attached = false },
        .{ .tok = .{ .word = "car" }, .attached = true },
        .{ .tok = .{ .newline = .{} } },
        .{ .tok = .{ .indent = .{} } },
        .{ .tok = .{ .word = "hah" }, .attached = false },
        .{ .tok = .{ .newline = .{} } },
        .{ .tok = .{ .dedent = .{} } },
        .{ .tok = .{ .dedent = .{} } },
    };
    const Expected = struct {
        tok: Token,
        attached: ?bool,
    };

    var expecteds: [cexpecteds.len]Expected = undefined;
    for (cexpecteds) |cexpected, i|
        expecteds[i] = .{ .tok = try cexpected.tok.resolve(&cache), .attached = cexpected.attached };

    const cpuncs = [_]CPunc{
        .{ .str = ",", .preempts = true },
        .{ .str = ":", .preempts = false },
        .{ .str = "(", .preempts = true },
        .{ .str = ")", .preempts = true },
        .{ .str = ";", .preempts = true },
        .{ .str = "--", .preempts = true },
        .{ .str = "..=", .preempts = true, .rewrite = .{ .sigil = "..=" } },
        .{ .str = "..", .preempts = true, .rewrite = .{ .sigil = ".." } },
        .{ .str = ".", .preempts = true, .rewrite = .{ .sigil = "." } },
        .{ .str = "!.", .preempts = true, .rewrite = .{ .sigil = "!." } },
        .{ .str = "?.", .preempts = true, .rewrite = .{ .sigil = "?." } },
        .{ .str = "#", .preempts = true },
    };
    var puncs: [cpuncs.len]Punc = undefined;
    for (cpuncs) |cpunc, i|
        puncs[i] = try cpunc.resolve(&cache);

    const commentor = Token{ .punc = try cache.intern("--") };

    var lexer = Lexer.init(input, puncs[0..], commentor, &cache, cache.alloc);

    printf("\n", .{});
    var i: usize = 0;
    while (try lexer.scan()) |res| {
        const tok = res.tok;
        tok.print(&cache);
        printf(" (attached: {})\n", .{res.attached});
        if (tok.neql(expecteds[i].tok)) {
            printf("Found: ", .{});
            tok.print(&cache);
            printf("\nExpected: ", .{});
            expecteds[i].tok.print(&cache);
            printf("\n", .{});
            return error.Unexpected;
        }
        if (expecteds[i].attached) |should| {
            if (should != res.attached) {
                printf("Expected the {}th token to {s} attached.\n", .{
                    i,
                    // Note to self: In Ligi, make the string lit type be `comptime slice comptime const u8`
                    // and allow implicit conversion of `comptime slice comptime T` -> any array of T
                    if (should) @as([]const u8, "be") else @as([]const u8, "not be"),
                });
                return error.NotAttached;
            }
        }
        i += 1;
    }
    if (i != expecteds.len) {
        printf("Only scanned {} of them before hitting the end!\n", .{i});
        return error.NotEnoughTokens;
    }
}
