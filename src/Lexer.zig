const Lexer = @This();
const This = @This();

const std = @import("std");
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
            .invalid => printf("INVALID!!!", .{}),
            else => {},
        }
    }
};

/// Convenience type for representing comptime knowable punctuation
pub const CPunc = struct {
    str: Str,
    preempts: bool,

    pub fn resolve(self: CPunc, cache: *StrCache) !Punc {
        return Punc{
            .str = self.str,
            .id = try cache.intern(self.str),
            .preempts = self.preempts,
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
};

// ======================================================================================
// Fields
// ======================================================================================
/// Column of the first char on the line
level: usize,
/// Column of the next non-whitespace character
col: usize,
/// Line of the next non-whitespace character
line: usize,
/// If we encounter this *complete* Token,
/// we assume the rest of the line is a comment
/// For C/C++ styled comments where the mere presence of the characters is enough to
/// trigger a comment, use a `.{ .punc = blah }`. If you want the comment to be
/// triggered by a /full/ token (e.g `--` but not `---`), then use a .word or .sigil.
commentor: Token,
/// List of strs to be counted as punctuation. Will be checked sequentially.
puncs: []const Punc,
cache: *StrCache,

/// The source. Continually advances as `input = input[1..]`
input: Str,
/// Either input[0] if input.len > 0 or 0 if it's not.
cur: u8,

// ======================================================================================
// Pub Funcs
// ======================================================================================
pub fn init(input: Str, puncs: []const Punc, commentor: Token, cache: *StrCache) Lexer {
    return .{
        .cache = cache,
        .level = 0,
        .col = 0,
        .line = 0,
        .input = input,
        .puncs = puncs,
        .commentor = commentor,
        .cur = input[0],
    };
}

pub fn scan(self: *This) !?Token {
    self.skipWs();
    if (self.cur == 0) return null;

    // printf("Entered scan() and skipped ws. Next is {c}\n", .{self.cur});

    var res: Token = .{ .invalid = .{} };
    var was_punc = false;

    for (self.puncs) |punc| {
        if (!punc.preempts) continue;

        if (self.nextIs(punc.str)) {
            was_punc = true;
            res = .{ .punc = punc.id };
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
            res = .{ .str = try self.cache.intern(str) };
        } else if (try self.scanSurroundedBy(quoted_str)) |str| {
            res = .{ .str = try self.cache.intern(str) };
        } else if (try self.scanBorderedByAny(&.{line_str}, &.{ "\n", &.{0} })) |str| {
            res = .{ .str = try self.cache.intern(str) };
        } else if (isWord(self.cur)) {
            res = .{ .word = try self.cache.intern(self.scanWhileCur(isWord)) };
        } else if (isSigil(self.cur)) {
            res = .{ .sigil = try self.cache.intern(self.scanWhileCur(isSigil)) };
        } else {
            printf(
                "Error at {}:{}: I don't know how to handle the character {c}\n",
                .{
                    self.line,
                    self.col,
                    self.cur,
                },
            );
            return error.Unhandlable;
        }
    }

    // It doesn't have a string repr. No sense checking anything else
    if (res.id() == null) return res;

    if (res.neql(self.commentor)) {
        for (self.puncs) |punc| {
            if (punc.preempts) continue;
            if (res.id() != null and res.id().? == punc.id) {
                res = .{ .punc = punc.id };
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
    return res;
}

pub fn isWord(c: u8) bool {
    return isAlNum(c) or switch (c) {
        '_', '@' => true,
        else => false,
    };
}
pub fn isSigil(c: u8) bool {
    return switch (c) {
        '~',
        '!',
        '@',
        '$',
        '%',
        '^',
        '&',
        '*',
        '-',
        '=',
        '+',
        '<',
        '>',
        '.',
        '|',
        '/',
        '?',
        ':',
        => true,
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

    return old;
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
    return self.scanBorderedByAny(&[_]Str{left}, &[_]Str{right});
}
fn scanBorderedByAny(self: *This, lefts: []const Str, rights: []const Str) !?Str {
    if (self.nextIsAny(lefts)) |i| {
        _ = self.advanceN(lefts[i].len);
    } else return null;

    var n: usize = 0;
    var str: Str = undefined;
    while (true) {
        if (self.nextFromNIsAny(n, rights)) |i| {
            str = self.advanceN(n);
            _ = self.advanceN(rights[i].len);
            break;
        }
        if (n > self.input.len) return error.RightNotFound;
        n += 1;
    }

    // printf(
    //     \\Leaving scanBorderedBy("{s}","{s}") when cur is {c}\n
    // , .{ left, right, self.cur });
    return str;
}
fn scanWhileCur(self: *This, pred: fn (u8) bool) Str {
    var n: usize = 0;
    while (pred(self.input[n])) n += 1;
    return self.advanceN(n);
}

test "Lexing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var cache = StrCache.init(&arena.allocator);
    const input =
        \\ Test 123 . , .. ! !@#!! : :: "Hello, world!" """Hello, world""" \\ Testy
    ;

    const cexpecteds = [_]CToken{
        .{ .word = "Test" },
        .{ .word = "123" },
        .{ .sigil = "." },
        .{ .punc = "," },
        .{ .sigil = ".." },
        .{ .sigil = "!" },
        .{ .sigil = "!@" },
        .{ .punc = "#" },
        .{ .sigil = "!!" },
        .{ .punc = ":" },
        .{ .sigil = "::" },
        .{ .str = "Hello, world!" },
        .{ .str = "Hello, world" },
        .{ .str = " Testy" },
    };
    var expecteds: [cexpecteds.len]Token = undefined;
    for (cexpecteds) |cexpected, i|
        expecteds[i] = try cexpected.resolve(&cache);

    const cpuncs = [_]CPunc{
        .{ .str = ",", .preempts = true },
        .{ .str = ":", .preempts = false },
        .{ .str = "(", .preempts = true },
        .{ .str = ")", .preempts = true },
        .{ .str = ";", .preempts = true },
        .{ .str = "--", .preempts = true },
        .{ .str = "#", .preempts = true },
    };
    var puncs: [cpuncs.len]Punc = undefined;
    for (cpuncs) |cpunc, i|
        puncs[i] = try cpunc.resolve(&cache);

    const commentor = Token{ .punc = try cache.intern("--") };

    var lexer = Lexer.init(input, puncs[0..], commentor, &cache);

    printf("\n", .{});
    var i: usize = 0;
    while (try lexer.scan()) |tok| {
        // tok.print(&cache);
        // printf("\n", .{});
        if (tok.neql(expecteds[i])) {
            printf("Found: ", .{});
            tok.print(&cache);
            printf("\nExpected: ", .{});
            expecteds[i].print(&cache);
            printf("\n", .{});
            return error.TestFail;
        }
        i += 1;
    }
}
