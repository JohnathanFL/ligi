const Lexer = @This();
const This = @This();

const std = @import("std");
const List = std.ArrayList;
const clamp = std.math.clamp;
const Alloc = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const mem = std.mem;
const printf = std.debug.warn;

const common = @import("common_ast.zig");
const CommonID = common.CommonID;
const resolveCommon = common.resolveCommon;

const isSpace = std.ascii.isSpace;
const isAlNum = std.ascii.isAlNum;

const assert = std.debug.assert;

const StrCache = @import("StrCache.zig");
const Str = StrCache.Str;
const StrID = StrCache.StrID;

pub fn TokenDict(comptime V: type) type {
    return std.HashMap(Token, V, Token.hash, Token.eql, 80);
}

pub const TokenType = enum {
    word,
    str,
    punc,
    comment,
    /// To be used solely to find bad/unset data
    invalid,
    ///Soley at compiletime
    common,
};

/// Convenience type for representing comptime knowable tokens
pub const CToken = union(TokenType) {
    common: CommonID,
    word: Str,
    str: Str,
    punc: Str,
    comment: Str,
    invalid: void,

    pub fn resolve(self: CToken, cache: *StrCache) LexError!Token {
        return switch (self) {
            .common => |c| .{ .word = resolveCommon(c) },
            .word => |s| .{ .word = try cache.intern(s) },
            .str => |s| .{ .str = try cache.intern(s) },
            .punc => |s| .{ .punc = try cache.intern(s) },
            .comment => |s| .{ .comment = s },
            .invalid => .{ .invalid = .{} },
        };
    }
};
pub const Token = union(TokenType) {
    word: StrID,
    str: StrID,
    punc: StrID,

    comment: Str,

    invalid: void,
    common: void,

    pub fn id(self: Token) ?StrID {
        return switch (self) {
            .word, .str, .punc => |i| i,
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
            .str => |i| printf("str({}): `{s}`", .{ i, cache.resolve(i) }),
            .punc => |i| printf("punc({}): `{s}`", .{ i, cache.resolve(i) }),
            .comment => |i| printf("comment(`{s}`)", .{i}),
            else => printf("INVALID!!!", .{}),
        }
    }

    pub fn hash(self: Token) u64 {
        var res = @as(u64, @enumToInt(std.meta.activeTag(self)));
        if (self.id()) |i| {
            res ^= i;
        } else if (self == .comment) {
            var sum: u64 = 0;
            for (self.comment) |c| sum +%= c;
            res ^= sum;
        }
        return res;
    }
};

/// Convenience type for representing comptime knowable punctuation
pub const CPunc = struct {
    str: Str,
    preempts: bool,
    // TODO: Let rewrites specify a function to rewrite themselves.
    rewrite: ?CToken = null,
    comments: bool = false,

    pub fn resolve(self: CPunc, cache: *StrCache) LexError!Punc {
        return Punc{
            .str = self.str,
            .id = try cache.intern(self.str),
            .preempts = self.preempts,
            .rewrite = if (self.rewrite) |tok| try tok.resolve(cache) else null,
            .comments = self.comments,
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
    /// Does encountering this Punc trigger a line comment?
    comments: bool = false,
};

pub const FilePos = struct {
    line: usize = 0,
    col: usize = 0,
    /// The col of the first non-whitespace character in this file.
    level: usize = 0,
};

pub const LexError = error{
    OutOfMemory,
    Unhandlable,
    RightNotFound,
};

pub const ScanRes = struct {
    tok: Token,
    /// Was this token "attached" (preceeded by no whitespace) to the previous token?
    /// Note this means that it is impossible for two words or two sigils to be "attached" to one another directly,
    /// along with certain other combinations like 2 `:` puncs (which would be a `::` sigil).
    attached: bool,
    pos: FilePos,
};

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Fields                                                                     ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
/// Position inside the file
pos: FilePos,
/// Position of the /end/ of the last non-ws token
prev_pos: FilePos,
/// List of strs to be counted as punctuation. Will be checked sequentially.
puncs: []const Punc,
/// This should technically be the global one, but we'll keep the lexer generic.
cache: *StrCache,

/// The source. Continually advances as `input = input[1..]`
input: Str,
/// Either input[0] if input.len > 0 or 0 if it's not.
cur: u8,

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Pub Funcs                                                                  ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
pub fn init(input: Str, puncs: []const Punc, cache: *StrCache, alloc: *Alloc) Lexer {
    return .{
        .cache = cache,
        .pos = .{},
        // We set this to a (practically) unreachable column so we don't see the first one as attached.
        .prev_pos = .{ .col = 999999999 },
        .input = input,
        .puncs = puncs,
        .cur = input[0],
    };
}

pub fn scan(self: *This) LexError!?ScanRes {
    const prev_line = self.pos.line;
    // printf("Cur0: `{c}`, ", .{self.cur});
    self.skipWs();
    const attached = self.pos.line == self.prev_pos.line and self.pos.col == self.prev_pos.col;
    const pos = self.pos;
    // printf("cur1: `{c}`\n", .{self.cur});
    if (self.pos.line != prev_line or self.cur == 0) {
        // std.debug.warn("Newlined. New col: {}. ", .{self.pos.col});
        self.pos.level = self.pos.col;
    }

    if (self.cur == 0) return null;

    // printf("Entered scan() and skipped ws. Next is {c}\n", .{self.cur});

    var res: Token = .{ .invalid = .{} };
    var was_punc = false;

    for (self.puncs) |punc| {
        if (!punc.preempts) continue;

        if (self.nextIs(punc.str)) {
            was_punc = true;
            var should_advance = true;
            if (punc.comments) {
                res = .{ .comment = self.scanWhileCur(isNotNewline) };
                printf("Hit comment: {s}\n", .{res.comment});
                _ = self.advance(); // Over newline
                should_advance = false;
            } else if (punc.rewrite) |rewrite| {
                res = rewrite;
            } else {
                res = .{ .punc = punc.id };
            }
            if (should_advance) _ = self.advanceN(punc.str.len);
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
            res = .{ .word = try self.cache.intern(self.scanWhileCur(isSigil)) };
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
    } else {
        for (self.puncs) |punc| {
            if (punc.preempts) continue;
            if (res.id() != null and res.id().? == punc.id) {
                if (punc.comments) {
                    res = .{ .comment = self.scanWhileCur(isNotNewline) };
                }
                if (punc.rewrite) |rewrite| {
                    res = rewrite;
                } else {
                    res = .{ .punc = punc.id };
                }
                break;
            }
        }
    }

    self.prev_pos = self.pos;
    return ScanRes{ .tok = res, .attached = attached, .pos = pos };
}

pub fn isNotNewline(c: u8) bool {
    return c != '\n';
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
    var i: usize = 0;
    while (i < n) {
        const over = self.advance();
        // printf("While advancing {}, advanced over `{c}`\n", .{ n, over });
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
fn scanSurroundedBy(self: *This, surrounding: Str) LexError!?Str {
    return self.scanBorderedBy(surrounding, surrounding);
}
/// Scan a string that starts with `left` and ends with `right`. If `left` matches, then
/// it is an error for `right` not to match before the end of the input.
/// If `left` doesn't match, then returns null.
fn scanBorderedBy(self: *This, left: Str, right: Str) LexError!?Str {
    var res = try self.scanBorderedByAny(&[_]Str{left}, &[_]Str{right});
    if (res) |r| {
        return r.str;
    }
    return null;
}
/// Leaves the rights in the input.
const BorderedStr = struct { str: Str, right: usize };
fn scanBorderedByAny(self: *This, lefts: []const Str, rights: []const Str) LexError!?BorderedStr {
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
pub const COMMON_CPUNCS = [_]CPunc{
    .{ .str = ",", .preempts = true },
    .{ .str = ":", .preempts = false },
    .{ .str = "(", .preempts = true },
    .{ .str = ")", .preempts = true },
    .{ .str = ";", .preempts = true },
    .{ .str = "..=", .preempts = true, .rewrite = .{ .word = "..=" } },
    .{ .str = "..", .preempts = true, .rewrite = .{ .word = ".." } },
    .{ .str = ".", .preempts = true, .rewrite = .{ .word = "." } },
    .{ .str = "!.", .preempts = true, .rewrite = .{ .word = "!." } },
    .{ .str = "?.", .preempts = true, .rewrite = .{ .word = "?." } },
    .{ .str = "#", .preempts = true },
    .{ .str = "--", .preempts = true, .comments = true },
};

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
        .{ .tok = .{ .word = "." }, .attached = false },
        .{ .tok = .{ .punc = "," }, .attached = false },
        .{ .tok = .{ .word = ".." }, .attached = false },
        .{ .tok = .{ .word = "!" }, .attached = false },
        .{ .tok = .{ .word = "!@" }, .attached = false },
        .{ .tok = .{ .punc = "#" }, .attached = true },
        .{ .tok = .{ .word = "!!" }, .attached = true },
        .{ .tok = .{ .punc = ":" }, .attached = false },
        .{ .tok = .{ .word = "::" }, .attached = false },
        .{ .tok = .{ .str = "Hello, world!" }, .attached = false },
        .{ .tok = .{ .str = "Hello, world" }, .attached = false },
        .{ .tok = .{ .str = " Testy" }, .attached = false },
        // .{ .tok = .{ .newline = .{} } },
        .{ .tok = .{ .word = "foo" }, .attached = false },
        // .{ .tok = .{ .newline = .{} } },
        // .{ .tok = .{ .indent = .{} } },
        .{ .tok = .{ .word = "?." }, .attached = false },
        .{ .tok = .{ .word = "bar" }, .attached = true },
        // .{ .tok = .{ .newline = .{} } },
        // .{ .tok = .{ .indent = .{} } },
        .{ .tok = .{ .word = "!." }, .attached = false },
        .{ .tok = .{ .word = "baz" }, .attached = true },
        // .{ .tok = .{ .newline = .{} } },
        // .{ .tok = .{ .dedent = .{} } },
        .{ .tok = .{ .word = "." }, .attached = false },
        .{ .tok = .{ .word = "car" }, .attached = true },
        // .{ .tok = .{ .newline = .{} } },
        // .{ .tok = .{ .indent = .{} } },
        .{ .tok = .{ .word = "hah" }, .attached = false },
        // .{ .tok = .{ .newline = .{} } },
        // .{ .tok = .{ .dedent = .{} } },
        // .{ .tok = .{ .dedent = .{} } },
    };
    const Expected = struct {
        tok: Token,
        attached: ?bool,
    };

    var expecteds: [cexpecteds.len]Expected = undefined;
    for (cexpecteds) |cexpected, i|
        expecteds[i] = .{ .tok = try cexpected.tok.resolve(&cache), .attached = cexpected.attached };

    var puncs: [COMMON_CPUNCS.len]Punc = undefined;
    for (COMMON_CPUNCS) |cpunc, i|
        puncs[i] = try cpunc.resolve(&cache);

    const commentor = Token{ .punc = try cache.intern("--") };

    var lexer = Lexer.init(input, puncs[0..], &cache, cache.alloc);

    printf("\n", .{});
    var i: usize = 0;
    while (try lexer.scan()) |res| {
        const tok = res.tok;
        // tok.print(&cache);
        // printf(" (attached: {})\n", .{res.attached});
        if (tok.neql(expecteds[i].tok)) {
            printf("#{}:\n", .{i});
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

test "Token equality" {
    // const tokLhs = Token{ .newline = .{} };
    // const tokRhs = Token{ .newline = .{} };

    // assert(tokLhs.eql(tokRhs));
}
