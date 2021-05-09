pub const Parser = @This();

const std = @import("std");
const printf = std.debug.warn;
const Alloc = std.mem.Allocator;
const List = std.ArrayList;

const StrCache = @import("StrCache.zig");
const StrID = StrCache.StrID;

const Lexer = @import("Lexer.zig");
const FilePos = Lexer.FilePos;
const ScanRes = Lexer.ScanRes;
const Token = Lexer.Token;
const CToken = Lexer.CToken;
const ast = @import("ast.zig");
const Atom = ast.Atom;
const common = @import("common_ast.zig");
const CommonID = common.CommonID;
const TokenDict = Lexer.TokenDict;

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Accompanying Types                                                         ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
pub const ParseHook = fn (*Parser) ParseError!?Atom;

pub const PrecKind = enum { none, binary, access, assg };
pub const CPrec = struct { tok: CToken, prec: PrecKind = .binary };
pub const Prec = union(PrecKind) {
    none: void,
    binary: usize,
    access: void,
    assg: void,

    pub fn eql(p1: Prec, p2: Prec) bool {
        return switch (p1) {
            .binary => p1.binary == p2.binary,
            .access => p2 == .access,
            .assg => p2 == .assg,
        };
    }
};

pub const Matchable = union(enum) {
    token: Token,
    common: union(enum) {
        word: CommonID,
        punc: CommonID,

        pub fn toStr(self: @This()) []const u8 {
            return common.resolve(common.resolveCommon(switch (self) {
                .word => |x| x,
                .punc => |x| x,
            }));
        }
    },
    prec: Prec,
    /// Anything that can be a binary operator
    any_binary: void,
    /// Anything that can be an access operator
    any_access: void,
    any_assg: void,
    any_word: void,

    any_of: []const Matchable,
    one_of: []const Matchable,
    seq_of: []const Matchable,

    pub fn eql(self: Matchable, tok: Token, parser: *const Parser) bool {
        switch (self) {
            .token => |t| return tok.eql(t),
            .common => |com| switch (com) {
                .word => |id| return tok.eql(.{ .word = common.resolveCommon(id) }),
                .punc => |id| return tok.eql(.{ .punc = common.resolveCommon(id) }),
            },
            .prec => |p1| {
                return if (parser.precs.get(tok)) |p2| p1.eql(p2) else false;
            },
            .any_binary => {
                return if (parser.precs.get(tok)) |p| p == .binary else false;
            },
            .any_access => {
                return if (parser.precs.get(tok)) |p| p == .access else false;
            },
            .any_assg => {
                return if (parser.precs.get(tok)) |p| p == .assg else false;
            },
            .any_word => return tok == .word,
            else => return false,
        }
    }
};

pub const ParseError = error{
    Unmatched,
    OutOfMemory,
    Unhandlable,
    RightNotFound,
};

pub const TokInfo = struct {
    tok: Token,
    pos: FilePos,
    attached: bool,
    prec: Prec,

    pub fn from(s: ScanRes, p: *Parser) TokInfo {
        return .{
            .tok = s.tok,
            .pos = s.pos,
            .attached = s.attached,
            .prec = p.precOf(s.tok, s.attached),
        };
    }
};

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Fields                                                                     ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
cache: *StrCache,
lexer: *Lexer,

toks: List(TokInfo),
/// The level we expect to be at. Will be routinely pushed back by functions, with the new one
/// being popped back on with a `defer`.
ref_level: usize,
/// Set if something unambiguously ended the current expression level.
/// Parsers like tuples/blocks/etc should unset this flag before moving to the
/// next value in the chain.
/// For example, control structures should usually set this, as they have no concrete end token.
ended: bool = false,

/// Parsed just below the unary level. Allows for things like `if`, `{}`, `fn`, `$`, etc.
hooks: TokenDict(ParseHook),
precs: TokenDict(Prec),

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Interface                                                                  ║
// ╚═══════════════════════════════════════════════════════════════════════════╝

pub fn init(lexer: *Lexer, alloc: *Alloc) Parser {
    return .{
        .ref_level = 0,
        .lexer = lexer,
        .cache = lexer.cache,
        .toks = List(TokInfo).init(alloc),
        .hooks = TokenDict(ParseHook).init(alloc),
        .precs = TokenDict(Prec).init(alloc),
    };
}
pub fn advanceN(self: *Parser, n: usize) ParseError!void {
    var i: usize = 0;
    while (i < n) _ = try self.advance();
}

/// Peek at the nth token without actually consuming anything.
pub fn nth(self: *Parser, n: usize) !?TokInfo {
    var res: ?Token = null;
    while (n >= self.cur.items.len) {
        const next = try self.lexer.scan();
        if (next) |item| {
            try self.cur.items.append(item);
        } else break;
    }
    if (self.cur.items.len > n) {
        res = self.cur.items[n];
    }

    return res;
}

/// Actual advancing will happen when we next call nth(...)
pub fn advance(self: *Parser) ParseError!?TokInfo {
    var res: ?TokInfo = null;
    if (self.toks.items.len > 0) {
        res = self.toks.orderedRemove(0);
    }
    return res;
}

pub fn peek(self: *Parser, matchable: Matchable) !bool {
    if (self.ended) {
        return false;
    }
    return try self.nth(0) != null and matchable.eql(try self.nth(0).?.tok, self);
}

pub fn peekAny(self: *Parser, matchables: []const Matchable) bool {
    for (matchables) |matchable| {
        if (self.peek(matchable)) return true;
    }
    return false;
}

/// Are these exact options going to happen in order?
pub fn peekAll(self: *Parser, matchables: []const Matchable) bool {
    for (matchables) |matchable, i| {
        if (try self.nth(i) == null or !matchable.eql(try self.nth(i).?.tok, self)) return false;
    }
    return true;
}

pub fn peekBlockStartingWith(self: *Parser, matchable: Matchable) bool {
    return self.peekAll(&[_]Matchable{
        .{ .token = .{ .newline = .{} } },
        .{ .token = .{ .indent = .{} } },
        .{ .any_access = .{} },
    });
}

pub fn tryMatch(self: *Parser, matchable: Matchable) ParseError!?Token {
    if (self.peek(matchable)) return self.advance();
    return null;
}

pub fn tryMatchAny(self: *Parser, matchables: []const Matchable) ParseError!?Token {
    if (self.peekAny(matchables)) return try self.advance();
    return null;
}

pub fn tryMatchAll(self: *Parser, matchables: []const Matchable) !bool {
    if (self.peekAll(matchables)) {
        try self.advanceN(matchables.len);
        return true;
    } else return false;
}

pub fn tryMatchBlockStartingWith(self: *Parser, matchable: Matchable) ParseError!?Token {
    if (self.peekBlockStartingWith(matchable)) {
        try self.advanceN(2);
        return try self.advance();
    } else return false;
}

pub fn match(self: *Parser, matchable: Matchable) ParseError!Token {
    const res = try self.tryMatch(matchable);
    if (res) |r| return r;
    return error.Unmatched;
}

pub fn matchAny(self: *Parser, matchables: []const Matchable) ParseError!Token {
    if (try self.tryMatchAny(matchables)) |m| return m;
    self.cache.dump();
    printf("\nExpected any of {any}, found {any}\n\n", .{ matchables, try self.nth(0) });
    return error.Unmatched;
}

pub fn matchAll(self: *Parser, matchables: []const Matchable) ParseError!void {
    if (!try self.tryMatchAll(matchables)) return error.Unmatched;
}

pub fn matchBlockStartingWith(self: *Parser, matchable: Matchable) ParseError!Token {
    if (try self.tryMatchBlockStartingWith(matchable)) |m| return m;
    return error.Unmatched;
}

pub fn parseExpr(self: *Parser) ParseError!Atom {
    return self.parseBinary(0);
}

pub fn parseBinary(self: *Parser, prec: usize) ParseError!Atom {
    var res = try self.descend(prec);
    while (try self.tryMatch(.{ .prec = .{ .binary = prec } })) |op| {
        res = try common.cmdID(op.id().?, &.{ res, try self.descend(prec) });
    }

    return res;
}

pub fn parseUnary(self: *Parser) ParseError!Atom {
    // printf("\nParsing unary\n", .{});
    var res = try self.parseHookable();
    const should_unary = all(&.{
        try self.nth(0) != null,
        !self.ended,
        !self.peekAny(STOPPERS[0..]),
        !self.peek(.{ .any_binary = .{} }),
        !self.peek(.{ .any_assg = .{} }),
    });
    if (should_unary) {
        // printf("\nParsing unary argument...\n", .{});
        res = try common.list(res, &.{try self.parseUnary()});
    }
    return res;
}

/// This is where the magic hooking for stuff like `if` happens.
pub fn parseHookable(self: *Parser) ParseError!Atom {
    var res: ?Atom = null;
    if (self.cur[0] != null) {
        if (self.hooks.get(self.cur[0].?.tok)) |hook| {
            res = try hook(self);
        }
    }
    if (res == null) res = try self.parseParticle();

    try self.parseTrailers(&res.?);
    return res.?;
}

// Valid example:
//from(foos)
//  .select: foo => foo.x
//  .where: x => x > 10
//  .order_by: x => x, #Desc

// TODO: WIP
// Parse any series of calls and accesses. At the very end can be either an access block or a `:` call.
pub fn parseTrailers(self: *Parser, res: *Atom) ParseError!void {
    const any_access = Matchable{ .any_access = .{} };
    var clean = false;
    while (!clean) {
        clean = true;
        if (self.peek(.{ .any_access = .{} })) {
            clean = false;
        }
        if (self.peekAny(&CALLERS) and self.cur[0].?.attached) {
            clean = false;
        }
    }

    if (try self.tryMatch(.{ .common = .{ .punc = .iColon } }) != null) {
        if (try self.tryMatch(.{ .seq_of = &.{
            .{ .token = .{ .newline = .{} } },
            .{ .token = .{ .indent = .{} } },
        } }) != null) {
            // TODO: Make this properly append the block as an argument if this was appended to a call
            res.* = try common.list(res.*, &.{try self.parseStmtSeq()});
            _ = try self.match(.{ .seq_of = &.{
                .{ .token = .{ .newline = .{} } },
                .{ .token = .{ .dedent = .{} } },
            } });
        } else res.* = try common.list(res.*, &.{try self.parseExpr()});
        self.ended = true;
    } else if (self.peekBlockStartingWith(any_access)) {
        try self.advanceN(2); // enter the block

        // Each line starts with an access
        while (true) {
            const accessor = try self.match(any_access);
            res.* = try common.cmdID(accessor.id().?, &.{ res.*, try self.parseParticle() });
            // Just recurse to deal with all the subsequent calls/accesses on that line
            try self.parseTrailers(res);

            if (self.peek(.{ .token = .dedent })) break;
        }

        _ = try self.match(.{ .token = .{ .dedent = .{} } });
        self.ended = true;
    }
}

/// Word, tag, tuple, block, or array
pub fn parseParticle(self: *Parser) ParseError!Atom {
    const res = try self.matchAny(&PARTICLES);
    switch (res) {
        .word => |id| return common.word(id),
        // .tag => |id| return Atom{ .tag = id },
        else => unreachable,
    }
}

/// (@block as stmt1 stmt2...)
pub fn parseStmtSeq(self: *Parser) ParseError!Atom {
    var res = try common.cmdCommon(.ibBlock, &.{});

    while (true) {
        printf("Top of seq loop\n", .{});
        while ((try self.tryMatch(.{ .token = .newline })) != null or (try self.tryMatch(.{ .common = .{ .punc = .iSemicolon } })) != null) {}

        // printf("Begin new stmtSeqPart\n", .{});
        try res.list.append(try self.parseExpr());

        if (self.ended) {
            self.ended = false;
        }

        if ((try self.tryMatch(.{ .token = .newline })) == null and (try self.tryMatch(.{ .common = .{ .punc = .iSemicolon } })) == null) {
            // printf("Found a {}, and so did not continue in the seq.\n", .{self.cur[0]});
            break;
        }

        if (self.cur[0] == null) {
            break;
        }
    }

    return res;
}

pub fn expected(self: *Parser, what: Matchable) ParseError!void {
    printf("\nError at {}: Expected ", .{self.cur[0].?.pos});
    switch (what) {
        .token => |tok| printf("{}", .{tok}),
        .common => |com| printf("{}", .{com.toStr()}),
        .prec => |com| {
            switch (com) {
                .access => printf("an access operator", .{}),
                .binary => |p| printf("a binary operator of precedence {}", .{p}),
            }
        },
        .any_binary => printf("any binary operator", .{}),
        .any_access => printf("any access operator", .{}),
        .any_word => printf("any word", .{}),
    }
    printf(", but found {}\n\n", .{self.cur[0].?.tok});
    return error.Expected;
}

// As a workaround for zigfmt's zealousness around samelining everything.
pub fn all(conds: []const bool) bool {
    for (conds) |cond| {
        if (!cond) return false;
    }
    return true;
}

pub fn precOf(self: Parser, tok: Token, attached: bool) Prec {
    if (self.precs.tryGetValue(tok)) |prec| return prec;

    return .{ .none = .{} };
}

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Internals                                                                  ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
fn descend(self: *Parser, prec: usize) ParseError!Atom {
    if (prec == self.precs.count()) {
        return self.parseUnary();
    } else {
        return self.parseBinary(prec + 1);
    }
}

// Things which can unambiguously stop the current binary/unary expression.
// `:`, `)`, `}`, `]`, `,`, `;`, and newline
pub const STOPPERS = [_]Matchable{
    .{ .common = .{ .word = .iColon } },
    .{ .common = .{ .word = .iRParen } },
    .{ .common = .{ .word = .iRBrace } },
    .{ .common = .{ .word = .iRBracket } },
    .{ .common = .{ .word = .iComma } },
    .{ .common = .{ .word = .iSemicolon } },
    .{ .token = .{ .newline = .{} } },
};

pub const ENDERS = [_]Matchable{
    .{ .token = .{ .newline = .{} } },
    .{ .common = .{ .word = .iSemicolon } },
};
pub const CALLERS = [_]Matchable{
    .{ .common = .{ .word = .iLParen } },
    .{ .common = .{ .word = .iLBracket } },
};

pub const PARTICLES = [_]Matchable{
    .{ .any_word = .{} },
};

pub const COMMON_PRECS = comptime [_][]const CPrec{
    &[_]CPrec{
        // Assg and Access
        .{ .tok = .{ .common = .iAccess }, .prec = .access },
        .{ .tok = .{ .common = .iAssg }, .prec = .assg },
        .{ .tok = .{ .common = .iSubAssg }, .prec = .assg },
        .{ .tok = .{ .common = .iAddAssg }, .prec = .assg },
        .{ .tok = .{ .common = .iMulAssg }, .prec = .assg },
        .{ .tok = .{ .common = .iDivAssg }, .prec = .assg },
    },

    // All binaries grouped by precedence
    &[_]CPrec{
        .{ .tok = .{ .common = .iLambda } },
    },
    &[_]CPrec{
        .{ .tok = .{ .common = .iOr } },
        .{ .tok = .{ .common = .iXor } },
    },
    &[_]CPrec{
        .{ .tok = .{ .common = .iAnd } },
    },
    &[_]CPrec{
        .{ .tok = .{ .common = .iEq } },
        .{ .tok = .{ .common = .iNeq } },
        .{ .tok = .{ .common = .iGtEq } },
        .{ .tok = .{ .common = .iLtEq } },
        .{ .tok = .{ .common = .iLt } },
        .{ .tok = .{ .common = .iGt } },
    },
    &[_]CPrec{
        .{ .tok = .{ .common = .iIn } },
        .{ .tok = .{ .common = .iNotIn } },
    },
    &[_]CPrec{
        .{ .tok = .{ .common = .iOpenRange } },
        .{ .tok = .{ .common = .iClosedRange } },
    },
    &[_]CPrec{
        .{ .tok = .{ .common = .iAdd } },
        .{ .tok = .{ .common = .iSub } },
    },
    &[_]CPrec{
        .{ .tok = .{ .common = .iMul } },
        .{ .tok = .{ .common = .iDiv } },
    },
};

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Tests                                                                      ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
test "basic parsing" {
    printf("\n\n", .{});
    try common.initCommons();
    var puncs: [Lexer.COMMON_CPUNCS.len]Lexer.Punc = undefined;
    for (Lexer.COMMON_CPUNCS) |cpunc, i|
        puncs[i] = try cpunc.resolve(&common.cache);
    var lexer = Lexer.init(
        @embedFile("../tests/parsing.li"),
        &puncs,
        &common.cache,
        common.alloc,
    );
    var parser = Parser.init(&lexer, common.alloc);
    for (COMMON_PRECS[0]) |cprec| {
        if (cprec.prec == .assg) {
            try parser.precs.put(try cprec.tok.resolve(&common.cache), .{ .assg = .{} });
        } else if (cprec.prec == .access) {
            try parser.precs.put(try cprec.tok.resolve(&common.cache), .{ .access = .{} });
        }
    }
    for (COMMON_PRECS[1..]) |cprecs, i| {
        for (cprecs) |cprec|
            try parser.precs.put(try cprec.tok.resolve(&common.cache), .{ .binary = i });
    }

    _ = try parser.advance();
    _ = try parser.advance();
    _ = try parser.advance();
    common.cache.dump();
    const root = try parser.parseStmtSeq();
    printf("Root: \n", .{});
    root.print(&common.cache, 0);
    printf("\n\n", .{});
}
