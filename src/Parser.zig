pub const Parser = @This();

const std = @import("std");
const Alloc = std.mem.Allocator;
const Dict = std.AutoHashMap;

const StrCache = @import("StrCache.zig");
const StrID = StrCache.StrID;

const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");
const common = @import("common_ast.zig");

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Accompanying Types                                                         ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
pub const ParseHook = fn (*Parser) !Atom;

pub const Prec = union(enum) {
    binary: usize,
    access: void,

    pub fn eql(p1: Prec, p2: Prec) bool {
        return switch (p1) {
            .binary => p1.binary == p2.binary,
            .access => p2 == .access,
        };
    }
};
pub const Matchable = union(enum) {
    token: Token,
    common: union(enum) {
        word: CommonID,
        sigil: CommonID,
        punc: CommonID,
    },
    prec: Prec,
    /// Anything that can be a binary operator
    any_binary: void,
    /// Anything that can be an access operator
    any_access: void,
    any_word: void,
    any_sigil: void,
    any_tag: void,

    pub fn eql(self: Matchable, tok: Token, parser: *const Parser) bool {
        switch (self) {
            .token => |tok| return tok.eql(self),
            .common => |com| switch (com) {
                .word => |id| return tok.eql(.{ .word = common.ids_ar[id] }),
                .sigil => |id| return tok.eql(.{ .sigil = common.ids_ar[id] }),
                .punc => |id| return tok.eql(.{ .punc = common.ids_ar[id] }),
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
            .any_word => tok == .word,
            .any_sigil => tok == .sigil,
            .any_tag => tok == .tag,
        }
    }
};

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Fields                                                                     ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
lexer: *Lexer,
cur: [3]?Token = .{ null, null },
cache: *StrCache,

hooks: Dict(Token, ParseHook),
precs: Dict(Token, Prec),

/// Set if something unambiguously ended the current expression.
/// Parsers like tuples/blocks/etc should unset this flag before moving to the
/// next value in the chain.
/// For example, control structures should usually set this.
ended: bool,

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Interface                                                                  ║
// ╚═══════════════════════════════════════════════════════════════════════════╝

/// Must be `.advance`d twice before use.
pub fn init(lexer: *Lexer, alloc: *Alloc) Parser {
    return .{
        .lexer = lexer,
        .cache = lexer.cache,
        .cur = null,
        .next = null,
        .hooks = Dict(Token, ParseHook).init(alloc),
        .precs = Dict(Token, Prec).init(alloc),
    };
}

pub fn advance(self: *Parser) !?Token {
    const res = self.cur[0];
    self.cur[0] = self.cur[1];
    self.cur[1] = self.cur[2];
    self.cur[2] = try self.lexer.scan();
    if (self.cur[0] != null and self.cur[0] == .newline) {
        self.ended = true;
    }
    return res;
}

pub fn peek(self: *Parser, matchable: Matchable) bool {
    if (self.ended) return false;
    return self.cur[0] != null and matchable.eql(self.cur[0].?, self.cache);
}

pub fn peekAny(self: *Parser, matchables: []const Matchable) bool {
    for (matchables) |matchable| {
        if (self.peek(matchable)) return true;
    }
    return false;
}

pub fn peekAll(self: *Parser, matchables: []const Matchable) bool {
    for (matchables) |matchable, i| {
        if (!matchable.eql(self.cur[i])) return false;
    }
    return true;
}

pub fn peekBlockStartingWith(self: *Parser, matchable: Matchable) bool {
    return self.peekAll(&.{
        .{ .token = .{ .newline = .{} } },
        .{ .token = .{ .indent = .{} } },
        .{ .any_access = .{} },
    });
}

pub fn tryMatch(self: *Parser, matchable: Matchable) !?Token {
    if (self.peek(matchable)) return self.advance();
    return null;
}

pub fn match(self: *Parser, matchable: Matchable) !Token {
    const res = self.tryMatch(matchable);
    if (res) |r| return r;
    return error.Unmatched;
}

pub fn parseExpr(self: *Parser) !Atom {
    return self.parseBinary(0);
}

pub fn parseBinary(self: *Parser, prec: usize) !Atom {
    var res = try self.descend(prec);
    while (try self.tryMatch(.{ .prec = .{ .binary = prec } })) |op| {
        res = try common.list(op.id(), &.{ res, try self.descend() });
    }

    return res;
}

pub fn parseUnary(self: *Parser) !Atom {
    var res = try self.parseHookable();
    if (!self.peek(&STOPPERS)) {
        res = try common.list(res, &.{self.parseUnary()});
    }
    return res;
}

/// This is where the magic hooking for stuff like `if` happens.
pub fn parseHookable(self: *Parser) !Atom {
    var res = if (self.hooks.get(self.cur[0].?)) |hook| try hook(self) else try self.parseParticle();
    const any_access = Matchable{.any_access};
}

pub fn parseTrailers(self: *Parser, res: *Atom) !void {
    if (self.peek(any_access) or self.peekBlockStartingWith(any_access)) {
        try self.parseAccess(&res);
    }
    if(self.peekAny())
}

//from(foos)
//  .select: foo => foo.x
//  .where: x => x > 10
//  .order_by: x => x, #Desc

/// Parse the access binary expr level.
pub fn parseAccess(self: *Parser, res: *Atom) !void {
    if (self.peek(.{.any_access})) {}
    if (self.peek(.{ .common = .iColon })) {
        // A `:` call always ends the current statement
        self.ended = true;
    } else if (self.peekBlockStartingWith(.{.any_access})) {
        _ = try self.advance();
        _ = try self.advance();
        // we're now in a new block with an access waiting for us in cur[0]

        // End block
        try self.match(.{ .token = .{.dedent} });
    }
}

/// Word, sigil, or tag
pub fn parseParticle(self: *Parser) !Atom {
    const res = try self.match(&PARTICLES);
    switch (res) {
        .word, .sigil => |id| return common.word(id),
        .tag => |id| return Atom{ .tag = id },
        else => unreachable,
    }
}

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Internals                                                                  ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
fn descend(self: *Parser, prec: usize) !Atom {
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
pub const CALLERS = [_]Matchable{
    .{ .common = .{ .word = .iLParen } },
    .{ .common = .{ .word = .iLBracket } },
};

pub const PARTICLES = [_]Matchable{
    .{ .any_word = .{} },
    .{ .any_sigil = .{} },
    .{ .any_tag = .{} },
};

// ╔═══════════════════════════════════════════════════════════════════════════╗
// ║Tests                                                                      ║
// ╚═══════════════════════════════════════════════════════════════════════════╝
