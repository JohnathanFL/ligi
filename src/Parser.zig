const std = @import("std");
const StrHashMap = std.StringHashMap;
const mem = std.mem;

const ast = @import("ast.zig");
const lex = @import("lexing.zig");

const str = []const u8;

const Parser = @This();

// Will eventually be a complete file-recursing parser
pub fn parse(input: str, alloc: *mem.Allocator) !*ast.Expr {
    var parser = try Parser.init(input, 0, alloc);
    return parser.parseBlock(false);
}

alloc: *mem.Allocator,
lexer: lex.Lexer,
cur: lex.Token,
/// Did moving to the current token take us over a linebreak?
newlined: bool,
// I'd say 1024 expressions is a reasonable starting place
exprs: std.SegmentedList(ast.Expr, 1024),

const Error = error{ Expected, Unimplemented, OutOfMemory, MutExc } || lex.Lexer.Error;

pub fn init(main: str, file: usize, alloc: *mem.Allocator) Error!Parser {
    var res = Parser{
        .alloc = alloc,
        .lexer = lex.Lexer.init(main, file, alloc),
        .exprs = std.SegmentedList(ast.Expr, 1024).init(alloc),
        .cur = undefined,
        .newlined = false,
    };
    // There's no way for the first
    res.cur = try res.lexer.lex();
    return res;
}

fn newExpr(self: *Parser, e: ast.Expr) *ast.Expr {
    self.exprs.push(e) catch unreachable;
    return self.exprs.at(self.exprs.len - 1);
}
fn newExprList(self: *Parser) ast.ExprList {
    return ast.ExprList.init(self.alloc);
}
fn newCall(self: *Parser, op: ast.Op, initial: ?[]*ast.Expr) *ast.Expr {
    var list = self.newExprList();
    if (initial) |i| list.appendSlice(i) catch unreachable;
    return self.newCallFrom(op, list);
}
fn newCallFrom(self: *Parser, op: ast.Op, initial: ast.ExprList) *ast.Expr {
    return self.newExpr(.{
        .Call = .{
            .op = op,
            .args = initial,
        },
    });
}

fn nextIs(self: *Parser, tag: lex.Tag) bool {
    return self.cur.tag == tag;
}
fn nextIsOne(self: *Parser, tags: []const lex.Tag) bool {
    for (tags) |tag| {
        if (self.cur.tag == tag) return true;
    }
    return false;
}
fn match(self: *Parser, tag: lex.Tag) Error!lex.Token {
    if (self.tryMatch(tag)) |t| {
        return t;
    } else {
        std.debug.warn("{}: Expected {}, but found {}\n", .{ self.cur.pos, tag, self.cur.tag });
        return error.Expected;
    }
}
fn tryMatch(self: *Parser, tag: lex.Tag) ?lex.Token {
    if (self.nextIs(tag)) {
        const prev = self.cur;
        // TODO: Fix this. This could cause problems when it comes to .UnTermChar/Str
        self.cur = self.lexer.lex() catch return null;
        self.newlined = prev.pos.line != self.cur.pos.line;
        //std.debug.warn("Matched {}, newlined is now {}\n", .{@tagName(prev.tag), self.newlined});
        return prev;
    } else {
        return null;
    }
}
fn tryMatchOne(self: *Parser, tags: []const lex.Tag) ?lex.Token {
    for (tags) |tag| {
        if (self.nextIs(tag)) return self.tryMatch(tag);
    }
    return null;
}
fn matchOne(self: *Parser, tags: []const lex.Tag) !lex.Token {
    if (self.tryMatchOne(tags)) |tok| {
        return tok;
    } else {
        std.debug.warn("{}: Expected one of {}, but found {}\n", .{ self.cur.pos, tags, self.cur.tag });
        return error.Expected;
    }
}

fn expected(self: *Parser, comptime what: str) Error {
    std.debug.warn("{}: Expected {}, but found a {}", .{ self.cur.pos, what, self.cur.tag });
    return error.Expected;
}

pub fn parseBlock(self: *Parser, comptime braces: bool) Error!*ast.Expr {
    var res = self.newExpr(.{
        .Block = .{
            .label = null,
            .body = std.ArrayList(*ast.Expr).init(self.alloc),
        },
    });
    var this = &res.Block;

    if (braces) _ = try self.match(.LBrace);
    const end: lex.Tag = if (braces) .RBrace else .EOF;

    while (!self.nextIs(end))
        try this.body.append(try self.parseStmt());

    _ = try self.match(end);

    return res;
}

pub fn parseStmt(self: *Parser) Error!*ast.Expr {
    switch (self.cur.tag) {
        .Let, .Var, .CVar, .Field, .Property, .Using, .Pub => return self.parseBindStmt(),
        // assert expr [',' str]
        .Assert => {
            _ = try self.match(.Assert);
            return self.newExpr(.{
                .Assert = .{
                    .expr = try self.parseExpr(),
                    .msg = if (self.tryMatch(.Comma)) |_| (try self.match(.Str)).str else null,
                },
            });
        },
        // break [label [',' expr]]
        .Break => {
            _ = try self.match(.Break);
            var lab: ?str = null;
            var val: ?*ast.Expr = null;
            if (self.tryMatch(.Label)) |label| {
                lab = label.str;
                if (self.tryMatch(.Comma) != null)
                    val = try self.parseExpr();
            }
            return self.newExpr(.{ .Break = .{ .label = lab, .val = val } });
        },
        .Return => {
            _ = try self.match(.Return);
            // A return can only come before a }, so anything else must be an expr
            const val = if (!self.nextIs(.RBrace)) try self.parseExpr() else null;
            return self.newExpr(.{ .Return = val });
        },
        .Defer => {
            _ = try self.match(.Defer);
            return self.newExpr(.{ .Defer = try self.parseExpr() });
        },
        else => return try self.parseAssg(),
    }
}
// [using|pub] bindop {bindloc [= expr]}
// TODO: If we go ahead with state-machine-like functions, could using and pub
// not be mutually exclusive?
pub fn parseBindStmt(self: *Parser) Error!*ast.Expr {
    const using = if (self.tryMatch(.Using)) |_| true else false;
    const used_pub = self.tryMatch(.Pub) != null;

    if (used_pub and using) {
        std.debug.warn("{}: `using` and `pub` are mutually exclusive!\n", .{self.cur.pos});
        return error.MutExc;
    }

    const op = if (self.tryMatchOne(&BIND_OPS)) |tok| tok.tag.toBindOp() else return self.expected("a bind spec");

    const res = self.newExpr(.{
        .Bind = .{
            .op = op,
            .using = using,
            // Fields and Enums are *always* public.
            // Only statics can be private.
            .level = if (used_pub) .Pub else switch (op) {
                .Field, .Enum => ast.BindLevel.Pub,
                else => .Priv,
            },
            .locs = std.ArrayList(ast.LocInit).init(self.alloc),
        },
    });
    const this = &res.Bind;

    while (true) {
        const loc = try self.parseBindLoc();
        const initExpr = if (self.tryMatch(.Assg) != null) try self.parseExpr() else null;
        try this.locs.append(.{ .loc = loc, .init = initExpr });

        if (self.tryMatch(.Comma) == null) break;
    }

    return res;
}
// name level [: expr] | '(' loc {',' loc} ')' [: expr]
// Thus, you are allowed to specify the bind types on the inside or outside of a bind tuple.
// Thus, you can unpack a tuple without specifying each inside variable's type while
// still having type safety (for functions and the like)
pub fn parseBindLoc(self: *Parser) Error!ast.BindLoc {
    if (self.tryMatch(.LParen) != null) {
        var locs = std.ArrayList(ast.BindLoc).init(self.alloc);
        // No trailing commas allowed for binds
        while (true) {
            try locs.append(try self.parseBindLoc());
            if (self.tryMatch(.Comma) == null) break;
        }

        _ = try self.match(.RParen);
        const ty = try self.parseTypeSpec(false);

        return ast.BindLoc{ .Tuple = .{ .locs = locs, .ty = ty } };
    } else {
        const name = (try self.match(.Word)).str;
        const ty = try self.parseTypeSpec(false);
        return ast.BindLoc{ .Named = .{ .name = name, .ty = ty } };
    }
}

// Just a special case of parseBin
pub fn parseAssg(self: *Parser) Error!*ast.Expr {
    const lhs = try self.parseExpr();
    if (self.tryMatchOne(&[_]lex.Tag{ .Assg, .AddAssg, .SubAssg, .MulAssg, .DivAssg })) |op| {
        const rhs = try self.parseExpr();
        return self.newCall(op.tag.toOp(.Binary), &[_]*ast.Expr{ lhs, rhs });
    } else return lhs;
}
// Just a sugar for a bin_expr
pub fn parseExpr(self: *Parser) Error!*ast.Expr {
    return self.parseBin(0);
}
pub fn parseBin(self: *Parser, comptime prec: usize) Error!*ast.Expr {
    var lhs = if (prec + 1 == BIN_OPS.len) try self.parseUna() else try self.parseBin(prec + 1);
    // TODO: Verify that this has left associativity
    while (self.tryMatchOne(BIN_OPS[prec])) |tok| {
        const rhs = if (prec + 1 == BIN_OPS.len) try self.parseUna() else try self.parseBin(prec + 1);
        lhs = self.newCall(tok.tag.toOp(.Binary), &[_]*ast.Expr{ lhs, rhs });
    }

    return lhs;
}
pub fn parseUna(self: *Parser) Error!*ast.Expr {
    if (self.tryMatchOne(&UNA_OPS)) |tok| {
        return self.newCall(tok.tag.toOp(.Unary), &[_]*ast.Expr{try self.parseUna()});
    } else {
        return try self.parseAtom();
    }
}

pub fn parseAtom(self: *Parser) Error!*ast.Expr {
    switch (self.cur.tag) {
        .If => return try self.parseIf(),
        .When => return try self.parseWhen(),
        .Loop => return try self.parseLoop(),
        .While, .For => return try self.parseWhileFor(),
        .Label, .LBrace => return try self.parseBlock(true),
        .Tag => return try self.parseEnumLit(),
        .Fn => return try self.parseFn(),
        else => return try self.parsePipeline(false),
    }
}
// '#' word [ tuple | compound ]
pub fn parseEnumLit(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.Tag);
    const tag = (try self.match(.Word)).str;
    const inner = if (!self.newlined) switch (self.cur.tag) {
        .LParen => try self.parseTuple(),
        .LBracket => try self.parseCompound(),
        else => null,
    } else null;

    return self.newExpr(.{
        .EnumLit = .{
            .tag = tag,
            .inner = inner,
        },
    });
}

// '[' [':' expr ':'] {'.'word ['=' expr] | expr}  ']'
pub fn parseCompound(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.LBracket);
    const as = try self.parseTypeSpec(true);

    // Note this technically means it's impossible to have an empty struct lit,
    // as it'll parse as an array lit
    if (self.nextIs(.Access)) { // struct lit
        var fields = ast.FieldList.init(self.alloc);
        while (self.tryMatch(.Access) != null) {
            const loc = try self.parseBindLoc();
            const val = if (self.tryMatch(.Assg)) |_| try self.parseExpr() else null;
            try fields.append(.{ .loc = loc, .val = val });
            // This combined with the while cond allows trailing commas
            if (self.tryMatch(.Comma) == null) break;
        }
        _ = try self.match(.RBracket);
        return self.newExpr(.{
            .Struct = .{
                .as = as,
                .fields = fields,
            },
        });
    } else { // array lit
        return self.newExpr(.{
            .Array = .{
                .as = as,
                .vals = try self.parseExprList(.RBracket), // matches the .RBracket
            },
        });
    }
}

pub fn parseTuple(self: *Parser) Error!*ast.Expr {
    //std.debug.warn("In parseTuple\n", .{});
    _ = try self.match(.LParen);
    var res = self.newExpr(.{
        .Tuple = .{
            .as = try self.parseTypeSpec(true),
            .vals = try self.parseExprList(.RParen),
        },
    });

    return res;
}

/// Parse a csv list of expressions, which may have a trailing comma
pub fn parseExprList(self: *Parser, closer: lex.Tag) Error!std.ArrayList(*ast.Expr) {
    var vals = std.ArrayList(*ast.Expr).init(self.alloc);
    while (!self.nextIs(closer)) {
        try vals.append(try self.parseExpr());
        if (self.tryMatch(.Comma) == null) break;
    }
    _ = try self.match(closer);
    return vals;
}

/// Either `:expr` or `:expr:`, depending on trailing_colon
pub fn parseTypeSpec(self: *Parser, comptime trailing_colon: bool) Error!?*ast.Expr {
    //std.debug.warn("In parseTypeSpec!\n", .{});
    if (self.tryMatch(.Colon) != null) {
        const res = try self.parseExpr();
        if (trailing_colon) _ = try self.match(.Colon);
        return res;
    } else return null;
}

// Parse anything that can be either block or => expr
pub fn parseThen(self: *Parser) Error!*ast.Expr {
    if (self.tryMatch(.Then)) |_| {
        // TODO: Should this be Assg or Expr?
        return try self.parseAssg();
    } else {
        return try self.parseBlock(true);
    }
}

pub fn parseIf(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.If);
    return self.newExpr(.{
        .If = .{
            .arms = val: {
                var arms = std.ArrayList(ast.IfArm).init(self.alloc);
                while (true) {
                    const cond = try self.parseExpr();
                    const capt = if (self.tryMatch(.StoreIn)) |_| try self.parseBindLoc() else null;
                    const then = try self.parseThen();
                    try arms.append(.{ .cond = cond, .capt = capt, .then = then });

                    if (self.tryMatch(.ElIf) == null) break;
                }
                break :val arms;
            },
            .default = if (self.tryMatch(.Else) == null) null else try self.parseThen(),
            .finally = if (self.tryMatch(.Finally) == null) null else try self.parseThen(),
        },
    });
}
pub fn parseWhen(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.When);
    return self.newExpr(.{
        .When = .{
            .expr = try self.parseExpr(),
            .arms = val: {
                var arms = std.ArrayList(ast.WhenArm).init(self.alloc);
                while (self.tryMatch(.Is)) |_| {
                    const op = if (self.tryMatchOne(&IS_OPS)) |tok| tok.tag.toOp(.Binary) else .Eq;
                    const val = try self.parseExpr();
                    const capt = if (self.tryMatch(.StoreIn)) |_| try self.parseBindLoc() else null;
                    const then = try self.parseThen();
                    try arms.append(.{ .op = op, .val = val, .capt = capt, .then = then });
                }
                break :val arms;
            },
            .default = if (self.tryMatch(.Else) == null) null else try self.parseThen(),
            .finally = if (self.tryMatch(.Finally) == null) null else try self.parseThen(),
        },
    });
}
pub fn parseLoop(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.Loop);
    return self.newExpr(.{
        .Loop = .{
            .expr = null,
            .op = .NOP,
            .capt = null,
            .counter = if (self.tryMatch(.StoreIn)) |_| try self.parseBindLoc() else null,
            .body = try self.parseThen(),
            .finally = null,
        },
    });
}
pub fn parseWhileFor(self: *Parser) Error!*ast.Expr {
    const op = if (self.tryMatch(.For)) |_| ast.LoopOp.For else if (self.tryMatch(.While)) |_| ast.LoopOp.While else return self.expected("while or for");
    const expr = try self.parseExpr();

    var capt: ?ast.BindLoc = null;
    var counter: ?ast.BindLoc = null;
    if (self.tryMatch(.StoreIn)) |_| {
        capt = try self.parseBindLoc();
        if (self.tryMatch(.Comma)) |_| counter = try self.parseBindLoc();
    }
    const body = try self.parseThen();
    const finally = if (self.tryMatch(.Finally)) |_| try self.parseBlock(true) else null;
    return self.newExpr(.{
        .Loop = .{
            .op = op,
            .expr = expr,
            .capt = capt,
            .counter = counter,
            .body = body,
            .finally = finally,
        },
    });
}

// Lower than unary
// access { `::` access }
// If restricted, then accesses are restricted
// Parses like so:
// a.b::d.f()::e.g()
// ->
// Call.Pipe(a.b, d.f(), e.g())
// And will interpret like so:
// e.g(d.f(a.b()))
pub fn parsePipeline(self: *Parser, comptime restricted: bool) Error!*ast.Expr {
    var res = try self.parseAccess(restricted);

    if (self.nextIs(.Pipe)) {
        res = self.newCall(.Pipeline, &[_]*ast.Expr{res});
        const pipes = &res.Call.args;
        while (self.tryMatch(.Pipe)) |_| {
            try pipes.append(try self.parseAccess(restricted));
        }
    }

    return res;
}

// Can we do a foo() or a foo[]?
// ( or [ must be on the same line as foo
// foo must have already been matched
pub fn canCall(self: *Parser) bool {
    if (self.nextIs(.LParen) or self.nextIs(.LBracket)) {
        return !self.newlined;
    } else return false;
}

// (word|str|tuple|compound) ~ {'.' ~ (swizzle_group | word ) | call | index}
// If restricted, then the first can only be a word, not a tuple or compound
// // swizzle_group results in a call to a restricted parsePipeline inside
// The entire access is parsed into a linear arraylist of args to a call (.Op=.Call)
// Calls/indexes are just more "accesses" to the AST
pub fn parseAccess(self: *Parser, comptime restricted: bool) Error!*ast.Expr {
    //std.debug.warn("In parseAccess, next is {}\n", .{self.cur});
    var res = if (restricted) try self.parseWord() else switch (self.cur.tag) {
        .Word => try self.parseWord(),
        .LParen => try self.parseTuple(),
        .LBracket => try self.parseCompound(),
        .Str => try self.parseStr(),
        else => return self.expected("An access op"),
    };

    if (self.nextIs(.Access) or self.canCall()) {
        res = self.newCall(.Access, &[_]*ast.Expr{res});
        const path = &res.Call.args;
        while (self.nextIs(.Access) or self.canCall()) {
            const accesser = (try self.matchOne(&ACCESS_OPS)).tag;
            switch (accesser) {
                .Access => {
                    if (self.nextIs(.Word)) {
                        try path.append(try self.parseWord());
                    } else if (self.tryMatch(.LParen)) |_| {
                        var els = self.newExprList();
                        while (!self.nextIs(.RParen)) {
                            try els.append(try self.parsePipeline(true));
                            if (self.tryMatch(.Comma) == null) break;
                        }
                        _ = try self.match(.RParen);
                        try path.append(self.newExpr(.{
                            .Tuple = .{
                                .as = null,
                                .vals = els,
                            },
                        }));
                    } else return self.expected("either a word or a swizzle group");
                },
                .LParen, .RParen => {
                    const op = if (accesser == .LParen) ast.Op.Call else .Index;
                    const call = self.newCallFrom(.Call, try self.parseExprList(if (accesser == .LParen) .RParen else .RBracket));
                    try path.append(call);
                },
                else => unreachable,
            }
        }
    }
    return res;
}

pub fn parseWord(self: *Parser) Error!*ast.Expr {
    const word = try self.match(.Word);
    return self.newExpr(.{ .Word = word.str });
}
pub fn parseStr(self: *Parser) Error!*ast.Expr {
    const tok = try self.match(.Str);
    return self.newExpr(.{ .Str = tok.str });
}

pub fn parseFn(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.Fn);
    var args = std.ArrayList(ast.BindLoc).init(self.alloc);
    while (!self.nextIsOne(&[_]lex.Tag{ .StoreIn, .Then })) {
        try args.append(try self.parseBindLoc());
        if (self.tryMatch(.Comma) == null) break;
    }

    // Functions are either of the form
    // fn arg1, arg2, arg3 -> res = body
    // or
    // fn args1, arg2, arg3 => body
    // which is equivalent to
    // fn args1, arg2, arg3 -> _: void = body
    // (where => is .Then)
    var ret: ast.BindLoc = undefined;
    var body: ?*ast.Expr = null;
    if (self.tryMatch(.Then)) |_| {
        // A => function must return void
        // TODO: Should we find a way to relax this rule? Maybe => type = body?
        ret = .{
            .Named = .{
                .name = "_",
                .ty = self.newExpr(.{ .Word = "void" }),
            },
        };
        body = try self.parseExpr();
    } else if (self.tryMatch(.StoreIn)) |_| {
        ret = try self.parseBindLoc();
        if (self.tryMatch(.Assg)) |_| body = try self.parseExpr();
    } else return self.expected("either => or ->");

    return self.newExpr(.{
        .Func = .{
            .args = args,
            .ret = ret,
            .body = body,
        },
    });
}

const ACCESS_OPS = [_]lex.Tag{ .LParen, .LBracket, .Access };

const BIN_OPS = .{
    &[_]lex.Tag{ .Eq, .NotEq, .Gt, .Lt, .GtEq, .LtEq, .Spaceship },
    &[_]lex.Tag{ .Or, .Xor },
    &[_]lex.Tag{.And},
    &[_]lex.Tag{ .In, .NotIn },
    &[_]lex.Tag{ .OpenRange, .ClosedRange },
    &[_]lex.Tag{ .Add, .Sub },
    &[_]lex.Tag{ .Mul, .Div, .Mod },
    &[_]lex.Tag{ .BitOr, .BitAnd, .BitXor },
};
const UNA_OPS = [_]lex.Tag{
    .Sub,      .Enum,   .Mul,
    .Struct,   .Ref,    .Slice,
    .Array,    .Const,  .Comptime,
    .BitNot,   .Not,    .Opt,
    .Pure,     .Inline, .Overload,
    .Property,
};

/// Operators allowed to be used with `when...is OP expr`
const IS_OPS = [_]lex.Tag{
    .Eq, .NotEq, .In, .NotIn,
};

const BIND_OPS = [_]lex.Tag{
    .Let, .Var, .CVar, .Enum, .Field,
};
