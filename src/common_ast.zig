// Utilities/statics common to all AST usage

const std = @import("std");
const clamp = std.math.clamp;
const Alloc = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const List = std.ArrayList;
const Dictionary = std.AutoHashMap;
const mem = std.mem;
const printf = std.debug.warn;

const assert = std.debug.assert;

const StrCache = @import("StrCache.zig");
const Str = StrCache.Str;
const StrID = StrCache.StrID;

const ast = @import("ast.zig");
const Atom = ast.Atom;
pub const alloc = ast.alloc;

pub fn initCommons() !void {
    cache = StrCache.init(alloc);
    for (ids_ar) |*id, i| {
        const en = @intToEnum(CommonID, i);
        const str = commonIDToStr(en);
        const newId = try cache.intern(str);
        id.* = newId;
        // printf("Interned common {} ({s}) as ID {} index {}\n", .{ en, str, newId, i });
    }
}

pub const ASTError = error{
    OutOfMemory,
};

pub fn resolve(id: StrID) Str {
    return cache.resolve(id);
}

pub fn resolveCommon(cm: CommonID) StrID {
    return ids_ar[@enumToInt(cm)];
}

pub fn intern(str: Str) !StrID {
    return cache.intern(str);
}

pub fn list(cmd: Atom, children: ?[]const Atom) !Atom {
    var l = List(Atom).init(alloc);
    try l.append(cmd);
    if (children) |childs| try l.appendSlice(childs);
    return Atom{ .list = l };
}

pub fn cmdID(cmd: StrID, children: ?[]const Atom) !Atom {
    var l = List(Atom).init(alloc);
    try l.append(.{ .word = cmd });
    if (children) |childs| try l.appendSlice(childs);
    return Atom{ .list = l };
}

pub fn cmdCommon(cmd: CommonID, children: ?[]const Atom) !Atom {
    var l = List(Atom).init(alloc);
    // const id =
    try l.append(.{ .word = ids_ar[@enumToInt(cmd)] });
    if (children) |childs| try l.appendSlice(childs);
    // printf("Resolved cmdCommon {} as {s}({})\n", .{cmd, })
    return Atom{ .list = l };
}

pub fn word(w: StrID) Atom {
    return .{ .word = w };
}

pub var cache: StrCache = undefined;
pub var ids_ar: [std.meta.fieldNames(CommonID).len]StrID = undefined;

pub const CommonID = enum(StrID) { iColon = 0, iComma, iLBrace, iRBrace, iLParen, iRParen, iLBracket, iRBracket, iStoreIn, iAssg, iAddAssg, iSubAssg, iMulAssg, iDivAssg, iLambda, iSpaceship, iAnd, iOr, iXor, iEq, iNeq, iLt, iGt, iLtEq, iGtEq, iIn, iNotIn, iAdd, iSub, iMul, iPtr, iDiv, iMod, iAccess, iAccessPipe, iOptAccess, iOptAccessPipe, iFn, iMacro, iLet, iVar, iCVar, iField, iCase, iIf, iElIf, iWhen, iWhile, iLoop, iFor, iElse, iFinally, iIs, iAssert, iExpect, iBreak, iReturn, iDelete, iContinue, iSink, ibBlock, ibTuple, ibArray, ibAt, ibArm, ibElse, ibFinally, ibIf, ibWhen, ibWhile, ibFor, ibLoop, ibBind, ibFunc, iSpread, iSemicolon, iOpenRange, iClosedRange };

pub fn commonIDToStr(id: CommonID) Str {
    return switch (id) {
        .iSpread => "...",

        .iOpenRange => "..",
        .iClosedRange => "..=",

        .iColon => ":",
        .iSemicolon => ";",
        .iComma => ",",
        .iLBrace => "{",
        .iRBrace => "}",
        .iLParen => "(",
        .iRParen => ")",
        .iLBracket => "[",
        .iRBracket => "]",
        .iStoreIn => "->",

        .iAssg => "=",
        .iAddAssg => "+=",
        .iSubAssg => "-=",
        .iMulAssg => "*=",
        .iDivAssg => "/=",

        .iLambda => "=>",

        .iSpaceship => "<=>",

        .iAnd => "and",
        .iOr => "or",
        .iXor => "xor",

        .iEq => "==",
        .iNeq => "!=",
        .iLt => "<",
        .iGt => ">",
        .iLtEq => "<=",
        .iGtEq => ">=",

        .iIn => "in",
        .iNotIn => "notin",

        .iAdd => "+",
        .iSub => "-",
        .iMul => "*",
        .iPtr => "*",
        .iDiv => "/",
        .iMod => "mod",

        .iAccess => ".",
        .iAccessPipe => ".>",
        .iOptAccess => ".?",
        .iOptAccessPipe => ".?>",

        .iFn => "fn",
        .iMacro => "macro",

        .iLet => "let",
        .iVar => "var",
        .iCVar => "cvar",
        .iField => "field",
        .iCase => "case",

        .iIf => "if",
        .iElIf => "elif",
        .iWhen => "when",
        .iWhile => "while",
        .iLoop => "loop",
        .iFor => "for",

        .iElse => "else",
        .iFinally => "finally",
        .iIs => "is",

        .iAssert => "assert",
        .iExpect => "expect",
        .iBreak => "break",
        .iReturn => "return",
        .iDelete => "delete",
        .iContinue => "continue",

        .iSink => "_",

        .ibBlock => "@block",
        .ibTuple => "@tuple",
        .ibArray => "@array",
        .ibAt => "@at",
        .ibArm => "@arm",
        .ibElse => "@else",
        .ibFinally => "@finally",
        .ibIf => "@if",
        .ibWhen => "@when",
        .ibWhile => "@while",
        .ibFor => "@for",
        .ibLoop => "@loop",
        .ibBind => "@bind",

        .ibFunc => "@func",
    };
}
