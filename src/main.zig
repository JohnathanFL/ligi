const std = @import("std");
const testing = std.testing;
const expect = testing.expect;

const Tag = enum {
    Assign, Or, And, Pipe, Xor, BitAnd, Equal, NotEqual, Less, 
    Greater, LessEq, GreaterEq, Shl, Shr, Add, Sub, BitNot, Not, 
    Inc, Dec, IncNow, DecNow, LParen, RParen, LBracket, RBracket, 
    Dot, DotOpt, ID, IntLit, BoolLit, FloatLit, CharLit, StringLit,
    NullLit, If, While, For, 
};

const Expr = union(enum) {
    IfStmt: IfStmt,
    WhileStmt: WhileStmt,
    ForStmt: ForStmt,
    // All operators are stripped down to function calls for simplicity
    Call: Call
};

const StmtBody = std.ArrayList(*Stmt);

const ID = []const u8;

const IfStmt = struct {
    cond: *Expr,
    captID: ?ID,
    body: StmtBody,
    otherwise: *Expr,
};

const WhileStmt = struct {
    cond: *Expr,
    cont: ?*Stmt,
    captID: ?ID,
    body: StmtBody,
    otherwise: *Expr
};

const ForStmt = struct {
    range: *Expr,
    captID: ?ID,
    body: StmtBody
};

const Call = struct {
    func: []const u8,
    args: std.ArrayList(*Expr)
};