// The idea is essentially to use AST as the lingua franca for the compiler.
// The AST should have no dependencies on anything else, and exists only to
// express code itself

const std = @import("std");
const str = []const u8;
const types = @import("types.zig");
const ArrayList = std.ArrayList;
const StrHashMap = std.StringHashMap;

pub const Op = enum {
    Assg,
    AddAssg,
    SubAssg,
    MulAssg,
    DivAssg,

    // Bin
    Eq,
    NotEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Spaceship,
    Or,
    Xor,
    And,
    In,
    NotIn,
    OpenRange,
    ClosedRange,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitOr,
    BitAnd,
    BitXor,

    // Una
    Neg,
    Enum,
    Ptr,
    Struct,
    Ref,
    Slice,
    Array,
    Const,
    Comptime,
    BitNot,
    Not,
    Opt,
    Pure,
    Inline,
    Overload,
    Property,

    Call,
    Index,

    Access,
    Pipeline, // Only exists in pre-eval tree
};

pub const BindOp = enum {
    Let,
    Var,
    CVar,
    Field,
    Enum,
    Alias,
};
pub const BindLevel = enum { Pub, Priv };
pub const BindLoc = union(enum) {
    Tuple: struct { locs: ArrayList(BindLoc), ty: ?*Expr },
    Named: struct { name: str, ty: ?*Expr },
};

pub const LocInit = struct { loc: BindLoc, init: ?*Expr };
pub const IfArm = struct { cond: *Expr, capt: ?BindLoc, then: *Expr };
// .op must be one of .Eq, .NotEq, .Gt, .Lt, .GtEq, .LtEq, .NotIn, .In
pub const WhenArm = struct { op: Op, val: *Expr, capt: ?BindLoc, then: *Expr };
pub const LoopOp = enum { NOP, While, For };
/// If the val is null, then the value is taken from context from the name of the bindloc
pub const FieldList = ArrayList(struct { loc: BindLoc, val: ?*Expr });

pub const ExprList = ArrayList(*Expr);

pub const Block = struct { label: ?str, body: ExprList };
pub const EnumLit = struct { label: str, inner: ?*Expr };
pub const Tuple = struct { as: ?*Expr, vals: ExprList };
pub const Struct = struct { as: ?*Expr, fields: FieldList };
pub const Array = struct { as: ?*Expr, vals: ExprList };
pub const Bind = struct {
    using: bool, // implicit defer
    level: BindLevel,
    op: BindOp,
    locs: ArrayList(LocInit),
};
pub const Break = struct { label: ?str, val: ?*Expr };
pub const Assert = struct { expr: *Expr, msg: ?str };
// This includes `.`, `::`, `()`, and `[]`
pub const Call = struct { op: Op, args: ExprList };
pub const Func = struct {
    args: ArrayList(BindLoc),
    // For a void function (fn->{}), it's #sink("void")
    ret: BindLoc,
    // What gets assigned to ret's loc
    // If null, then this is a function type, not a function definition
    body: ?*Expr,
};
pub const Macro = struct {
    args: ArrayList(BindLoc),
    body: ?*Expr,
};
pub const If = struct {
    arms: ArrayList(IfArm),
    default: ?*Expr,
    finally: ?*Expr,
};
pub const When = struct {
    expr: *Expr,
    arms: ArrayList(WhenArm),
    default: ?*Expr,
    finally: ?*Expr,
};
pub const Loop = struct {
    expr: ?*Expr,
    // How do we interpret .expr?
    op: LoopOp, capt: ?BindLoc, counter: ?BindLoc, body: *Expr,
    // Always null for inf loop
    finally: ?*Expr
};

// By convention, Exprs will never be stored by value.
// By convention, everything else is stored by value (in ArrayLists, if need be)
// TODO: Refactor the inner structs out into proper named structs
pub const Expr = union(enum) {
    // Specials
    NOP: void,
    /// TODO: The `$` operator. Takes the value an "expands" it into the AST
    Expansion: *Expr,

    // Values
    Word: str,
    EnumLit: EnumLit,
    Tuple: Tuple,
    Struct: Struct,
    Array: Array,
    Str: str,

    // Statements
    Continue: ?str,
    Block: Block,
    Bind: Bind,
    Break: Break,
    Return: ?*Expr,
    Assert: Assert,

    // Control flow
    Call: Call, // includes operators AND ()/[]
    If: If,
    When: When,
    Loop: Loop,

    ////
    // Only in post-eval tree
    ////
    Type: types.TypeID,
    FuncID: usize, // compiled form

    ////
    // Only in pre-eval tree
    ////
    Macro: Macro,
    Func: Func, // uncompiled form
    Defer: *Expr,
    Use: *Expr, // Expr must be an Access
};
