// The idea is essentially to use AST as the lingua franca for the compiler.
// The AST should have no dependencies on anything else, and exists only to
// express code itself

const std = @import("std");
const str = []const u8;
const ArrayList = std.ArrayList;
const StrHashMap = std.StringHashMap;

pub const Op = enum {

  Assg, AddAssg, SubAssg, MulAssg, DivAssg,
  
  // Bin
  Eq, NotEq, Gt, Lt, GtEq, LtEq, Spaceship,
  Or, Xor,
  And,
  In, NotIn,
  OpenRange, ClosedRange,
  Add, Sub,
  Mul, Div, Mod,
  BitOr, BitAnd, BitXor,

  // Una
  Neg, Enum, Ptr,
  Struct, Ref, Slice, Array, Const, Comptime,
  BitNot, Not, Opt, Pure, Inline, Overload, Property,
};

pub const BindOp = enum {
  Let, Var, CVar, Field, Enum, Alias,
};
pub const BindLevel = enum {
  Pub, ReadOnly, Priv,
};

pub const BindLoc = union(enum) {
  Tuple: struct {
    locs: ArrayList(BindLoc),
    ty: ?*Expr,
  },
  Named: struct {
    name: str,
    access: BindLevel,
    ty: ?*Expr
  }
};

pub const LocInit = struct {loc: BindLoc, init: ?*Expr};
pub const CallTarget = union(enum) { Op: Op, Func: *Expr };

pub const SwizzleOp = union(enum) {
  Name: str,
  Descend: void, // . (without deref or addr)
  Pipe: void,
  Call: struct {
    // Was this [], instead of ()?
    index: bool,
    args: ArrayList(*Expr)
  },
  GroupBegin: void, // (
  GroupEnd: void, // )
  
};
pub const IfArm = struct { cond: *Expr, capt: ?BindLoc, then: *Expr };
// .op must be one of .Eq, .NotEq, .Gt, .Lt, .GtEq, .LtEq, .NotIn, .In
pub const WhenArm = struct { op: Op, val: *Expr, capt: ?BindLoc, then: *Expr };

// By convention, Exprs will never be stored by value.
// By convention, everything else is stored by value (in ArrayLists, if need be)
// TODO: Refactor the inner structs out into proper named structs
pub const Expr = union(enum) {
  NOP: void,
  Word: str,
  Block: struct { label: ?str, body: ArrayList(*Expr) },
  EnumLit: struct { tag: str, inner: ?*Expr },
  Tuple: struct { as: ?*Expr, vals: ArrayList(*Expr) },
  // If the field's value is null, then the field name is used to look up the value from scope
  Struct: struct { as: ?*Expr, fields: StrHashMap(?*Expr) },
  Array: struct { as: ?*Expr, vals: ArrayList(*Expr) },
  
  Bind: struct {
    op: BindOp,
    using: bool,
    locs: ArrayList(LocInit),
  },
  Break: struct { label: ?str, val: ?*Expr },
  Return:?*Expr,
  Assert: *Expr,
  /// TODO: Why not rework the Block to have a list of deferred statements?
  Defer: *Expr,

  Access: struct {
    // word | tuple | compound
    what: *Expr,
    // The swizzle system is designed so it can be parsed directly left->right
    // as needed, so we'll just store it like that
    access: ArrayList(SwizzleOp),
  },

  Call: struct {
    // What do we call?
    what: CallTarget,
    // What do we call it with?
    args: ArrayList(*Expr),
  },
  
  Func: struct {
    args: ArrayList(BindLoc),
    // For a void function (fn->{}), it's #sink("void")
    ret: BindLoc,
    // What gets assigned to ret's loc
    body: *Expr,
  },
  If: struct {
    arms: ArrayList(IfArm),
    default: ?*Expr,
    finally: ?*Expr,
  },
  When: struct {
    expr: *Expr,
    arms: ArrayList(WhenArm),
    default: ?*Expr,
    finally: ?*Expr,
  },
  Loop: struct {
    expr: ?*Expr,
    // How do we interpret .expr?
    cond: enum { NOP, While, For },
    capt: ?BindLoc,
    counter: ?BindLoc,
    body: *Expr,
    // Always null for inf loop
    finally: ?*Expr
  }
};
