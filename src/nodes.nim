import options
import tables

import tokens

# Using pure refs instead of Options for slightly cleaner code (maybe)
# I'll try to annotate nillable with #?[, condition for nil]
# (And yeah, it is somewhat ironic to use this style while building a language without implicit nils)
type
  Command*{.pure.} = enum
    # Those mapped directly to a token
    Access, Add, AddAssign, Alias, And,
    Array, AShr, Assert, Assign, BitAnd,
    BitAndAssign, BitNot, BitNotAssign, BitOr, BitOrAssign,
    BitXor, BitXorAssign, Break, Char, ClosedRange,
    Comptime, Const, CVar, Div, DivAssign,
    DoWhile, Equal, Field,
    For, Greater, GreaterEq, If, In,
    Inline, Int, Less, LessEq, Let,
    Loop, Mod, Mul, MulAssign, Not,
    NotEqual, NotIn, Null, OpenRange, Optional,
    Or, Property, Pure, Return, Shl,
    ShlAssign, Shr, ShrAssign, Sink, Slice,
    Spaceship, String, StructDef, Sub, SubAssign,
    Symbol Test, Undef, Use, Var,
    Void, While, Xor,

    # Those...not
    Tuple, Enum, EnumDef,
  Stmt* = ref object of RootObj
    pos*: tuple[line: uint, col: uint]
    # Remember that this is present in BinExpr/etc
    cmd*: Command
  Assert* = ref object of Stmt
    expr*: Expr
  Break* = ref object of Stmt
    val*: Expr #?
    # label.len == 0 just means the next block up
    label*: string #?
  Return* = ref object of Stmt
    val*: Expr #?

  BindLoc* = ref object of RootObj
  BindTuple* = ref object of BindLoc
    locs*: seq[BindLoc]
  BindSym* = ref object of BindLoc
    loc*: string
    ty*: Expr
    isPub*: bool
  # We want to just discard any writes to this.
  # Has to be its own thing rather than Symbol(_) to avoid constant
  # 'is sym == _' checks
  # Note this technically means `let _ = 10` is just as valid as `_ = 10`
  BindSink* = ref object of BindLoc
  Bind* = ref object of Stmt
    # We know the bind type from Stmt.cmd
    loc*: BindLoc
    init*: Expr #?

  # Anything that can yield a value
  Expr* = ref object of Stmt
  Tuple* = ref object of Expr
    children*: seq[Expr]
  Sink* = ref object of Expr
  Null* = ref object of Expr

  # Can yield a value from either a break or an unconsumed value
  Block* = ref object of Expr
    children*: seq[Stmt]
    # label.len == 0 just means unlabeled
    label*: string 

  # The atoms
  
  Symbol* = ref object of Expr
    sym*: string
  Int*  = ref object of Expr
    val*: uint
  String* = ref object of Expr
    val*: string

  # Note that this also includes field access.
  # This is because an access could include calls and such
  BinExpr* = ref object of Expr
    lhs*: Expr
    rhs*: Expr
  UnaryExpr* = ref object of Expr
    target*: Expr

  IfArm* = object
    cond*: Expr
    val*: Block
  If* = ref object of Expr
    arms*: seq[IfArm]
    default*: Block #?
    final*: Block #?

  Loop* = ref object of Expr
    body*: Block
    counter*: Bind #?
  For* = ref object of Loop
    range*: Expr
    capture*: Bind #?
  While* = ref object of Loop
    cond*: Expr
    capture*: Bind #?
  DoWhile* = ref object of Loop
    cond*: Expr
    # Can't capture since the first run tests nothing.


  # Technically
  # `fn a, b: usize -> res:usize {...}`
  # is actually
  # `FnType Block`

  # This also means that when declaring a function type, you *must* give names to all (though they aren't checked)
  # Essentially, this is forcible documentation.
  # Compile errors will continue until documentation improves.
  FnType* = ref object of Expr
    args*: seq[Bind]
    ret*: Bind #?
  FnDef* = ref object of Expr
    ty*: FnType
    body*: Block

proc add*(self: Block, s: Stmt) =
  self.children.add s
