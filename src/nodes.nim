import options
import tables

import tokens

# Using pure refs instead of Options for slightly cleaner code (maybe)
# I'll try to annotate nillable with #?
# (And yeah, it is somewhat ironic to use this style while building a language without implicit nils)
type
  Command*{.pure.} = enum

    Access, Add, AddAssign, Alias, And,
    Array, AShr, Assert, Assign, BitAnd,
    BitAndAssign, BitNot, BitNotAssign, BitOr, BitOrAssign,
    BitXor, BitXorAssign, Break, Char, ClosedRange,
    Comptime, Const, CVar, Div, DivAssign,
    DoWhile, Enum, EnumDef, Equal, Field,
    For, Greater, GreaterEq, If, In,
    Inline, Int, Less, LessEq, Let,
    Loop, Mod, Mul, MulAssign, Not,
    NotEqual, NotIn, Null, OpenRange, Optional,
    Or, Property, Pure, Return, Shl,
    ShlAssign, Shr, ShrAssign, Sink, Slice,
    Spaceship, String, StructDef, Sub, SubAssign,
    Symbol Test, Undef, Use, Var,
    Void, While, Xor,
  Stmt* = ref object of RootObj
    pos*: tuple[line: uint, col: uint]
    cmd*: Command
  Assert* = ref object of Stmt
    expr*: Expr
  Break* = ref object of Stmt
    val*: Expr #?
    label*: ref string #?
  Return* = ref object of Stmt
    val*: Expr #?

  BindLoc* = ref object of RootObj
  BindTuple* = ref object of BindLoc
    locs*: seq[BindLoc]
  BindSym* = ref object of BindLoc
    loc*: string
    ty*: Expr
    isPub*: bool
  # As in let (_, x) = (1, 2).
  # We want to just discard any writes to this.
  # It becomes
    # BindTuple
      # BindSink
      # BindSym(x, undef, false)
  BindSink* = ref object of BindLoc
  # Note this technically means `let _ = 10` is valid.
  Bind* = ref object of Stmt
    # We know the bind type from Stmt.cmd
    loc*: BindLoc
    init*: Expr #?

  # Anything that can yield a value
  Expr* = ref object of Stmt

  # Can yield a value from either a break or an unconsumed value
  Block* = ref object of Expr
    children*: seq[Stmt]
    label*: ref string

  # The atoms
  
  Symbol* = ref object of Expr
    sym*: string
  Int*  = ref object of Expr
    val*: uint
  String* = ref object of Expr
    val*: string
  
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
