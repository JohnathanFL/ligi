import options

# By convention, I:
# - mark nullable references with #?
# - use `ty` for the type of an expression
# - use `val` when a type holds only one Expr (that's intended to be returned)
# - use `default` for `else`
# - use `final` for `finally`

# Only yields void
type Stmt* = ref object of RootObj
# May yield any type
type Expr* = ref object of Stmt

type BindSpec = enum
  # What a load of bs
  bsLet, bsVar, bsCVar,
  bsEnum, bsField
type BinOp* = enum # Incomplete
  opAdd, opSub, opMul, opDiv, Pipeline
type UnaOp* = enum # Incomplete
  # Mathematical
  opNeg, opNot, opBitNot,
  # Type creation
  opStruct, opArray, opSlice, opEnum, opComptime, opConst
  opPtr, opRef,
  # Special
  opBlock
type CallKind* = enum
  Call, Index


type AccessOp* = ref object of RootObj
type AccessSwizzle* = ref object of AccessOp
  accesses*: seq[AccessOp]
type AccessValue* = ref object of AccessOp
  name*: string
type AccessCall* = ref object of AccessOp # Either () or []
  kind*: CallKind
  args*: seq[Expr]


type Assert* = ref object of Stmt
  val*: Expr
  msg*: string
type Break* = ref object of Stmt
  label*: string #?=""
  val*: Expr
type Return* = ref object of Stmt
  val*: Expr
type Bind* = ref object of Stmt
  spec*: BindSpec
  ty*: Expr #?
  init*: Expr #?
type Assg* = ref object of Stmt
  augment*: Option[BinOp] # +=, -=, etc
  to*: Expr
  val*: Expr

type Binary* = ref object of Expr
  op*: BinOp
  lhs*: Expr
  rhs*: Expr
type Unary* = ref object of Expr
  op*: UnaOp
  val*: Expr

type Atom* = ref object of Expr # Can be accessed
  access*: seq[AccessOp]
type Word* = ref object of Atom
  word*: string
type String* = ref object of Atom
  str*: string
type Block* = ref object of Atom
  label*: string # An empty label is the same as no label. Thus #""{} is the same as {}
  stmts*: seq[Stmt]
type Compound* = ref object of Atom
  ty*: Expr
type Tuple* = ref object of Compound
  vals*: seq[Expr]
type Array* = ref object of Tuple # An array is a tuple that holds only one type
type Struct* = ref object of Compound
  fields*: seq[Bind] # Bind.spec is always bsVar. The other #? rules still apply


type
  IfArm* = tuple [
    cond: Expr,
    capt: Bind, #?
    val: Expr
  ]
  If* = ref object of Expr
    arms*: seq[IfArm]
    default*: Stmt #?
    final*: Stmt #?

type
  WhenArm* = tuple [
    op: BinOp, # Defaults to opEq
    rhs: Expr,
    capt: Bind, #?
    val: Stmt
  ]
  When* = ref object of Expr
    lhs*: Expr
    arms*: seq[WhenArm]
    default*: Stmt #?
    final*: Stmt#?


type Loop* = ref object of Expr
  label*: string #?=""
  default*: Expr #?
  final*: Expr #?
  counter*: Bind #?, can assume !nil if capt is !nil
type For* = ref object of Loop
  range*: Expr
  capt*: Bind #?
type While* = ref object of Loop
  cond*: Expr
  capt*: Bind #?




