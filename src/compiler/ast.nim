import options

import lexing

# By convention, I:
# - mark nullable references with #?
# - use `ty` for the type of an expression
# - use `val` when a type holds only one Expr (that's intended to be returned)
# - use `default` for `else`
# - use `final` for `finally`


# Only yields void
type Stmt* = ref object of RootObj
# May yield any type
type
  Expr* = ref object of Stmt
    path*: seq[AccessOp]
  AccessOp* = ref object of RootObj
  AccessSwizzle* = ref object of AccessOp
    paths*: seq[AccessOp]
  AccessValue* = ref object of AccessOp
    name*: string
  CallKind* = enum
    ckCall, ckIndex
  AccessCall* = ref object of AccessOp # Either () or []
    kind*: CallKind
    args*: seq[Expr]


type BindSpec* = enum
  # What a load of bs
  bsLet = tLet, bsVar = tVar, bsCVar = tCVar, bsField = tField
  bsEnum = tEnum, 
type BinOp* = enum # Incomplete
  opEq=tEq,
  opNotEq=tNotEq,
  opSpaceship=tSpaceship
  opGt=tGt,
  opLt=tLt,
  opGtEq=tGtEq,
  opLtEq=tLtEq
  opIn=tIn,
  opNotIn=tNotIn,
  opAdd=tAdd,
  opSub=tSub,
  opMul=tMul,
  opDiv=tDiv,
  opMod=tMod,
type UnaOp* = enum # Incomplete
  opNeg = tSub,
  opPtr = tMul,
  opBitNot = tBitNot,
  opNot = tNot,
  opStruct = tStruct,
  opRef = tRef,
  opSlice = tSlice,
  opArray = tArray,
  opConst = tConst
  opComptime = tComptime,
  opOpt = tOpt,
  opPure = tPure,
  opInline = tInline,
  opOverload = tOverload,
  opProperty = tProperty,
  opConcept = tConcept,
  opBlock = tBlock,
  opEnum = tEnum,


type
  BindLoc* = ref object of RootObj
    ty*: Expr #? (this means you can do `let (x, y: usize): (isize, usize)`)
  BindName* = ref object of BindLoc
    name*: string
  BindTuple* = ref object of BindLoc
    locs*: seq[BindLoc]
type Bind* = object
  loc*: BindLoc
  init*: Expr #?

type Assert* = ref object of Stmt
  val*: Expr
  msg*: string
type Break* = ref object of Expr
  label*: string #?=""
  val*: Expr
type Defer* = ref object of Stmt
  stmt*: Stmt
type Return* = ref object of Expr
  val*: Expr
type BindGroup* = ref object of Stmt
  spec*: BindSpec
  binds*: seq[Bind]
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


type Word* = ref object of Expr
  word*: string
type String* = ref object of Expr
  str*: string
type Block* = ref object of Expr
  label*: string #?=""
  stmts*: seq[Stmt]
type Compound* = ref object of Expr
  ty*: Expr
type Tuple* = ref object of Compound
  vals*: seq[Expr]
type ArrayLit* = ref object of Tuple # An array is a tuple that holds only one type
type StructLit* = ref object of Compound
  fields*: seq[Bind]
type EnumLit* = ref object of Expr
  label*: string
  inner*: Compound


type Macro* = ref object of Expr
  args*: seq[BindLoc]
  body*: Stmt
type Fn* = ref object of Expr
  args*: seq[BindLoc]
  ret*: Bind

type Pipeline* = ref object of Expr
  parts*: seq[Expr]

type ControlStructure* = ref object of Expr
  label*: string #?=""

  default*: Expr #?
  defCapt*: BindLoc #?

  final*: Expr #?
  finCapt*: BindLoc #?


type Selection* = ref object of ControlStructure
type
  IfArm* = object
    cond*: Expr
    capt*: BindLoc #?
    val*: Expr
  If* = ref object of Selection
    arms*: seq[IfArm]

type
  WhenArm* = object
    op*: BinOp # Defaults to opEq
    rhs*: Expr
    capt*: BindLoc #?
    val*: Stmt
  When* = ref object of Selection
    lhs*: Expr
    lhsCapt*: BindLoc #?
    arms*: seq[WhenArm]


type Loop* = ref object of ControlStructure
  counter*: BindLoc #?, can assume !nil if capt is !nil
  body*: Stmt
type For* = ref object of Loop
  expr*: Expr
  capt*: BindLoc #?
type While* = ref object of Loop
  expr*: Expr
  capt*: BindLoc #?




