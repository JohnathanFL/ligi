import options
import tables

import tokens

type
  Stmt* = ref object of RootObj
  Expr* = ref object of Stmt

  Atom* = ref object of Expr
    tok*: Token
  
  Assert* = ref object of Expr
    expr*: Expr


  BlockInterpret*{.pure.} = enum
    Struct, Enum,
    Tree, # block {}
    Token, # #{}
    Sequence
  Block* = ref object of Expr # Inherits from Expr so we can use struct {} and similar
    children*: seq[Stmt]
    interpret*: BlockInterpret
    label*: Option[Token]


  BindType*{.pure.} = enum
    Let, Var, Enum, Field, Property, CVar

  BindLoc* = ref object of RootObj
  BindSym* = ref object of BindLoc
    loc*: Token
    ty*: Option[Expr]
  BindTup* = ref object of BindLoc
    children*: seq[BindLoc]
    
  Bind* = ref object of Stmt
    interpret*: BindType
    loc*: BindLoc
    default*: Option[Expr]
  EnumLit* = ref object of Expr
    tok*: Token # The name of the enumeration
    val*: Option[Expr] # The union initializer, if any


  Loop* = ref object of Stmt # Infinite loop
    counter*: Option[Bind]
    body*: Block
    final*: Option[Block]
  While* = ref object of Loop # Conditional loop
    cond*: Expr
    capture*: Option[Bind]
  For* = ref object of Loop # Iterator loop
    range*: Expr
    capture*: Option[Bind] 
    
  IfArm* = object
    cond*: Expr
    val*: Block
    capture*: Option[Bind]
  IfStmt* = ref object of Stmt
    arms*: seq[IfArm]
    default*: Option[Block]
    final*: Option[Block]
  Tuple* = ref object of Expr
    children*: seq[Expr]
  Factor* = ref object of Expr
    val*: Token
  Call* = ref object of Expr
    fn*: Token ## What do we call?
    args*: seq[Expr]
  FuncCall* = ref object of Expr
    caller*: Expr
    args: seq[Expr]
  IndexCall* = distinct FuncCall

  # (Foo, Bar).(Baz, Zab)
  Access* = ref object of Expr
    fromWhat*: Expr
    toWhat*: Expr
  CompoundLiteral* = ref object of Expr
    ty*: Option[Expr]
  StructLiteral* = ref object of CompoundLiteral
    fields*: Table[string, Expr]
  ArrayLiteral* = ref object of CompoundLiteral
    children*: seq[Expr]

proc asBindType*(tag: Tag): BindType =
  case tag:
    of Tag.Let: return BindType.Let
    of Tag.Var: return BindType.Var
    of Tag.Property: return BindType.Property
    of Tag.Field: return BindType.Field
    of Tag.CVar: return BindType.CVar
    of Tag.Enum: return BindType.Enum
    else: assert false

proc `$`*(s: Stmt): string =
  if s is Stmt: return "STMT"
  elif s is Expr: return "EXPR"
  else: return "UNIMPLEMENTED"
