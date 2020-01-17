import options
import tables

import tokens

type
  Stmt* = ref object of RootObj
  Expr* = ref object of Stmt
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


  BindType{.pure.} = enum
    Let, Var, Enum, Field
  Bind* = ref object of Stmt
    loc*: Token
    interpret*: BindType
    default*: Option[Expr]
    ty*: Option[Expr] # Must resolve in typechecking to a type. After typechecking must not be null

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
    args*: seq[ref Expr]

  # (Foo, Bar).(Baz, Zab)
  Access* = ref object of Expr
    fromWhat*: Expr
    toWhat*: Expr
  CompoundLiteral* = ref object of Expr
    # ty must resolve to either an array type or a struct type.
    # If it's a an array type, it may have no named.
    # If it's a struct, all positionals get converted to either _0.._n
    # or (if I add it) something like `0`..`n` (using backticks to escape numerals)
    # Since type expressions are now just expressions, this works :D
    ty*: Option[ref Expr]
    positional*: seq[ref Expr]
    named*: Table[string, ref Expr]
