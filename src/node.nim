import options
import tables

import lexer

type
  Stmt = object of RootObj
  Expr = object of Stmt


  BlockInterpret{.pure.} = enum
    Struct, Enum, Concept,
    Token, # block {}
    Sequence
  Block = object of Expr # Inherits from Expr so we can use struct {} and similar
    children: seq[ref Stmt]
    interpret: BlockInterpret
    label: Option[lexer.Token]


  BindType{.pure.} = enum
    Let, Var, Enum, Field
  Bind = object of Stmt
    loc: Token
    interpret: BindType
    default: Option[ref Expr]
    ty: Option[ref Expr] # Must resolve in typechecking to a type. After typechecking must not be null

  Loop = object of Stmt # Infinite loop
    counter: Option[ref Bind]
    body: ref Block
    final: Option[ref Block]
  While = object of Loop # Conditional loop
    cond: ref Expr
    capture: Option[ref Bind]
    continuation: Option[ref Stmt]
  For = object of Loop # Iterator loop
    range: ref Expr
    capture: Option[ref Bind] 
    
  IfArm[T] = object
    cond: ref Expr
    val: ref T
    capture: Option[ref Bind]
  IfStmt = object of Stmt
    arms: seq[IfArm[Block]]
    default: Option[ref Block]
    final: Option[ref Block]


  Factor = object of Expr
    val: Token
  Call = object of Expr
    fn: Token ## What do we call?
    args: seq[ref Expr]
  IfExpr = object of Expr
    arms: seq[IfArm[Expr]]
    default: ref Expr # IfExpr must have a default
  CompoundLiteral = object of Expr
    # ty must resolve to either an array type or a struct type.
    # If it's a an array type, it may have no named.
    # If it's a struct, all positionals get converted to either _0.._n
    # or (if I add it) something like `0`..`n` (using backticks to escape numerals)
    # Since type expressions are now just expressions, this works :D
    ty: Option[ref Expr]
    positional: seq[ref Expr]
    named: Table[string, ref Expr]
