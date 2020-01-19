import options
import tables

import tokens

type
  Stmt* = ref object of RootObj
  Expr* = ref object of Stmt

  Atom* = ref object of Expr
    tok*: Token

  # assert expr
  Assert* = ref object of Stmt
    expr*: Expr
  # break [`label] [value]
  Return* = ref object of Stmt
    val*: Option[Expr]
  Break* = ref object of Stmt
    label*: Option[Token]
    val*: Option[Expr]

  BlockInterpret*{.pure.} = enum
    Struct, Enum,
    Tree, # block {}
    Token, # #{}
    Sequence
  # [struct|enum|block] [#label] {children}
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
    pub*: bool
  BindTup* = ref object of BindLoc
    children*: seq[BindLoc]
    
  Bind* = ref object of Stmt
    interpret*: BindType
    loc*: BindLoc
    default*: Option[Expr]
  EnumLit* = ref object of Expr
    tok*: Token # The name of the enumeration
    val*: Option[Expr] # The union initializer, if any


  Loop* = ref object of Expr # Infinite loop
    counter*: Option[Bind]
    body*: Block
  LoopType*{.pure.} = enum
    # 'infinite' or similar isn't here becase it's completely different from this type of loop
    For, While, DoWhile # DoWhile TODO
  CondLoop* = ref object of Loop # For/While.
    interpret*: LoopType # Is cond something to iterate, or to check for truthy-ness
    cond*: Expr
    capture*: Option[Bind]
    final*: Option[Block]
    
  IfArm* = object
    cond*: Expr
    val*: Block
    capture*: Option[Bind]
  If* = ref object of Expr
    arms*: seq[IfArm]
    default*: Option[Block]
    final*: Option[Block]
  Tuple* = ref object of Expr
    children*: seq[Expr]
  # The 'fn' here isn't an actual function yet
  # Instead it's an operator, using '(' and '[' for FuncCalls/Indexes
  # This avoids splitting the AST too much
  Call* = ref object of Expr
    fn*: Token ## What do we call?
    args*: seq[Expr]

  # Thus paths are parsed more as a linked list than a tree
  Path* = ref object of RootObj
    next*: Option[Path] # Points to baz in foo.bar.baz and foo.(bar, far).baz
  SwizzlePath* = ref object of Path
    paths*: seq[Path] # (bar, far) in foo.(bar, far).baz
  AccessPath* = ref object of Path
    # Symbol/Int (Perhaps String)
    name*: Token # bar, far, and baz in foo.bar.baz, and foo.(bar, far).baz
  
  # (Foo, Bar).(Baz, Zab)
  Access* = ref object of Expr
    accessed*: Expr
    path*: Path
  CompoundLiteral* = ref object of Expr
    ty*: Option[Expr]
  StructLiteral* = ref object of CompoundLiteral
    fields*: Table[string, Expr]
  ArrayLiteral* = ref object of CompoundLiteral
    children*: seq[Expr]

  Fn* = ref object of Expr
    args*: seq[Bind]
    ret*: Option[Bind] # if isNone then ret.@type == void
    body*: Block


proc asBindType*(tag: Tag): BindType =
  case tag:
    of Tag.Let: return BindType.Let
    of Tag.Var: return BindType.Var
    of Tag.Property: return BindType.Property
    of Tag.Field: return BindType.Field
    of Tag.CVar: return BindType.CVar
    of Tag.Enum: return BindType.Enum
    else: assert false

# Because the default $ for this is long and irksome
proc `$`(t: Token): string =
  case t.what.tag:
    of Symbol:
      return "SYM(" & t.what.lexeme & ")@" & $t.where
    of Label:
      return "LAB(" & t.what.lexeme & ")@" & $t.where
    of IntLit:
      return "INT(" & t.what.lexeme & ")@" & $t.where
    of StringLit:
      return "STR(" & t.what.lexeme & ")@" & $t.where
    of CharLit:
      return "CHAR(" & t.what.lexeme & ")@" & $t.where
    else:
      return "`" & $t.what.tag  & "`@" & $t.where
### Helpers for the prettyPrints
var prettyIndent* = 0
# Just to make it a little easier to handle all the indentation crap
template indent(body: untyped) =
  prettyIndent += 1
  body
  prettyIndent -= 1
template recurse(what: untyped) =
  indent:
    what.prettyPrint()
template prettyEcho(what: varargs[string]) =
  for i in 0..prettyIndent:
    stdout.write "  "
  for arg in what:
    stdout.write arg
  stdout.write "\n"


method prettyPrint*(s: Stmt) {.base.} =
  echo "FELL BACK TO STMT FOR ", repr(s)
method prettyPrint*(e: Expr) =
  echo "FELL BACK TO EXPR"
method prettyPrint*(a: Atom) =
  prettyEcho "Atom: ", $a.tok.what
method prettyPrint*(a: Assert) =
  prettyEcho "Asserting"
  a.expr.recurse()
method prettyPrint*(r: Return) =
  prettyEcho "Returning"
  if r.val.isSome:
    r.val.get.recurse()
method prettyPrint*(b: Break) =
  if b.label.isSome:
    prettyEcho "Breaking from ", b.label.get.what.lexeme
  else:
    prettyEcho "Breaking"
  if b.val.isSome:
    b.val.get.recurse()
method prettyPrint*(b: Block) =
  var msg = case b.interpret:
    of Sequence: "Block"
    of BlockInterpret.Token: "TokenStream"
    of Tree: "ASTree"
    of BlockInterpret.Struct: "StructDef"
    of BlockInterpret.Enum: "EnumDef"
  if b.label.isSome: msg &= " " & b.label.get.what.lexeme
  prettyEcho msg, " {"
  indent:
    for child in b.children:
      child.prettyPrint()
  prettyEcho "}"
method prettyPrint*(b: BindLoc) {.base.} =
  echo "FELL BACK TO BINDLOC"
method prettyPrint*(b: BindSym) =
  var msg = "BindSym "
  if b.pub: msg &= "pub "
  msg &= $b.loc
  if b.ty.isSome: msg &= " with type"
  prettyEcho msg
  if b.ty.isSome:
    b.ty.get.recurse()
method prettyPrint*(b: BindTup) =
  prettyEcho "BindTup"
  for binding in b.children:
    binding.recurse()
method prettyPrint*(b: Bind) =
  prettyEcho $b.interpret
  indent:
    prettyEcho "Loc:"
    b.loc.recurse()
    if b.default.isSome:
      prettyEcho "Value:"
      b.default.get.recurse()
method prettyPrint*(e: EnumLit) =
  prettyEcho "EnumLit: #", e.tok.what.lexeme
  if e.val.isSome: e.val.get.recurse()
method prettyPrint*(l: Loop) =
  prettyEcho "Loop"
  indent:
    if l.counter.isSome:
      prettyEcho "Counter:"
      l.counter.get.recurse()
    prettyEcho "Body:"
    l.body.recurse()
method prettyPrint*(l: CondLoop) =
  prettyEcho $l.interpret
  indent:
    if l.counter.isSome:
      prettyEcho "Counter:"
      l.counter.get.recurse()
    prettyEcho "Body:"
    l.body.recurse()

proc prettyPrint*(arm: IfArm) =
  prettyEcho "Arm:"
  indent:
    if arm.capture.isSome:
      prettyEcho "Capture:"
      arm.capture.get.recurse()
    prettyEcho "Cond:"
    arm.cond.recurse()
    prettyEcho "Then:"
    arm.val.recurse()
method prettyPrint*(i: If) =
  prettyEcho "If"
  indent:
    prettyEcho "Arms:"
    for arm in i.arms:
      arm.recurse()

    if i.default.isSome:
      prettyEcho "Else:"
      i.default.get.recurse()
    if i.final.isSome:
      prettyEcho "Finally:"
      i.final.get.recurse() 


method prettyPrint*(t: Tuple) =
  prettyEcho "Tuple ("
  for child in t.children:
    child.recurse()
  prettyEcho ")"
method prettyPrint*(c: Call) =
  prettyEcho "Calling ", $c.fn
  for arg in c.args:
    arg.recurse()
method prettyPrint*(p: Path) {.base.} =
  echo "HIT BASE PATH"
method prettyPrint*(p: AccessPath) =
  # Since only symbols and Int/String lits can be in a path, this works
  stdout.write p.name.what.lexeme
  if p.next.isSome:
    stdout.write '.'
    p.next.get.prettyPrint()
method prettyPrint*(p: SwizzlePath) =
  stdout.write '('
  for path in p.paths:
    path.prettyPrint()
  stdout.write ')'
  if p.next.isSome:
    stdout.write '.'
    p.next.get.prettyPrint()
method prettyPrint*(a: Access) =
  prettyEcho "Access"
  indent:
    prettyEcho "From:"
    a.accessed.recurse()
    prettyEcho "Take:"
    indent:
      # None of the AccessPaths will do a newline
      for i in 0..prettyIndent: stdout.write "  "
      a.path.prettyPrint()
      stdout.write '\n'
method prettyPrint*(c: CompoundLiteral) =
  echo "FALLING BACK TO COMPOUNDLIT"
method prettyPrint*(s: StructLiteral) =
  prettyEcho "StructLiteral"
  indent:
    if s.ty.isSome:
      prettyEcho "Type:"
      s.ty.get.recurse()
    prettyEcho "Fields:"
    for (name, val) in s.fields.pairs:
      indent:
        prettyEcho ".", name, " = "
        val.recurse()
method prettyPrint*(s: ArrayLiteral) =
  prettyEcho "ArrayLiteral"
  indent:
    if s.ty.isSome:
      prettyEcho "Type:"
      s.ty.get.recurse()
    prettyEcho "Members:"
    for val in s.children:
      val.recurse()
method prettyPrint*(f: Fn) =
  prettyEcho "Fn"
  indent:
    if f.args.len > 0:
      prettyEcho "Args:"
      for arg in f.args:
        arg.recurse()
    if f.ret.isSome:
      prettyEcho "Return:"
      f.ret.get.recurse()
    prettyEcho "Do:"
    f.body.recurse()
