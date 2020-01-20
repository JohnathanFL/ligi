import options
import tables

import tokens

type
  TypeClass*{.pure.} = enum
    Void, UInt, Int, USize, ISize,
    Bool, Char, Str, Any,
    Type, # i.e the symbol usize/isize, or a stored Struct
    Tuple,
    FnType,
    Structure, Enumeration
  TypeId* = object
    case class*: TypeClass
      of UInt, Int:
        numBits*: 1..256
      of Structure:
        structId*: uint
      of Enumeration:
        enumId*: uint
      of Tuple:
        children*: seq[TypeId]
      of FnType:
        args*: seq[TypeId]
        ret*: ref TypeId
      else: discard


# 1-256
proc uintType(size: 1..256): TypeId =
  return TypeId(class: TypeClass.UInt, numBits: size)
proc intType(size: uint): TypeId =
  return TypeId(class: TypeClass.Int, numBits: size)

# With heavy inspiration from Lisp/Nim/similar: The new S-expression styled AST
# The idea is that there will be a global table of union(Nim Func, Zag Func) to overload
# any command (although only operators can be overloaded by users) that gets indexed
# and called.
# The interpreter runs in 2 phases: Evaluator and Runner.
# Evaluator resolves all types and runs all comptime-possible operations
# Runner evaluates all static operations
type
  List* = seq[Expr]
  # Note that the actual values of symbols are stored outside this AST
  # This AST is designed to be incredibly simple to interpret.
  Expr* = object
    pos*: FilePos
    case cmd*: Command
    of Sink, Undef: discard
    # The atoms are themselves
    of Int:
      intVal*: int
    of Float:
      floatVal*: float
    of Symbol:
      symbol*: string
    of Block:
      label*: Option[string]
      subtree*: List
    of String:
      strVal*: string
    of Char:
      charVal*: string
    # Anything else needs to be interpreted from args
    else:
      args*: List
  Command* = enum
    # Do nothing, just return the list
    # Essentially the 'list' function in lisp
    Tuple,
    # Write to it does nothing
    # Comparing against it is always true. Thus (0, 1) < (1, _) is true
    Sink,
    # 'we don't know'. I'm thinking of replacing the 'any' type with just this
    Undef,
    # All normal operators.
    # All of these can be overloaded, with the exception of the specifics of tuples.
    AShr, Shr, Shl, BitNot, BitAnd, BitOr, BitXor,
    Add, Sub, Mul, Div, Mod,
    Not, And, Or, Xor
    Eq, NotEq, Less, Greater, LessEq, GreaterEq,
    OpenRange, ClosedRange,
    In, NotIn,
    Assign, AddAssign, SubAssign, MulAssign, DivAssign, ShlAssign, ShrAssign,

    # Find a value, given a starting point and a path
    # args[0] is what to access, all after is a path. Thus foo.bar.(baz, faz).car is:
      # Access:(Symbol:foo, Symbol:bar, Tuple:(Symbol:baz, Symbol:faz), Symbol:car)
    Access, 
    # Comptime stuff
    Proc, Optional, Inline, Comptime, Const,
    Slice, Array, Enum, Struct,
    # Control flow
    # arg[0] is a tuple of tuples, each of which is (condition, capture, value)
    # arg[1] is the else
    # arg[2] is the finally
    If,
    # arg[0] is the range or condition
    # arg[1] is the capture(s)
    # arg[2] is what to do each time
    For, While,
    # arg[0] is the capture
    # arg[1] is what to do each time
    Loop,
    # arg[0] is what to return
    Return,
    # arg[0] is what to break from
    # arg[1] is what to break with
    Break,
    # arg[0] is the type
    # arg[1] is a tuple of the values
    ArrayLit,
    # arg[0] is the type
    # arg[1] is a tuple of tuples of (name, val)
    StructLit,
    # arg[0] is the name of the enum
    # arg[1] is the value of the union
    EnumLit,
    # arg[0] is a tuple of arg binds
    # arg[1] is the return
    # arg[2] is what to do. Note this is not a block, as blocks may be broken/labeled
    FnDef,
    # A sequence of statements to be evaluated.
    # The first non-void value evaluated breaks the block
    # Break will attempt to go up the tree until it finds its label in one of these.
    Block,
    # Binders
    # Each of these uses:
      # arg[0]: The location(s) to bind. May be a tuple of tuples and so on
      # arg[1]: The total type of the bind
      # arg[2]: The value the expression begins as, or Undef for... undef
      # The types are expanded out from the initial expression to be in arg[2]
      # This means a parser must move the types in (x:usize, (y:isize, z:u8)) out into their own tuple
      # For example: let (x:usize, (y:isize, z:u8)) = (1, (2, 3)) becomes
        # Let(
          # Tuple(Symbol:x, Tuple(Symbol:y, Symbol:z)),
          # Tuple(Symbol:usize, Tuple(Symbol:isize, Symbol:u8)),
          # Tuple(Int:1, Tuple(Int:2, Int:3))
        # )
      # It's somewhat more convoluted for the parser, but I think it makes the AST more logical.
    Let, Var, CVar, Field, Property,
    # Special binders
    # Alias just takes 2 args: The symbol to bind as, and the path to bind
    # Use takes just 1 arg: The path to import into scope
    Alias, Use,
    # Special control flow: Only interpreted with `zag test`
    Test,
    # Atoms. Simply store their own stuff
    Int, Float, Symbol, String, Char
    
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
