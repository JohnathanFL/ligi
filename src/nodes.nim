import options

type
  FilePos* = tuple[line: uint, col: uint]
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
# This style also allows lexer/parser to be completely uncoupled from anything that comes
# after them, as the AST need no longer no about Tokens (other than the FilePos)
# It should also make the prettyprinter for it short and sweet
# Other changes: block/struct/enum are now unary ops.
# This frees up some parsing logic.
type
  Command* = enum
    # Do nothing, just return the list
    # Essentially the 'list' function in lisp
    Tuple,
    # Write to it does nothing
    # Comparing against it is always true. Thus (0, 1) < (1, _) is true
    Sink,
    # 'we don't know'. I'm thinking of replacing the 'any' type with just this
    Undef,
    # All normal operators. All are either binary or unary.
    # All of these can be overloaded, with the exception of the specifics of tuples.
    AShr, Shr, Shl, BitNot, BitAnd, BitOr, BitXor,
    Add, Sub, Mul, Div, Mod,
    Not, And, Or, Xor
    Eq, NotEq, Less, Greater, LessEq, GreaterEq, Spaceship,
    OpenRange, ClosedRange,
    In, NotIn,
    Assign, AddAssign, SubAssign, MulAssign, DivAssign, ShlAssign, ShrAssign,
    BitNotAssign, BitAndAssign, BitOrAssign, BitXorAssign,

    # Call arg[0] with arguments arg[1..]
    Call,
    # Index into arg[0] with arguments arg[1..]
    Index,

    # Panic if subtree is false at runtime or can be proven false at comptime
    Assert,

    # Find a value, given a starting point and a path
    # args[0] is what to access, all after is a path. Thus foo.bar.(baz, faz).car is:
      # Access:(Symbol:foo, Symbol:bar, Tuple:(Symbol:baz, Symbol:faz), Symbol:car)
    Access,
    # Mark a value with a key, as in '.x = 10'
    Designator,
    # Comptime stuff. In order:
      # Create a proc type/optional type, inline a subtree, comptime eval a subtree, mark a type as const,
      # make an array type, evaluate subtree as a new enum/struct, force purification of a function,
      # create a slice type
    Proc, Optional, Inline, Comptime, Const,
    Array, EnumDef, StructDef, Pure, Slice,
    # Control flow
    # arg[0] is a tuple of tuples, each of which is (condition, capture, value)
    # arg[1] is the else
    # arg[2] is the finally
    If,
    # arg[0] is the range or condition
    # arg[1] is the capture(s)
    # arg[2] is what to do each time
    For, While,
    # Same as above, but arg[1] isn't there
    DoWhile
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
      # These may be designators for a struct type
    CompoundLit,
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
    # A RawBlock is parsed the same, but is passed to user code as the raw AST itself
    Block, RawBlock,
    # Binders
    # Each of these uses:
      # arg[0]: The location(s) to bind. May be a tuple of tuples and so on
      # arg[1]: The total type of the bind
        # I.e (x:usize, i:isize) -> (x,y), (usize, isize)
      # arg[2]: Which locations are public
      # arg[3]: The value the expression begins as, or Undef for... undef
      # The types are expanded out from the initial expression to be in arg[2]
      # This means a parser must move the types in (x:usize, (y:isize, z:u8)) out into their own tuple
      # For example: let (x:usize, (y:isize, z:u8)) = (1, (2, 3)) becomes
        # Let(
          # Tuple(Symbol:x, Tuple(Symbol:y, Symbol:z)),
          # Tuple(Symbol:usize, Tuple(Symbol:isize, Symbol:u8)),
          # Tuple(Int:1, Tuple(Int:2, Int:3))
        # )
      # It's somewhat more convoluted for the parser, but I think it makes the AST more logical.
    Let, Var, CVar, Field, Property, Enum,
    # Special binders
    # Alias just takes 2 args: The symbol to bind as, and the path to bind
    # Use takes just 1 arg: The path to import into scope
    Alias, Use,
    # Special control flow: Only interpreted with `zag test`
    # arg[0]: The name of the test
    # arg[1]: The body of the test
    Test,
    # Atoms. Simply store their own stuff
    Int, Float, Symbol, String, Char, Bool,
    # Atom, but doesn't need to store anything
    Null,
  List* = seq[Expr]
  # Note that the actual values of symbols are stored outside this AST
  # This AST is designed to be incredibly simple to interpret.
  Expr* = object
    pos*: FilePos
    case cmd*: Command
    of Sink, Undef, Null: discard
    # The atoms are themselves
    of Int:
      intVal*: int
    # Never emitted by the parser, as 1.0 is Access(Int:1, Int:0)
    # The evaluator must evaluate that path into a float
    of Float:
      floatVal*: float
    of Bool:
      boolVal*: bool
    of Symbol:
      symbol*: string
    of Block, Test:
      label*: Option[string]
      subtree*: List
    of String:
      strVal*: string
    of Char:
      charVal*: string
    of Designator:
      key*: string
      val*: ref Expr
    # Anything else needs to be interpreted from args
    else:
      args*: List

proc `[]`*(e: Expr, index: uint): Expr =
  case e.cmd:
    of Sink, Undef, Int, String, Float, Char: assert false
    of Block:
      return e.subtree[index]
    else:
      return e.args[index]

### Helpers for the prettyPrints
var prettyIndent* = 0
# Just to make it a little easier to handle all the indentation crap
template indent(body: untyped) =
  prettyIndent += 1
  body
  prettyIndent -= 1
template prettyEcho(what: varargs[string]) =
  for i in 0..prettyIndent:
    stdout.write "  "
  for arg in what:
    stdout.write arg
  stdout.write "\n"

proc prettyPrint(expr: Expr) =
  case expr.cmd:
    of Int:
      prettyEcho "Int:", $expr.intVal
    of Float:
      prettyEcho "Float:", $expr.floatVal
    of Symbol:
      prettyEcho "Symbol:", expr.symbol
    of Block:
      if expr.label.isSome:
        prettyEcho "Block:", expr.label.get, "{"
      else:
        prettyEcho "Block {"
        indent:
          for s in expr.subtree:
            s.prettyPrint()
        prettyEcho "}"
    of String:
      prettyEcho "String:\"", expr.strVal, "\""
    of Char:
      prettyEcho "Char:'", expr.charVal, "\'"
    else:
      # Just a command with args
      prettyEcho $expr.cmd, "("
      indent:
        for arg in expr.args:
          arg.prettyPrint()
