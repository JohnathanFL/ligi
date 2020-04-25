import strutils
import tags



# Expression hierarchy is Binary->Unary->Access->(Repeat in parens)


type BinLevel*{.pure.} = enum
  Assignment, Equality, Relational, Ors, Ands, Membership, Range, Arithmetic, Product, Bitwise

template below*(level: BinLevel): BinLevel =
  BinLevel(ord(level) + 1)

# All expression-level binary operators.
# FieldAccess and such are not included here as they are parsed below even unary
const BinOps*: array[BinLevel, set[Tag]] = [
  {Tag.Assign, Tag.AddAssign, Tag.SubAssign, Tag.MulAssign, Tag.DivAssign, Tag.BitOrAssign, Tag.BitAndAssign, Tag.ShlAssign, Tag.ShrAssign},
  {Tag.Equal, Tag.NotEqual},
  {Tag.Less, Tag.Greater, Tag.GreaterEq, Tag.LessEq, Tag.Spaceship},
  {Tag.Or, Tag.Xor},
  {Tag.And},
  {Tag.In, Tag.NotIn},
  {Tag.OpenRange, Tag.ClosedRange},
  {Tag.Add, Tag.Sub},
  {Tag.Mul, Tag.Div, Tag.Mod},
  {Tag.BitOr, Tag.BitAnd, Tag.BitXor},
]

  

const UnaryOps*: set[Tag] = {
  Tag.Sub, Tag.BitNot, Tag.Not,
  Tag.Const, Tag.Comptime, # Used for type expressions
  Tag.Array, Tag.Slice, Tag.Optional,
  Tag.Pure, Tag.Inline, Tag.Struct, Tag.Enum, Tag.Overload, Tag.Property,
  # As pointer
  Tag.Mul
}

const CallOps*: set[Tag] = { Tag.LParen, Tag.LBracket }

const BindSpecs*: set[Tag] = {
  Tag.Let, Tag.Var, Tag.CVar, Tag.Field, Tag.Enum
}

# Note that tuples are technically atoms, though they aren't included here
const Atoms*: set[Tag] = {
  Tag.Symbol, Tag.NullLit, Tag.IntLit, Tag.StringLit
}
const Literals*: set[Tag] = {
  Tag.NullLit, Tag.IntLit, Tag.StringLit
}

const Closers*: set[Tag] = {
    Tag.RParen, Tag.RBracket, Tag.RBrace
}

# In foo.bar.baz, these are the bar/baz
const ValidSwizzles*: set[Tag] = {
  Tag.Symbol, Tag.IntLit
}

const ValidSymbolBeginnings*: set[char] = {'_', '@'} + Letters
const ValidSymbolChars*: set[char] = ValidSymbolBeginnings + Digits
# A stroke of insanity: I'll now parse float lits as field accesses into ints

type
  FilePos* = tuple[line:uint, col:uint]
  Token* = object
    pos*: FilePos
    case tag*: Tag
    of Tag.Symbol, Tag.Label,  StringLit, CharLit:
      lexeme*: string
    of Tag.IntLit:
      val*: uint
    else: discard

