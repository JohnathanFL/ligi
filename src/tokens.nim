import strutils

type
  Tag* {.pure.} = enum
    # Leaving this here, whether as a monument to my insanity or brilliance I don't know
    # Float literals are now parsed as field accesses into an int
    # (i.e every int has an infinite number of fields, each named for an int)
    # FloatLit = "FLOATLIT"
    Add = "+"
    AddAssign = "+="
    Alias = "alias"
    And = "and"
    Array = "array"
    AShr = ">>>"
    Assert = "assert"
    Assign = "="
    BitAnd = "&"
    BitAndAssign = "&="
    BitNot = "~"
    BitNotAssign = "~="
    BitOr = "|"
    BitOrAssign = "|="
    BitXor = "^"
    BitXorAssign = "^="
    Break = "break"
    CharLit = "CHARLIT"
    ClosedRange = "..="
    Comma = ","
    Comptime = "comptime"
    Const = "const"
    CVar = "cvar"
    Div = "/"
    DivAssign = "/="
    DoWhile = "dowhile"
    ElIf = "elif"
    Else = "else"
    Enum = "enum"
    EOF = "EOF"
    Equal = "=="
    FieldAccess = "."
    Field = "field"
    Finally = "finally"
    Fn = "fn"
    For = "for"
    Greater = ">"
    GreaterEq = ">="
    If = "if"
    In = "in"
    NotIn = "notin"
    Inline = "inline"
    IntLit = "INTLIT"
    Label = "LABEL"
    LBrace = "{"
    LBracket = "["
    Less = "<"
    LessEq = "<="
    Let = "let"
    Loop = "loop"
    LParen = "("
    Mod = "%"
    Mul = "*"
    MulAssign = "*="
    NotEqual = "!="
    Not = "not"
    NullLit = "null"
    OpenRange = ".."
    Optional = "?"    # These shall be actual operators
    Or = "or"
    Pound = "#"
    Proc = "proc"
    Property = "property"
    Pure = "pure"
    RBrace = "}"
    RBracket = "]"
    Return = "return"
    RParen = ")"
    Semicolon = ";"
    Separator = ":"
    Shl = "<<"
    ShlAssign = "<<="
    Shr = ">>"
    ShrAssign = ">>="
    Sink = "_"
    Slice = "slice"
    Spaceship = "<=>" # Spaceship only tentative
    StoreIn = "->"
    StringLit = "STRLIT"
    Struct = "struct"
    Sub = "-"
    SubAssign = "-="
    Symbol = "SYM"
    Test = "test" # TODO
    Undef = "undef"
    Use = "use"
    Var = "var"
    Void = "void"
    While = "while"
    Xor = "xor"

    INVALID_TAG = "INVALID"


# Expression hierarchy is Binary->Unary->Access->(Repeat in parens)


type BinLevel*{.pure.} = enum
  Assignment, Equality, Relational, Ors, Ands, Membership, Range, Arithmetic, Product, Bitwise

template below*(level: BinLevel): BinLevel =
  BinLevel(ord(level) + 1)

# All expression-level binary operators.
# FieldAccess and such are not included here as they are parsed below even unary
const BinOps*: array[BinLevel, set[Tag]] = [
  {Assign, AddAssign, SubAssign, MulAssign, DivAssign, BitOrAssign, BitAndAssign, ShlAssign, ShrAssign},
  {Equal, NotEqual, Assert},
  {Less, Greater, GreaterEq, LessEq, Spaceship},
  {Or, Xor},
  {And},
  {In, NotIn},
  {OpenRange, ClosedRange},
  {Add, Sub},
  {Mul, Div, Mod},
  {BitOr, BitAnd, BitXor},
]

  

const UnaryOps*: set[Tag] = {
  Sub, BitNot, Not,
  Const, Comptime, # Used for type expressions
  Array, Tag.Slice, Optional,
  Pure, Inline, Proc,
  # As pointer
  Mul
}

const CallOps*: set[Tag] = { LParen, LBracket }

const BindSpecs*: set[Tag] = {
  Let, Var, CVar, Field, Property, Enum
}

const Atoms*: set[Tag] = {
  Symbol, NullLit, IntLit, StringLit
}
const Literals*: set[Tag] = {
  NullLit, IntLit, StringLit
}

const Closers*: set[Tag] = {
    RParen, RBracket, RBrace
}

# In foo.bar.baz, these are the bar/baz
const ValidSwizzles*: set[Tag] = {
  Symbol, IntLit
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

