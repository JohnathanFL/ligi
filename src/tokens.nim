
type
  Tag* {.pure.} = enum
    # Or/And-Assigns are Bitwise
    Test = "test" # TODO
    Array = "array"
    Semicolon = ";"
    StoreIn = "->"
    Comma = ","
    Add = "+"
    AddAssign = "+="
    And = "and"
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
    Block = "block"
    CharLit = "CHARLIT"
    Comptime = "comptime"
    Concept = "concept"
    Const = "const"
    Div = "/"
    DivAssign = "/="
    Enum = "enum"
    ElIf = "elif"
    Else = "else"
    EOF = "EOF"
    Equal = "=="
    Field = "field"
    FieldAccess = "."
    # Leaving this here, whether as a monument to my insanity or brilliance I don't know
    # Float literals are now parsed as field accesses into an int
    # (i.e every int has an infinite number of fields, each named for an int)
    # FloatLit = "FLOATLIT"
    Fn = "fn"
    Greater = ">"
    GreaterEql = ">="
    If = "if"
    IntLit = "INTLIT"
    Label = "LABEL"
    LBrace = "{"
    LBracket = "["
    Less = "<"
    LessEql = "<="
    Let = "let"
    LParen = "("
    Mod = "%"
    Mul = "*"
    MulAssign = "*="
    Not = "not"
    NotEqual = "!="
    NullLit = "null"
    Optional = "?"    # These shall be actual operators
    Or = "or"
    PureFn = "purefn"
    RBrace = "}"
    RBracket = "]"
    RParen = ")"
    Separator = ":"
    Slice = "slice"
    Shl = "<<"
    ShlAssign = "<<="
    Shr = ">>"
    ShrAssign = ">>="
    Spaceship = "<=>" # Spaceship only tentative
    StringLit = "STRLIT"
    Struct = "struct"
    Sub = "-"
    SubAssign = "-="
    Symbol = "SYM"
    Undef = "undef"
    Var = "var"
    Xor = "xor"
    Void = "void"
    For = "for"
    While = "while"
    Loop = "loop"
    Finally = "finally"
    Pound = "#"

    Break = "break"
    Pure = "pure"
    Property = "property"
    Use = "use"
    Alias = "alias"
    In = "in"
    CVar = "cvar"

    OpenRange = ".."
    ClosedRange = "..="

    INVALID_TAG = "INVALID"


# Expression hierarchy is Binary->Unary->Access->(Repeat in parens)


type BinLevel*{.pure.} = enum
  Assignment, Equality, Relational, Ors, Ands, Range, Arithmetic, Product, Bitwise

template below*(level: BinLevel): BinLevel =
  BinLevel(ord(level) + 1)

# All expression-level binary operators.
# FieldAccess and such are not included here as they are parsed below even unary
const binOps*: array[BinLevel, set[Tag]] = [
  {Assign, AddAssign, SubAssign, MulAssign, DivAssign, BitOrAssign, BitAndAssign, ShlAssign, ShrAssign},
  {Equal, NotEqual, Assert},
  {Less, Greater, GreaterEql, LessEql, Spaceship},
  {Or, Xor},
  {And},
  {OpenRange, ClosedRange},
  {Add, Sub},
  {Mul, Div, Mod},
  {BitOr, BitAnd, BitXor},
]

  

const unaryOps*: set[Tag] = {
  Sub, BitNot, Not,
  Const, Comptime, # Used for type expressions
  Array, Tag.Slice, Optional
}

const callOps*: set[Tag] = { LParen, LBracket }

const bindSpecs*: set[Tag] = {
  Let, Var, CVar, Field, Property, Enum
}

const atoms*: set[Tag] = {
  Symbol, NullLit, IntLit, StringLit
}

const validSymbolBeginnings*: set[char] = {
  '_', '@', 'a'..'z', 'A'..'Z'
}
const validSymbolChars*: set[char] = validSymbolBeginnings + {'0'..'9'}
# A stroke of insanity: I'll now parse float lits as field accesses into ints
const validNumLitChars*: set[char] = {'0'..'9'}

type
  FilePos* = object
    line*: uint
    col*: uint
  Tok* = object
    case tag*: Tag
    of Tag.Symbol, Tag.Label, Tag.IntLit, StringLit, CharLit:
      lexeme*: string
    else: discard
  Token* = tuple[what: Tok, where: FilePos]

