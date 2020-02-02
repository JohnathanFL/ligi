import strutils
import nodes

type
  Tag* {.pure.} = enum
    # Leaving this here, whether as a monument to my insanity or brilliance I don't know
    # Float literals are now parsed as field accesses into an int
    # (i.e every int has an infinite number of fields, each named for an int)
    # FloatLit = "FLOATLIT"

    


    
    Access = (Command.Access , ".")
    Add = (Command.Add , "+")
    AddAssign = (Command.AddAssign , "+=")
    Alias = (Command.Alias , "alias")
    And = (Command.And , "and")
    Array = (Command.Array , "array")
    AShr = (Command.AShr , ">>>")
    Assert = (Command.Assert , "assert")
    Assign = (Command.Assign , "=")
    BitAnd = (Command.BitAnd , "&")
    BitAndAssign = (Command.BitAndAssign , "&=")
    BitNot = (Command.BitNot , "~")
    BitNotAssign = (Command.BitNotAssign , "~=")
    BitOr = (Command.BitOr , "|")
    BitOrAssign = (Command.BitOrAssign , "|=")
    BitXor = (Command.BitXor , "^")
    BitXorAssign = (Command.BitXorAssign , "^=")
    Break = (Command.Break , "break")
    CharLit = (Command.Char, "CHARLIT")
    ClosedRange = (Command.ClosedRange , "..=")
    Comptime = (Command.Comptime , "comptime")
    Const = (Command.Const , "const")
    CVar = (Command.CVar , "cvar")
    Div = (Command.Div , "/")
    DivAssign = (Command.DivAssign , "/=")
    DoWhile = (Command.DoWhile , "dowhile")
    Equal = (Command.Equal , "==")
    Field = (Command.Field , "field")
    For = (Command.For , "for")
    Greater = (Command.Greater , ">")
    GreaterEq = (Command.GreaterEq , ">=")
    If = (Command.If , "if")
    In = (Command.In , "in")
    Inline = (Command.Inline , "inline")
    IntLit = (Command.Int , "INTLIT")
    Less = (Command.Less , "<")
    LessEq = (Command.LessEq , "<=")
    Let = (Command.Let , "let")
    Loop = (Command.Loop , "loop")
    Mod = (Command.Mod , "%")
    Mul = (Command.Mul , "*")
    MulAssign = (Command.MulAssign , "*=")
    Not = (Command.Not , "not")
    NotEqual = (Command.NotEqual , "!=")
    NotIn = (Command.NotIn , "notin")
    NullLit = (Command.Null , "null")
    OpenRange = (Command.OpenRange , "..")
    Optional = (Command.Optional , "?")    # These shall be actual operators
    Or = (Command.Or , "or")
    Property = (Command.Property , "property")
    Pure = (Command.Pure , "pure")
    Return = (Command.Return , "return")
    Shl = (Command.Shl , "<<")
    ShlAssign = (Command.ShlAssign , "<<=")
    Shr = (Command.Shr , ">>")
    ShrAssign = (Command.ShrAssign , ">>=")
    Sink = (Command.Sink , "_")
    Slice = (Command.Slice , "slice")
    Spaceship = (Command.Spaceship , "<=>") # Spaceship only tentative
    StringLit = (Command.String , "STRLIT")
    Struct = (Command.StructDef , "struct")
    Sub = (Command.Sub , "-")
    SubAssign = (Command.SubAssign , "-=")
    Symbol = (Command.Symbol , "SYM")
    Test = (Command.Test , "test") # TODO
    Undef = (Command.Undef , "undef")
    Use = (Command.Use , "use")
    Var = (Command.Var , "var")
    Void = (Command.Void , "void") # Can't be a symbol since we need to be able to do 'fn -> void' without a bind
    While = (Command.While , "while")
    Xor = (Command.Xor , "xor")

    
    # Punctuation
    # These don't map (directly) to any actual commands, and only serve to disambiguate the syntax
    # (They're mapped >100 since no commands are that high)
    StoreIn = (100, "->")
    RParen = ")"
    Pound = "#"
    LParen = "("
    Label = "LABEL"
    Fn = "fn"
    Finally = "finally"
    EOF = "EOF"
    Comma = ","
    ElIf = "elif"
    Else = "else"
    LBrace = "{"
    LBracket = "["
    RBrace = "}"
    RBracket = "]"
    Semicolon = ";"
    Separator = ":"
    
    Enum = "enum" # Cannot be mapped directly, as it could be either enum or enumdef


    INVALID_TAG = "INVALID"


# Expression hierarchy is Binary->Unary->Access->(Repeat in parens)


type BinLevel*{.pure.} = enum
  Assignment, Equality, Relational, Ors, Ands, Membership, Range, Arithmetic, Product, Bitwise

template below*(level: BinLevel): BinLevel =
  BinLevel(ord(level) + 1)

# All expression-level binary operators.
# FieldAccess and such are not included here as they are parsed below even unary
const BinOps*: array[BinLevel, set[Tag]] = [
  {Tag.Assign, Tag.AddAssign, Tag.SubAssign, Tag.MulAssign, Tag.DivAssign, Tag.BitOrAssign, Tag.BitAndAssign, Tag.ShlAssign, Tag.ShrAssign},
  {Tag.Equal, Tag.NotEqual, Tag.Assert},
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
  Tag.Pure, Tag.Inline,
  # As pointer
  Tag.Mul
}

const CallOps*: set[Tag] = { Tag.LParen, Tag.LBracket }

const BindSpecs*: set[Tag] = {
  Tag.Let, Tag.Var, Tag.CVar, Tag.Field, Tag.Property, Tag.Enum
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

