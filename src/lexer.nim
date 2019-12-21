import streams
import strutils
import gara

type
  Tag* {.pure.} = enum
    # Or/And-Assigns are Bitwise
    Semicolon = ";"
    Comma = ","
    Add = "+"
    AddAssign = "+="
    And = "and"
    AShr = ">>>"
    Assert = "==="
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
    BoolLit = "BOOLLIT"
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
    FloatLit = "FLOATLIT"
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
    Not = "!"
    NotEqual = "!="
    NullLit = "null"
    Optional = "?"    # These shall be actual operators
    Or = "or"
    PureFn = "purefn"
    RBrace = "}"
    RBracket = "]"
    RParen = ")"
    Separator = ":"
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

    INVALID_TAG = "INVALID"


# Expression hierarchy is Binary->Unary->Access->(Repeat in parens)

# All expression-level binary operators.
# FieldAccess and such are not included here as they are parsed below even unary
const binOps*: seq[set[Tag]] = @[
  {Assign, AddAssign, SubAssign, MulAssign, DivAssign, BitOrAssign,
      BitAndAssign, ShlAssign, ShrAssign},
  {Equal, NotEqual, Assert},
  {Less, Greater, GreaterEql, LessEql, Spaceship},
  {Or, And, Xor},
  {Add, Sub},
  {Mul, Div, Mod},
  {BitOr, BitAnd, BitXor},
]

const unaryOps*: set[Tag] = {
  Sub, BitNot, Not,
  Const, Comptime # Used for type expressions
}

const validSymbolBeginnings: set[char] = {
  '_', '@', 'a'..'z', 'A'..'Z'
}
const validSymbolChars: set[char] = validSymbolBeginnings + {'0'..'9'}
const validNumLitChars: set[char] = {'0'..'9', '.'}

type
  FilePos* = tuple[line: uint, col: uint]
  Token* = object
    case tag*: Tag
    of Tag.Symbol, Tag.Label, Tag.IntLit, Tag.FloatLit, StringLit, CharLit:
      lexeme: string
    else: discard

  Lexer* = object
    input: Stream
    line*: uint
    col*: uint
    next: array[3, char] # Lookahead(3). 3 is only for ===

proc advance(self: var Lexer): char =
  result = self.next[0]
  self.next[0] = self.next[1]
  self.next[1] = self.next[2]
  self.next[2] = self.input.readChar()

  if self.next[0] == '\n':
    inc self.line
    self.col = 0
  else:
    inc self.col
  #stdout.write self.next[0]

proc advanceBy(self: var Lexer, by: int): void =
  for i in 0..by: discard self.advance()

proc newLexer*(stream: Stream): Lexer =
  result = Lexer(
    line: 1,
    col: 1,
    next: [0.char, 0.char, 0.char],
    input: stream,
  )

  result.advanceBy(2)
  result.line = 1
  result.col = 1


proc nextChar(self: var Lexer): char {.inline.} =
  return self.next[0]
proc peek(self: var Lexer): char {.inline.} =
  return self.next[1]

proc nextEql(self: var Lexer, pat: string): bool {.inline.} =
  for i, c in pat:
    if self.next[i] != c: return false
  return true


proc scan*(self: var Lexer): tuple[where: FilePos, what: Token] =
  var doneSkipping = false
  while not doneSkipping:
    doneSkipping = true
    while self.nextChar.isSpaceAscii: discard self.advance()
    if self.nextEql("(:") or self.nextEql "(=": # TODO: Treat doc comments as literals
      doneSkipping = false # We found a comment that may have whitespace after it.
      var depth = 1
      while depth > 0:
        discard self.advance()
        if self.nextEql ":)":
          dec depth
          self.advanceBy 2
        elif self.nextChar == '\n': depth = 0 # Note we don't advance here. We let the main line logic take it.
        elif self.nextEql "(:":
          inc depth
          self.advanceBy 2
  # We are now definitely not about to start a comment or be in whitespace, but may be EOF

  # The token starts here, so we copy the position here
  result.where.line = self.line
  result.where.col = self.col
  if self.nextChar == '\0':
    result.what = Token(tag: EOF)
    return


  if self.nextChar in validSymbolBeginnings:
    const words: seq[Tag] = @[
      Fn, PureFn, And, Or, Xor, Let, Var, Field, Enum, Concept, NullLit, If,
      ElIf, Else, Comptime, Const, Undef
    ]
    var lexeme = ""
    while self.nextChar in validSymbolChars: lexeme &= self.advance()
    for word in words:
      if $word == lexeme:
        result.what = Token(tag: word)
        return
    result.what = Token(tag: Symbol, lexeme: lexeme)
  elif self.nextChar in '0'..'9':
    var lexeme = ""
    var isFloat = false
    while self.nextChar in validNumLitChars:
      if self.nextChar == '.': isFloat = true
      lexeme &= self.advance()
    if isFloat: result.what = Token(tag: Tag.FloatLit)
    else: result.what = Token(tag: Tag.IntLit)

    result.what.lexeme = lexeme
  elif self.nextEql "`":
    var lexeme = "" & self.advance() # Get the '`' in there
    while self.nextChar in validSymbolChars: lexeme &= self.advance()
    result.what = Token(tag: Label, lexeme: lexeme)
  elif self.nextChar == '"':
    var lexeme = ""
    discard self.advance()
    while self.nextChar notin {'"', '\n'}:
      lexeme &= self.nextChar
      if self.nextChar == '\\':
        discard self.advance
        lexeme &= self.nextChar
      discard self.advance
    if self.nextChar == '\n':
      echo "ERROR: Newline terminated string literal at ", result.where
    discard self.advance # Skip the '"'
    result.what = Token(tag: StringLit, lexeme: lexeme)
  else:
    const ops: seq[Tag] = @[
      # Grouped by starting character
      AddAssign, Add,

      SubAssign, Sub,

      Mul, MulAssign,

      Div, DivAssign,

      NotEqual,

      Assert, Equal, Assign,

      AShr, ShrAssign, Shr, GreaterEql, Greater,

      ShlAssign, Shl, Spaceship, LessEql, Less,

      BitAndAssign, BitAnd,

      BitOrAssign, BitOr,

      BitNotAssign, BitNot,

      BitXorAssign, BitXor,

      # All unrelated single characters
      Comma,
      Separator,
      Optional,
      Mod,
      FieldAccess,
      LBrace,
      RBrace,
      LBracket,
      RBracket,
      LParen,
      RParen,
      Semicolon,
    ]
    for t in ops:
      if self.nextEql $t:
        result.what.tag = t
        # -1 because \0
        self.advanceBy (($t).len - 1)
        break
      result.what.tag = Tag.INVALID_TAG

