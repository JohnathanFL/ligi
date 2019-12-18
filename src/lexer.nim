import macros
import streams
import strutils

type
  Tag* {.pure.}= enum 
    # Or/And-Assigns are Bitwise
    Add = "+"
    AddAssign = "+="
    And = "and"
    Assert = "==="
    Assign = "="
    BitAnd = "&"
    BitAndAssign = "&="
    BitNot = "~"
    BitOr = "|"
    BitOrAssign = "|="
    BitXor = "^"
    Block = "block"
    BoolLit = "BOOLLIT"
    CharLit = "CHARLIT"
    Comptime = "comptime"
    Concept = "concept"
    Const = "const"
    Div = "/"
    DivAssign = "/="
    Enum = "enum"
    Equal = "=="
    Field = "."
    FloatLit = "FLOATLIT"
    Fn = "fn"
    Greater = ">"
    GreaterEql = ">="
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
    Optional = "?" # These shall be actual operators
    Or = "or"
    PureFn = "purefn"
    RBrace = "}"
    RBracket = "]"
    RParen = ")"
    ShlAssign = "<<="
    ShrAssign = ">>="
    Spaceship = "<=>" # Spaceship only tentative
    StringLit = "STRLIT"
    Struct = "struct"
    Sub = "-"
    SubAssign = "-="
    Symbol = "SYM"
    Var = "var"
    Xor = "xor"

const binOps*: array[7, set[Tag]] = [
  {Assign, AddAssign, SubAssign, MulAssign, DivAssign, BitOrAssign, BitAndAssign, ShlAssign, ShrAssign},
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
    pos: FilePos
    lexeme: string
    tag: Tag

  Lexer* = object
    input: FileStream
    line*: uint
    col*: uint
    next: array[2, char] # Lookahead(2)

proc advance(self: var Lexer): char =
  result = self.next[0]
  self.next[0] = self.next[1]
  self.next[1] = self.input.readChar()
  inc self.col

proc advanceBy(self: var Lexer, by: uint): void =
  for i in 0..by: discard self.advance()

proc nextChar(self: var Lexer): char {.inline.} =
  return self.next[0]
proc peek(self: var Lexer): char {.inline.} =
  return self.next[1]

proc scan*(self: var Lexer): Token =
  var doneSkipping = false
  while not doneSkipping:
    doneSkipping = true
    while self.nextChar.isSpaceAscii:
      if self.nextChar == '\n': inc self.line
      discard self.advance()
    if self.nextChar == '(' and self.peek == ':' :
      doneSkipping = false # We found a comment that may have whitespace after it.
      var depth = 1
      while depth > 0:
        discard self.advance()
        if self.nextChar == ':' and self.peek == ')':
          dec depth
          self.advanceBy 2
        elif self.nextChar == '\n': depth = 0 # Note we don't advance here. We let the main line logic take it.
        elif self.nextChar == '(' and self.peek == ':':
          inc depth
          self.advanceBy 2
  # We are now definitely not about to start a comment or be in whitespace

  # The token starts here, so we copy the position here
  result.pos.line = self.line
  result.pos.col  = self.col

  if self.nextChar in validSymbolBeginnings:
    while self.nextChar in validSymbolChars: result.lexeme &= self.advance()
    result.tag = Tag.Symbol
    # TODO: Recognize keywords
  elif self.nextChar in '0'..'9':
    var isFloat = false
    while self.nextChar in validNumLitChars:
      if self.nextChar == '.': isFloat = true
      result.lexeme &= self.advance()
    result.tag = if isFloat: Tag.FloatLit else: Tag.IntLit
  else:
    proc recog(body: Call): Node =
      return
    macro recognize(body: untyped): untyped =
      result = nnkStmtList.newTree
      for node in body:
        result.add recog(node)
    recognize:
      '+':
        '=': Tag.AddAssign
        else: Tag.Add
      '-':
        '=': Tag.SubAssign
        else: Tag.Sub
      '*':
        '=': Tag.MulAssign
        else: Tag.Mul
      '/':
        '=': Tag.DivAssign
        else: Tag.Div
      '=':
        '=':
          '=': Tag.Assert
          else: Tag.Equal
        else: Tag.Equal
      
    
