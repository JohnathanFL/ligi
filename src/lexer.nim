import streams
import strutils

import tokens

type
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


proc scan*(self: var Lexer): Token =
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
    result.what = Tok(tag: EOF)
    return


  if self.nextChar in validSymbolBeginnings:
    const words: seq[Tag] = @[
      Fn, PureFn, Not, And, Or, Xor, Let, Var, Field, Enum, Concept, NullLit, If,
      ElIf, Else, Comptime, Const, Undef, Tag.Slice, Array, Assert, Void, For, While, Loop, Finally, Struct, Enum,
      Break, Pure, Property, Use, Alias, In, CVar
    ]
    var lexeme = ""
    while self.nextChar in validSymbolChars: lexeme &= self.advance()
    for word in words:
      if $word == lexeme:
        result.what = Tok(tag: word)
        return
    result.what = Tok(tag: Symbol, lexeme: lexeme)
  elif self.nextChar in '0'..'9':
    var lexeme = ""
    while self.nextChar in validNumLitChars:
      lexeme &= self.advance()
    result.what = Tok(tag: Tag.IntLit)

    result.what.lexeme = lexeme
  elif self.nextEql "`":
    var lexeme = "" & self.advance() # Get the '`' in there
    while self.nextChar in validSymbolChars: lexeme &= self.advance()
    result.what = Tok(tag: Label, lexeme: lexeme)
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
    result.what = Tok(tag: StringLit, lexeme: lexeme)
  else:
    const ops: seq[Tag] = @[
      # Grouped by starting character
      AddAssign, Add,

      SubAssign, StoreIn, Sub,

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
      
      ClosedRange, OpenRange, FieldAccess,

      # All unrelated single characters
      Pound,
      Comma,
      Separator,
      Optional,
      Mod,
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

