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
  for i in 0..<by:
    discard self.advance()

proc newLexer*(stream: Stream): Lexer =
  result = Lexer(
    line: 1,
    col: 1,
    next: [0.char, 0.char, 0.char],
    input: stream,
  )

  result.advanceBy(3)
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
  let pos = (line: self.line, col: self.col)
  if self.nextChar == '\0':
    result = Token(pos: pos, tag: EOF)
    return


  if self.nextChar in validSymbolBeginnings:
    const words: seq[Tag] = @[
      Alias, And, Array, Assert, Break, Comptime, Const, CVar, DoWhile,
      ElIf, Else, Enum, Enum, Field, Finally, Fn, For, If, In,
      Inline, Let, Loop, Not, NotIn, NullLit, Or, Proc, Property, Pure,
      Return, Sink, Struct, Tag.Slice, Undef, Use, Var, Void, While,
      Xor,
    ]
    var lexeme = ""
    while self.nextChar in validSymbolChars: lexeme &= self.advance()
    for word in words:
      if $word == lexeme:
        result = Token(pos: pos, tag: word)
        return
    result = Token(pos: pos, tag: Symbol, lexeme: lexeme)
  elif self.nextChar in '0'..'9':
    var lexeme = ""
    while self.nextChar in validNumLitChars:
      lexeme &= self.advance()
    result = Token(pos: pos, tag: Tag.IntLit)

    result.lexeme = lexeme
  elif self.nextEql "`":
    var lexeme = "" & self.advance() # Get the '`' in there
    while self.nextChar in validSymbolChars: lexeme &= self.advance()
    result = Token(pos: pos, tag: Label, lexeme: lexeme)
  elif self.nextEql "#\"":
    #Stropping
    var lexeme = ""
    self.advanceBy(2)
    # TODO: Iron out the exact rules for what's allowed in a strop
    while self.nextChar != '"': lexeme &= self.advance
    discard self.advance
    result = Token(pos: pos, tag: Symbol, lexeme: lexeme)
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
      echo "ERROR: Newline terminated string literal at ", result.pos
    discard self.advance # Skip the '"'
    result = Token(pos: pos, tag: StringLit, lexeme: lexeme)
  else:
    const ops: seq[Tag] = @[
      # Grouped by starting character
      AddAssign, Add,

      SubAssign, StoreIn, Sub,

      MulAssign, Mul,
      DivAssign, Div,
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
        result.tag = t
        # -1 because \0
        self.advanceBy (($t).len)
        break
      result.tag = Tag.INVALID_TAG
  #echo "Scanned a ", result

