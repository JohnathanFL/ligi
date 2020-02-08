import streams
import strutils

import tags
import tokens

type
  Lexer* = ref object
    input: Stream
    pos: FilePos
    cur: char

# Return the old char
proc advance(self: var Lexer): char =
  result = self.cur
  self.cur = self.input.readChar()
  #echo "\tAdvanced past ", result

  if result == '\n':
    inc self.pos.line
    self.pos.col = 1
  else:
    inc self.pos.col

proc newLexer*(stream: Stream): Lexer =
  result = Lexer(
    pos: (line:0.uint, col:0.uint),
    input: stream,
    cur: '\0'
  )
  discard result.advance
  result.pos.line = 1
  result.pos.col = 1

template nextChar():char = self.cur # I'm gonna get diabetes soon with all this sugar
template nextIs(c: char): bool = self.cur == c

# Must be a template so the LParen can be returned
template skip(self: var Lexer) =
  var doneSkipping = false
  while not doneSkipping: # Whitespace and comments
    doneSkipping = true
    while self.cur in Whitespace: discard self.advance()
    let pos = self.pos # in case we have a Tag.LParen
    if nextIs '(':
      discard self.advance()
      if nextIs ':':
        doneSkipping = false
        while nextChar() != '\n': discard self.advance()
      else: # Oopsie! We just consumed a Tag.LParen. Better rectify that.
        return Token(pos: pos, tag: Tag.LParen)
proc scan*(self: var Lexer): Token =
  self.skip() # comments/whitespace
  let pos = self.pos
  if nextChar() in ValidSymbolBeginnings: # Symbol or keyword
    var lexeme = ""
    while nextChar() in ValidSymbolChars: lexeme &= self.advance()

    const words: set[Tag] = {
      Alias, And, Array, Assert, Break, Comptime, Const, CVar, DoWhile, ElIf, Else, Enum,
      Field, Finally, Fn, For, If, In, NotIn, Inline, Let, Loop, Not, NullLit, Property,
      Pure, Return, Tag.Slice, Struct, Test, Undef, Use, Var, Void, While, Xor, Sink
    }
    for word in words:
      if $word == lexeme:
        return Token(pos: pos, tag: word)
    return Token(pos: pos, tag: Symbol, lexeme: lexeme)
  elif nextChar() in Digits: # IntLit
    var lexeme = ""
    while nextChar() in Digits: lexeme &= self.advance()
    return Token(pos: pos, tag: IntLit, val: lexeme.parseInt.uint)
  elif nextIs  '`': # Label
    var lexeme = $self.advance()
    while nextChar() in ValidSymbolChars: lexeme &= self.advance()
    return Token(pos: pos, tag: Label, lexeme: lexeme)
  # Begin operator parsing
  # Remember '(' is already handled by the skipper
  else:
    template ret(t: Tag): untyped = return Token(pos: pos, tag: t)
    let cur = self.advance()
    case cur:
      of '\0': ret Tag.EOF
      of '{': ret Tag.LBrace
      of '}': ret Tag.RBrace
      of '(': ret Tag.LParen
      of ')': ret Tag.RParen
      of '[': ret Tag.LBracket
      of ']': ret Tag.RBracket
      of ':': ret Tag.Separator
      of '%': ret Tag.Mod
      of '?': ret Tag.Optional
      of '=':
        if nextIs '=':
          discard self.advance()
          ret Tag.Equal
        else: ret Tag.Assign
      of '+':
        if nextIs '=':
          discard self.advance()
          ret Tag.AddAssign
        else: ret Tag.Add
      of '-':
        if nextIs '=':
          discard self.advance()
          ret Tag.SubAssign
        elif nextIs '>':
          discard self.advance()
          ret Tag.StoreIn
        else: ret Tag.Sub
      of '*':
        if nextIs '=':
          discard self.advance()
          ret Tag.MulAssign
        else: ret Tag.Mul
      of '/':
        if nextIs '=':
          discard self.advance()
          ret Tag.DivAssign
        else: ret Tag.Div
      # All &, |, etc are TODO atm
      of '.':
        if nextIs '.':
          discard self.advance()
          if nextIs '=':
            discard self.advance()
            ret Tag.ClosedRange
          else: ret Tag.OpenRange
        else: ret Tag.Access
      of '>':
        if nextIs '=':
          discard self.advance()
          ret Tag.GreaterEq
        else: ret Tag.Greater
      of '<':
        if nextIs '=':
          discard self.advance()
          ret Tag.LessEq
        else: ret Tag.Less
      else: quit "UNEXPECTED CHARACTER AT " & $self.pos
