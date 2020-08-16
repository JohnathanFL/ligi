import strutils
import tables
import strformat

import ast

# Lexing specific tags. No need for the AST in general to know about them
makeTags {
  iLParen: "(",
  iRParen: ")",
  iLBracket: "[",
  iRBracket: "]",
  iLBrace: "{",
  iRBrace: "}",
  iComma: ",",
}

template alias(what: untyped, asWhat: untyped): untyped =
  template what():untyped {.dirty.} = asWhat
# TODO
template err(msg: string) =
  quit msg

const WordChars: set[char] = {'a'..'z', 'A'..'Z', '0'..'9', '_', '@'}

# To form sigils like `+`, `=>`, or '<=>'
const SigilChars: set[char] = {
  '-', '+', '*', '/', '?', '=', '<', '>', '~',
  '$', ':', '.'
}

type
  TokFlag* = enum
    tfWord, # A word, i.e not a sigil
    tfAssg, # An assignment-type operator
    tfBinOp, # A binary infix operator
    tfUnaOp, # A unary prefix operator
  TokFlags* = set[TokFlag]


type Pos* = tuple[line: int, col: int]

type
  TokenKind* = enum
    tkEOF,
    tkWord, tkStr, tkChar, tkLabel,
    tkStrop, # A word which may have the characters of a keyword, but may not be interpreted as one
             # (may also be a sigil)

  Token* = object
    pos*: Pos
    flags*: TokFlags
    case kind*: TokenKind
      of tkWord, tkLabel, tkStrop:
        id*: StringID
        prec*: int # Operator precedence, or -1
      of tkStr:
        str*: string
      of tkChar:
        chr*: char
      else: discard


proc `$`*(t: Token): string =
  result = case t.kind:
    of tkWord: t.id.lookup
    of tkStr: fmt""""{t.str}""""
    of tkChar: fmt"'{t.chr}'"
    else: $t.kind
# proc `$`*(t: Token): string = repr(t.tag)

type Lexer* = object
  pos*: Pos
  curLevel*: int
  data*: iterator(): char
  next*: array[3, char]

proc cur(self: Lexer): char = self.next[0]
proc nextIs(self: Lexer, what: string): bool =
  assert what.len <= 3, "Only 3 char lookahead"
  for i, c in what:
    if c != self.next[i]: return false
  return true
proc nextIs(self: Lexer, charset: static[set[char]]): bool = self.cur in charset
proc nextIs(self: Lexer, c: char): bool = self.cur == c
template nextIs(x: untyped): bool = self.nextIs(x)

proc advance(self: var Lexer): char =
  ## Returns the character we just passed
  result = self.cur
  self.next[0] = self.next[1]
  self.next[1] = self.next[2]
  self.next[2] = if self.data.finished: '\0' else: self.data()

  # handle file position
  # only need to advance col if we aren't already at the end
  if result != '\0':
    if result == '\n':
      self.pos.line += 1
      self.pos.col = 1
    else:
      self.pos.col += 1
  # echo fmt"Advanced over {result}"
proc consume*(self: var Lexer, n = 1) =
  for i in 1..n: discard advance self
template consume(n: int) =
  self.consume n
template tryMatch(what: string): bool =
  if not nextIs what: false
  else:
    consume what.len
    true
template match(what: string) =
  if not tryMatch what:
    err fmt"Expected " & what

proc consumeWord(self: var Lexer): StringID =
  var str = ""
  if self.cur notin WordChars: err "Expected a word char"
  while self.cur in WordChars:
    str &= self.advance
  return str.toID
proc consumeSigil(self: var Lexer): StringID =
  var str = ""
  if self.cur notin SigilChars: err "Expected a sigil char"
  while self.cur in SigilChars:
    str &= self.advance
  return str.toID


proc skip(self: var Lexer) =
  var dirty = true
  while dirty:
    dirty = false
    while self.cur.isSpaceAscii:
      consume 1
      dirty = true
    if nextIs "--":
      dirty = true
      while not nextIs "\n": consume 1

proc scanMLStr(self: var Lexer): string =
  result = ""
  var multiline = false
  while nextIs "\\\\":
    consume 2
    if nextIs "\n":
      if multiline: result &= "\\n"
    elif not nextIs " ":
      err "A \\\\ string literal must always have a space after the last \\"
    else:
      consume 1
      if multiline: result &= "\\n"
      multiline = true

      while not nextIs '\n': result &= self.advance
      self.skip

proc scanStr(self: var Lexer): string =
  result = ""
  match "\""
  while not nextIs "\"":
    if nextIs "\n": err "A \"\" string literal may not have a newline in it!"
    elif nextIs "\\": result &= self.advance
    result &= self.advance
  match "\""


proc scan*(self: var Lexer): tuple[pos: Pos, tok: Token] =
  alias pos: result.pos
  alias token: result.tok

  template tok(t: TokenKind) =
    token = Token(kind: t)
  template tok(t: TokenKind, i: StringID) =
    token = Token(kind: t, id: i)
  template tok(i: StringID) = tok tkWord, i
  template tok(s: string) =
    token = Token(kind: tkStr, str: s)

  self.skip
  pos = self.pos

  if nextIs "\0": tok tkEOF
  elif tryMatch "(": tok iLParen
  elif tryMatch ")": tok iRParen
  elif tryMatch "[": tok iLBracket
  elif tryMatch "]": tok iRBracket
  elif tryMatch "{": tok iLBrace
  elif tryMatch "}": tok iRBrace
  elif tryMatch ",": tok iComma
  elif nextIs WordChars:
    tok self.consumeWord
    token.flags.incl tfWord
    let prec = token.id.opPrec
    if prec != -1:
      token.prec = prec
      token.flags.incl tfBinOp
  elif nextIs "\\\\": tok self.scanMLStr
  elif nextIs "\"": tok self.scanStr
  elif nextIs "#":
    consume 1
    if nextIs WordChars:
      tok tkLabel, self.consumeWord
    elif nextIs SigilChars:
      tok tkLabel, self.consumeSigil
    elif nextIs "\"": tok tkLabel, self.scanStr.toID
    else: err "Expected either a word or a string literal"
  elif nextIs "\\":
    consume 1
    if not nextIs WordChars + SigilChars:
      err "A single \\ must be followed by a word or sigil to strop"
    if nextIs WordChars:
      tok tkStrop, self.consumeWord
    else:
      tok tkStrop, self.consumeSigil
  elif nextIs SigilChars:
    tok self.consumeSigil
    let prec = token.id.opPrec
    if prec != -1:
      token.prec = prec
      token.flags.incl tfBinOp


proc lex*(data: iterator(): char {.closure.}): Lexer =
  result = Lexer(
    pos: (1, 1),
    data: data,
  )
  result.consume 3 # Kick the next buffer.

proc lex*(data: string): Lexer =
  iterator iter(): char {.closure.} =
    for c in data: yield c
  return iter.lex
