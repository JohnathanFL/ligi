import strutils
import tables
import strformat

import ast
import common_tags

# TODO: Better error handling
template err(msg: string) =
  quit msg

const WordChars: set[char] = {'a'..'z', 'A'..'Z', '0'..'9', '_', '@'}

# To form sigils like `+`, `=>`, or '<=>'
const SigilChars: set[char] = {
  '-', '+', '*', '/', '?', '=', '<', '>', '~',
  '$', ':', '.'
}

type Pos* = tuple[line: int, col: int, level: int]

type
  TokenKind* = enum
    tkEOF,
    tkWord, tkNumber,
    tkSigil, tkStr, tkChar, tkLabel,
    tkPunc,
    tkStrop, # A word which may have the characters of a keyword, but may not be interpreted as one
 # (may also be a sigil)

  Token* = object
    case kind*: TokenKind
      of tkWord, tkSigil, tkLabel, tkStrop, tkPunc:
        id*: StrID
      of tkStr:
        str*: string
      of tkChar:
        chr*: char
      of tkNumber:
        num*: uint
      else: discard


proc `$`*(t: Token): string =
  result = case t.kind:
    of tkWord: t.id.lookup
    of tkSigil: fmt"\{t.id.lookup}"
    of tkStrop: fmt"\{t.id.lookup}"
    of tkPunc: fmt"Punc'{t.id.lookup}'"
    of tkStr: fmt""""{t.str}""""
    of tkChar: fmt"'{t.chr}'"
    else: $t.kind
# proc `$`*(t: Token): string = repr(t.tag)

type Lexer* = object
  pos*: Pos
  # The *end* of the last token scanned
  lastPos*: Pos
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
      self.pos.col = 0
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

# Parse either a number or a word. Signal which in isStrID
# This is rather ugly, I know, but whatever.
# `stropped` controls whether it's always a word.
var str = ""
proc consumeWord(self: var Lexer, stropped = false): tuple[i: StrID, isStrID: bool] =
  result.isStrID = stropped
  str = ""
  var num = 0.uint
  if self.cur notin WordChars: err "Expected a word char"
  while self.cur in WordChars:
    let c = self.advance
    if not c.isDigit:
      result.isStrID = true
    else:
      num *= 10
      num += c.uint - '0'.uint
    str &= c
  if result.isStrID:
    result.i = str.toID
  else:
    result.i = num
proc consumeSigil(self: var Lexer): StrID =
  var str = ""
  if self.cur notin SigilChars: err "Expected a sigil char"
  while self.cur in SigilChars:
    str &= self.advance
  return str.toID


proc skip(self: var Lexer) =
  let lastLine = self.pos.line
  var dirty = true
  while dirty:
    dirty = false
    while self.cur.isSpaceAscii:
      consume 1
      dirty = true
    if nextIs "--":
      dirty = true
      while not nextIs "\n": consume 1
  if self.pos.line != lastLine:
    self.pos.level = self.pos.col

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

proc scanStr(self: var Lexer): string =
  result = ""
  match "\""
  while not nextIs "\"":
    if nextIs "\n": err "A \"\" string literal may not have a newline in it!"
    elif nextIs "\\": result &= self.advance
    result &= self.advance
  match "\""


type TupTok* = tuple[
  # Where is the token?
  pos: Pos,
  # Was it attached to the previous token? (i.e no whitespace between)
  attached: bool,
  # What is it?
  tok: Token
]
proc scan*(self: var Lexer): TupTok =
  template tok(t: TokenKind) =
    result.tok = Token(kind: t)
  template tok(t: TokenKind, i: StrID) =
    result.tok = Token(kind: t, id: i)
  template tok(i: StrID) = tok tkWord, i
  template tok(s: string) =
    result.tok = Token(kind: tkStr, str: s)

  self.skip
  result.attached = self.pos == self.lastPos
  result.pos = self.pos

  if nextIs "\0": tok tkEOF
  elif tryMatch "(": tok tkPunc, iLParen
  elif tryMatch ")": tok tkPunc, iRParen
  elif tryMatch "[": tok tkPunc, iLBracket
  elif tryMatch "]": tok tkPunc, iRBracket
  elif tryMatch "{": tok tkPunc, iLBrace
  elif tryMatch "}": tok tkPunc, iRBrace
  elif tryMatch ",": tok tkPunc, iComma
  elif nextIs WordChars:
    let (i, isWord) = self.consumeWord
    if isWord: tok i
    else: result.tok = Token(kind: tkNumber, num: i.uint)
    # TODO
    # token.prec = token.id.opPrec
  elif nextIs "\\\\": tok self.scanMLStr
  elif nextIs "\"": tok self.scanStr
  elif nextIs "#":
    consume 1
    if nextIs WordChars:
      tok tkLabel, self.consumeWord(stropped=true).i
    elif nextIs SigilChars:
      tok tkLabel, self.consumeSigil
    elif nextIs "\"": tok tkLabel, self.scanStr.toID
    else: err "Expected either a word or a string literal"
  elif nextIs "\\":
    consume 1
    if not nextIs WordChars + SigilChars:
      err "A single \\ must be followed by a word or sigil to strop"
    if nextIs WordChars:
      tok tkStrop, self.consumeWord(stropped=true).i
    else:
      tok tkStrop, self.consumeSigil
  elif nextIs SigilChars:
    tok tkSigil, self.consumeSigil
    if result.tok.id == iStoreIn:
      result.tok.kind = tkPunc
    elif result.tok.id == iColon:
      result.tok.kind = tkPunc
    #token.prec = token.id.opPrec
  self.lastPos = self.pos
  # echo result


proc lex*(data: iterator(): char {.closure.}): Lexer =
  result = Lexer(
    pos: (0, 0, 0),
    lastPos: (-1, -1, 0),
    data: data,
  )
  result.consume 3 # Kick the next buffer.

proc lex*(data: string): Lexer =
  iterator iter(): char {.closure.} =
    for c in data: yield c
  return iter.lex
