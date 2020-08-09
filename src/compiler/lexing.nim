import strutils
import tables
import strformat


template alias(what: untyped, asWhat: untyped): untyped =
  template what():untyped {.dirty.} = asWhat
# TODO
template err(msg: string) =
  quit msg

const WordChars: set[char] = {'a'..'z', 'A'..'Z', '0'..'9', '_', '@'}


type Tag* = enum
  # Values
  tWord, tLabel, tStr, tChar,
  #Assignments
  tAssg, tAddAssg, tSubAssg, tMulAssg, tDivAssg,
  #Binaries
  tEq, tNotEq, tSpaceship, tGt, tLt, tGtEq, tLtEq,
  tOr, tXor, tAnd,
  tIn, tNotIn,
  tOpenRange, tClosedRange,
  tAdd, tSub, tMul, tDiv, tMod,
  tBitOr, tBitAnd, tBitXor,
  #Unaries
  tBitNot, tNot,

  tStruct, tRef, tSlice, tArray, tConst, tComptime,
  tOpt, tPure, tInline, tOverload, tProperty, tConcept,

  tBlock,
  # Special unary:
  tExpand,
  # Accesses
  tAccess, tPipe,
  # Binds
  tUsing, tLet, tVar, tCVar, tField, tEnum, tAlias, tPub,
  # Keywords
  tUse, tAssert, tBreak, tReturn, tDefer, tIf, tElIf, tElse, tWhen,
  tIs, tWhile, tLoop, tFor, tFinally, tFn, tMacro, tContinue,
  # Punctuation
  tColon, tComma, tStoreIn, tThen, tLParen, tRParen, tLBracket, tRBracket,
  tLBrace, tRBrace, tSemicolon,

  tEOF,

const Keywords = toTable {
  "alias": tAlias,
  "and": tAnd,
  "array": tArray,
  "assert": tAssert,
  "break": tBreak,
  "comptime": tComptime,
  "concept": tConcept,
  "const": tConst,
  "continue": tContinue,
  "cvar": tCVar,
  "defer": tDefer,
  "elif": tElIf,
  "else": tElse,
  "enum": tEnum,
  "field": tField,
  "finally": tFinally,
  "finally": tFinally,
  "fn": tFn,
  "for": tFor,
  "if": tIf,
  "in": tIn,
  "inline": tInline,
  "is": tIs,
  "let": tLet,
  "loop": tLoop,
  "macro": tMacro,
  "mod": tMod,
  "notin": tNotIn,
  "not": tNot,
  "or": tOr,
  "overload": tOverload,
  "property": tProperty,
  "pub": tPub,
  "pure": tPure,
  "ref": tRef,
  "return": tReturn,
  "slice": tSlice,
  "struct": tStruct,
  "use": tUse,
  "using": tUsing,
  "var": tVar,
  "when": tWhen,
  "while": tWhile,
  "xor": tXor,
}
# Must keep these sorted by len
const Sigils = toOrderedTable {
  "=>": tThen,
  "->": tStoreIn,
  "~": tBitNot,
  "?": tOpt,
  "+=": tAddAssg,
  "+": tAdd,
  "-=": tSubAssg,
  "-": tSub,
  "*=": tMulAssg,
  "*": tMul,
  "/=": tDivAssg,
  "/": tDiv,
  "..=": tClosedRange,
  "..": tOpenRange,
  ".": tAccess,
  "::": tPipe,
  ":": tColon,
  "<=>": tSpaceship,
  "<=": tLtEq,
  "<": tLt,
  ">=": tGtEq,
  ">": tGt,
  "==": tEq,
  "=": tAssg,
  "!=": tNotEq,
  "|": tBitOr,
  "&": tBitAnd,
  "^": tBitXor,
  "(": tLParen,
  ")": tRParen,
  "[": tLBracket,
  "]": tRBracket,
  "{": tLBrace,
  "}": tRBrace,
  ",": tComma,
  ";": tSemicolon,
  "$": tExpand,
}

# Level is the indentation of that line, even though we also do INDENT/DEDENT tokens
type Pos* = tuple[line: int, col: int]

# TODO: A "StringStash" styled object

type Token* = object
  case tag*: Tag
    of tWord, tLabel, tStr:
      str*: string
    of tChar:
      chr*: char
    else: discard

proc `$`*(t: Token): string =
  result = case t.tag:
    of tWord: fmt"""$"{t.str}""""
    of tStr: fmt "\"{t.str}\""
    of tChar: fmt"'{t.chr}'"
    else: repr(t.tag)
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
      self.pos.col = 0
    else:
      self.pos.col += 1
  # echo fmt"Advanced over {result}"
proc consume*(self: var Lexer, n = 1) =
  for i in 1..n: discard advance self
template consume(n: int) =
  self.consume n
template match(what: string) =
  if not nextIs what: err "Expected `" & what & "`"
  else: consume what.len

proc consumeWord(self: var Lexer): string =
  result = ""
  if self.cur notin WordChars: err "Expected a word char"
  while self.cur in WordChars:
    result &= self.advance


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

proc scanMLStr(self: var Lexer): Token =
  result = Token(tag: tStr, str: "")
  var multiline = false
  while nextIs "\\\\":
    consume 2
    if not nextIs " ": err "A \\\\ string literal must always have a space after the last \\"
    consume 1

    if multiline: result.str &= "\\n"
    multiline = true

    while not nextIs '\n': result.str &= self.advance
    self.skip

proc scanStr(self: var Lexer): Token =
  result = Token(tag: tStr, str: "")
  match "\""
  while not nextIs "\"":
    if nextIs "\n": err "A \"\" string literal may not have a newline in it!"
    elif nextIs "\\": result.str &= self.advance
    result.str &= self.advance
  match "\""


proc scanWord(self: var Lexer, checkKeyword: static[bool] = true): Token =
  result = Token(tag: tWord, str: self.consumeWord)
  if checkKeyword:
    for word, tag in Keywords:
      if word == result.str:
        return Token(tag: tag)


proc scan*(self: var Lexer): tuple[pos: Pos, tok: Token] =
  alias pos: result.pos
  alias token: result.tok
  template tok(t: Tag) =
    token = Token(tag:t)
    return
  template tok(t: Tag, s: string) =
    token = Token(tag: t, str: s)
    return

  self.skip
  pos = self.pos

  if nextIs "\0": tok tEOF
  elif nextIs WordChars: token = self.scanWord
  elif nextIs "\\\\": token = self.scanMLStr
  elif nextIs "\"": token = self.scanStr
  elif nextIs "#":
    consume 1
    if nextIs WordChars: tok tLabel, self.consumeWord
    elif nextIs "\"": tok tLabel, self.scanStr().str
    else: err "Expected either a word or a string literal"
  elif nextIs "\\":
    consume 1
    if not nextIs WordChars: err "A single \\ must be followed by a word to strop"
    token = self.scanWord(checkKeyword=false)
  else:
    for sigil, tag in Sigils:
      if nextIs sigil:
        consume sigil.len
        tok tag


proc lex*(data: iterator(): char {.closure.}): Lexer =
  result = Lexer(
    pos: (1, 0),
    data: data,
  )
  result.consume 3 # Kick the next buffer.

proc lex*(data: string): Lexer =
  iterator iter(): char {.closure.} =
    for c in data: yield c
  return iter.lex
