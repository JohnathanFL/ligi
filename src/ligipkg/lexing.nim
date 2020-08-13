import strutils
import tables
import strformat
import algorithm
import sets


template alias(what: untyped, asWhat: untyped): untyped =
  template what():untyped {.dirty.} = asWhat
# TODO
template err(msg: string) =
  quit msg

const WordChars: set[char] = {'a'..'z', 'A'..'Z', '0'..'9', '_', '@'}


type Tag* = enum
  # Values
  tWord = "@WORD@",
  tLabel = "@LABEL@",
  tStr = "@STR@",
  tChar = "@CHAR@",
  #Assignments
  tAssg = "=",
  tAddAssg = "+=",
  tSubAssg = "-=",
  tMulAssg = "*=",
  tDivAssg = "/=",
  #Binaries
  tOr = "or",
  tXor = "xor",
  tAnd = "and",
  tEq = "==",
  tNotEq = "!=",
  tSpaceship = "<=>",
  tGt = ">",
  tLt = "<",
  tGtEq = ">=",
  tLtEq = "<=",
  tIn = "in",
  tNotIn = "notin",
  tOpenRange = "..",
  tClosedRange = "..=",
  tAdd = "+",
  tSub = "-",
  tMul = "*",
  tDiv = "/",
  tMod = "mod",
  tBitOr = "|",
  tBitAnd = "&",
  tBitXor = "^",
  #Unaries
  tBitNot = "~",
  tNot = "not",

  tStruct = "struct",
  tRef = "ref",
  tSlice = "slice",
  tArray = "array",
  tConst = "const",
  tComptime = "comptime",
  tOpt = "?",
  tPure = "pure",
  tInline = "inline",
  tOverload = "overload",
  tProperty = "property",
  tConcept = "concept",

  tBlock = "block",
  # Special unary:
  tExpand = "$",
  # Accesses
  tAccess = ".",
  tPipe = "::",
  # Binds
  tUsing = "using",
  tLet = "let",
  tVar = "var",
  tCVar = "cvar",
  tField = "field",
  tEnum = "enum",
  tAlias = "alias",
  tPub = "pub",
  # Keywords
  tUse = "use",
  tAssert = "assert",
  tBreak = "break",
  tReturn = "return",
  tDefer = "defer",
  tIf = "if",
  tElIf = "elif",
  tElse = "else",
  tWhen = "when",
  tIs = "is",
  tWhile = "while",
  tLoop = "loop",
  tFor = "for",
  tFinally = "finally",
  tFn = "fn",
  tMacro = "macro",
  tContinue = "continue",
  # Punctuation
  tColon = ":",
  tComma = ",",
  tStoreIn = "->",
  tThen = "=>",
  tLParen = "(",
  tRParen = ")",
  tLBracket = "[",
  tRBracket = "]",
  tLBrace = "{",
  tRBrace = "}",
  tSemicolon = ";",

  tEOF = "@EOF@",

proc extractAllKeywords(): set[Tag] {.compileTime.} =
  for tag in Tag.low..Tag.high:
    let first = ($tag)[0]
    if first.isAlphaAscii: result.incl tag
const KeywordSet* = extractAllKeywords()
proc mapAllKeywords(): Table[string, Tag] {.compileTime.} =
  for tag in KeywordSet: result.add($tag, tag)
const KeywordMap* = mapAllKeywords()

proc extractAllSigils(): set[Tag] {.compileTime.} =
  for tag in Tag.low..Tag.high:
    let first = ($tag)[0]
    if not first.isAlphaAscii() and not (first == '@'): result.incl tag
const SigilSet* = extractAllSigils()
proc orderAllSigils(): OrderedSet[Tag] {.compileTime.} =
  var allSigils: seq[Tag]
  for tag in SigilSet:
    allSigils.add tag
  # Sort by len. Does it ascending
  allSigils.sort do (x, y: Tag) -> int:
    result = cmp($x, $y)
  while allSigils.len > 0: result.incl allSigils.pop


# Must keep these sorted by len
const SigilList* = orderAllSigils()

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
      self.pos.col = 1
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
    if nextIs "\n":
      if multiline: result.str &= "\\n"
    elif not nextIs " ":
      err "A \\\\ string literal must always have a space after the last \\"
    else:
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
    if KeywordMap.contains result.str:
      result = Token(tag: KeywordMap[result.str])


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
    for tag in SigilList:
      if nextIs $tag:
        consume ($tag).len
        tok tag


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
