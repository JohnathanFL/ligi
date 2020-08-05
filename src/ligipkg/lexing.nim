import strutils
import tables
import strformat


template alias(what: untyped, asWhat: untyped): untyped =
  template what():untyped {.dirty.} = asWhat
template dowhile(cond: untyped, body: untyped): untyped =
  body
  while cond: body
# TODO
template err(msg: string) =
  quit msg

const WordChars: set[char] = {'a'..'z', 'A'..'Z', '0'..'9', '_', '@'}


type Tag* = enum
  # Values
  Word, Label, Str, Char,
  # Assignments
  Assg, AddAssg, SubAssg, MulAssg, DivAssg,
  # Binaries
  Eq, NotEq, Gt, Lt, GtEq, LtEq, Spaceship,
  Or, Xor, And,
  In, NotIn,
  OpenRange, ClosedRange,
  Add, Sub, Mul, Div, Mod,
  BitOr, BitAnd, BitXor,
  #Unaries
  Struct, Ref, Slice, Array, Const, Comptime,
  Opt, Pure, Inline, Overload, Property, Concept,
  BitNot, Not,
  # Special unary:
  Expand,
  # Accesses
  Access, Pipe,
  # Binds
  Using, Let, Var, CVar, Field, Enum, Alias, Pub,
  # Keywords
  Use, Assert, Break, Return, Defer, If, ElIf, Else, When,
  Is, While, Loop, For, Finally, Fn, Macro, Continue,
  # Punctuation
  Colon, Comma, StoreIn, Then, LParen, RParen, LBracket, RBracket,
  LBrace, RBrace, Semicolon,

  Indent, Dedent,

  EOF,

const Keywords = toTable {
  "alias": Alias,
  "and": And,
  "array": Array,
  "assert": Assert,
  "break": Break,
  "comptime": Comptime,
  "concept": Concept,
  "const": Const,
  "continue": Continue,
  "cvar": CVar,
  "defer": Defer,
  "elif": ElIf,
  "else": Else,
  "enum": Enum,
  "field": Field,
  "finally": Finally,
  "finally": Finally,
  "fn": Fn,
  "for": For,
  "if": If,
  "in": In,
  "inline": Inline,
  "is": Is,
  "let": Let,
  "loop": Loop,
  "macro": Macro,
  "mod": Mod,
  "notin": NotIn,
  "not": Not,
  "or": Or,
  "overload": Overload,
  "property": Property,
  "pub": Pub,
  "pure": Pure,
  "ref": Ref,
  "return": Return,
  "slice": Slice,
  "struct": Struct,
  "use": Use,
  "using": Using,
  "var": Var,
  "when": When,
  "while": While,
  "xor": Xor,
}
# Must keep these sorted by len
const Sigils = toOrderedTable {
  "=>": Then,
  "->": StoreIn,
  "~": BitNot,
  "?": Opt,
  "+=": AddAssg,
  "+": Add,
  "-=": SubAssg,
  "-": Sub,
  "*=": MulAssg,
  "*": Mul,
  "/=": DivAssg,
  "/": Div,
  "..=": ClosedRange,
  "..": OpenRange,
  ".": Access,
  "::": Pipe,
  ":": Colon,
  "<=>": Spaceship,
  "<=": LtEq,
  "<": Lt,
  ">=": GtEq,
  ">": Gt,
  "==": Eq,
  "=": Assg,
  "!=": NotEq,
  "|": BitOr,
  "&": BitAnd,
  "^": BitXor,
  "(": LParen,
  ")": RParen,
  "[": LBracket,
  "]": RBracket,
  "{": LBrace,
  "}": RBrace,
  ",": Comma,
  ";": Semicolon,
  "$": Expand,
}

# Level is the indentation of that line, even though we also do INDENT/DEDENT tokens
type Pos* = tuple[line: int, level: int, col: int]

# TODO: A "StringStash" styled object

type Token* = object
  case tag*: Tag
    of Word, Label, Str:
      str*: string
    of Char:
      chr*: char
    else: discard

proc `$`*(t: Token): string =
  result = case t.tag:
    of Word: fmt"""$"{t.str}""""
    of Str: fmt "\"{t.str}\""
    of Char: fmt"'{t.chr}'"
    else: repr(t.tag)
# proc `$`*(t: Token): string = repr(t.tag)

type Lexer* = object
  pos*: Pos
  lastPos*: Pos
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
  result = Token(tag: Str, str: "")
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
  result = Token(tag: Str, str: "")
  match "\""
  while not nextIs "\"":
    if nextIs "\n": err "A \"\" string literal may not have a newline in it!"
    elif nextIs "\\": result.str &= self.advance
    result.str &= self.advance
  match "\""


proc scanWord(self: var Lexer, checkKeyword: static[bool] = true): Token =
  result = Token(tag: Word, str: self.consumeWord)
  if checkKeyword:
    for word, tag in Keywords:
      if word == result.str:
        return Token(tag: tag)


proc scan*(self: var Lexer): tuple[pos: Pos, tok: Token] =
  alias pos: result.pos
  alias token: result.tok
  template tok(t: Tag) =
    token = Token(tag:t)
    self.lastPos = pos
    return
  template tok(t: Tag, s: string) =
    token = Token(tag: t, str: s)
    self.lastPos = pos
    return

  self.skip
  pos = self.pos
  if pos.line > self.lastPos.line:
    self.pos.level = self.pos.col
    pos = self.pos
    if pos.level > self.lastPos.level:
      self.curLevel += 1
      tok Indent
    elif pos.level < self.lastPos.level:
      self.curLevel -= 1
      tok Dedent

  # Handle multiple dedents happening at once
  # TODO: Verify this more thoroughly
  if self.curLevel > pos.level:
    self.curLevel -= 1
    tok Dedent

  if nextIs "\0": tok EOF
  elif nextIs WordChars: token = self.scanWord
  elif nextIs "\\\\": token = self.scanMLStr
  elif nextIs "\"": token = self.scanStr
  elif nextIs "#":
    consume 1
    if nextIs WordChars: tok Label, self.consumeWord
    elif nextIs "\"": tok Label, self.scanStr().str
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
  self.lastPos = pos
