import strutils
import tables
import strformat

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
const Sigils = toTable {
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
}

type Pos* = tuple[line: int, col: int]

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

type Lexer* = object
  pos*: Pos
  data*: iterator(): char
  next*: array[3, char]

proc cur*(self: Lexer): char = self.next[0]
proc nextIs*(self: Lexer, what: string): bool =
  assert what.len <= 3, "Only 3 char lookahead"
  for i, c in what:
    if c != self.next[i]: return false
  return true

proc advance*(self: var Lexer): char =
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
  #echo fmt"Advanced by {result}"

template consume*(self: var Lexer, n = 1) =
  for i in 1..n: discard self.advance()

template skip(self: var Lexer) =
  ## comments will be handled in scan() as they could also be a `-` token
  var clean = false
  while not clean:
    clean = true
    while self.cur.isSpaceAscii:
      clean = false
      discard self.advance()
    while self.nextIs "--":
      clean = false
      while self.cur != '\n': self.consume


# Ironic. In the course of implementing a language with `alias`, I resorted to implementing `alias`
template alias(what: untyped, asWhat: untyped): untyped =
  template what(): untyped {.dirty.} = asWhat

proc scan*(self: var Lexer): tuple[pos: Pos, tok: Token] =
  # If you didn't already have diabetes... you're about to
  alias pos: result.pos
  alias tok: result.tok
  alias cur: self.cur
  template nextIs(str: string): bool = self.nextIs str
  template tag(t: Tag): untyped = result.tok = Token(tag: t)
  template tag(t: Tag, s: string): untyped = result.tok = Token(tag: t, str: s)
  template tag(t: Tag, c: char): untyped = result.tok = Token(tag: t, chr: c)
  template consume(n = 1): untyped {.dirty.} = self.consume(n)
  template expect(s: static[string]): untyped =
    if not nextIs s:
      quit "Expected `{" & s & "}`, but got `{$cur}`"
    consume s.len
  template consumeWord(startingWith: sink string = ""): string =
    var s = startingWith
    while self.cur in WordChars: s &= self.advance
    s


  self.skip
  pos = self.pos
  if nextIs "\0": tag EOF
  elif cur in WordChars:
    var str = consumeWord()
    if str in Keywords: tag Keywords[str]
    else:
      tag Word, str
  elif nextIs "\\\\": # line string lit. TODO
    var str = ""
    var multiline = false
    while nextIs "\\\\":
      consume 2
      if multiline: str &= '\n'
      multiline = true
      while self.cur != '\n': str &= self.advance()
      self.skip # Skip both whitespace and comments, allowing for interlacing comments in strings
    tag Str, str
  elif nextIs "\\": # Stropped keyword
    consume
    if self.cur notin WordChars: quit "Expected a word to strop"
    tag Word, consumeWord()
  elif nextIs "#":
    consume
    if self.cur notin WordChars: quit "Expected a word for a tag"
    tag Label, consumeWord()
  elif nextIs "\"":
    consume
    var str = ""
    while self.cur != '"':
      if self.cur == '\n':
        quit "No multiline quoted multiline string lits. Use \\\\ literals for that."
      if self.cur == '\0':
        quit "Unclosed quoted string literal at EOF"
      if self.cur == '\\':
        consume
      str &= self.advance
    expect "\""
    tag Str, str
  elif nextIs "'":
    consume
    var chr: char
    chr = self.advance
    if chr == '\\': chr = self.advance
    if self.advance != '\'': quit "expected a closing `'`"
  else:
    for sigil, theTag in Sigils.pairs:
      if nextIs sigil:
        consume sigil.len
        tag theTag
