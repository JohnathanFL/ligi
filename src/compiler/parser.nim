import ast
import lexing
import lexast


import options
import strformat
import tables
import sets
import sequtils


# Everything in Ligi is either blocking or non-blocking (indentation stuff).
# By convention, explicitly indicate the blockiness of every parser when i matters.

# Because of this, we also must be careful not to return from inside any functions, lest
# we skip preserveBlocking/withBlocking/etc

# I try to keep a record of the syntax for each proc. Some conventions for those:
# - I use a mixture of EBNF, regexes, and custom syntax
# - Actual words refer to something else
# - Things in quotes refer to the literal text
# - {} is 0+
# - [] is 0-1
# - ?word predicates an optional [] or {} on word's value
# - ~word means find but do not match word


template err(msg: string) = quit fmt"{getStackTrace()}{self.pos.line}:{self.pos.col}: " & msg  # TODO

type
  State = enum
    sNewlined, sIndented, sDedented
  States = set[State]

type Parser = ref object
  lexer: Lexer
  cur: Token
  pos: Pos
  blocking: bool
  curLevel: int # The col of the first token of this line
  ourLevel: int # The col of the first token of this sequence
  newlined: bool

template newlined() : bool {.dirty.} = self.blocking and self.newlined
template samelined(): bool {.dirty.} = not newlined 
template indented() : bool {.dirty.} = self.ourLevel < self.curLevel
template dedented() : bool {.dirty.} = self.ourLevel > self.curLevel

template moveline() {.dirty.} = self.newlined = false
template movedent() {.dirty.} = self.ourLevel = self.curLevel

proc advance(self: Parser): Token =
  result = self.cur
  let lastPos = self.pos
  (self.pos, self.cur) = self.lexer.scan()

  if lastPos.line != self.pos.line:
    self.newlined = true
    self.curLevel = self.pos.col

proc nextIs(self: Parser, t: Tag): bool =
  if self.blocking and newlined: return false
  else: return self.cur.tag == t
proc nextIs(self: Parser, t: set[Tag]): bool =
  if self.blocking and newlined: return false
  else: return self.cur.tag in t


template nextIs(t: untyped): bool = self.nextIs t
template tryMatch(t: untyped): bool =
  if nextIs t:
    discard self.advance()
    true
  else: false
template take(t: untyped): Token =
  if not nextIs t: err "Expected " & $t & ", but found " & $self.cur.tag
  self.advance()
template match(t: untyped) = discard take t

# All parse* functions should use one of these three for their level preservation
template withBlocking(body: untyped, b = true): untyped =
  let oldBlocking = self.blocking
  let oldLevel = self.ourLevel
  self.blocking = b
  body
  self.blocking = oldBlocking
  self.ourLevel = oldLevel
template withoutBlocking(body: untyped): untyped =
  withBlocking(body, false)
template preserveBlocking(body: untyped): untyped =
  let oldLevel = self.ourLevel
  body
  self.ourLevel = oldLevel

proc parseStmt(self: Parser): Stmt
proc parseExpr(self: Parser, allowBlock: bool): Expr


proc parseStmtSeq(self: Parser): seq[Stmt] = withBlocking:
  while not (indented or dedented) and not nextIs tEOF:
    result.add self.parseStmt()
    moveline
  if indented: err "Unexpected indent"

# 'return' [block | expr]
proc parseReturn(self: Parser): Return = preserveBlocking:
  new result
  match tReturn
  if not nextIs({tRBracket, tRParen}) and (indented or not newlined):
    result.val = self.parseExpr(allowBlock=true)

# 'break' [label] [':' expr]
proc parseBreak(self: Parser): Break = preserveBlocking:
  new result
  match tBreak
  if nextIs tLabel: result.label = tLabel.take.str
  if tryMatch tColon: result.val = self.parseExpr(allowBlock=true)

proc parseAccess(self: Parser): Expr = preserveBlocking:
  let s = take {tWord}
  result = Word(word: s.str)

# return and break are included here so you can do things like
# let x = optional or return false
# (since they're divergent, they're fine type-wise)
# return | break | controlStructure | access
proc parseAtom(self: Parser): Expr = preserveBlocking:
  result =
    if nextIs tReturn: self.parseReturn()
    elif nextIs tBreak: self.parseBreak()
    else: self.parseAccess()


proc parseStmt(self: Parser): Stmt = withBlocking:
  result = self.parseExpr(allowBlock=false)
proc parseExpr(self: Parser, allowBlock: bool): Expr = preserveBlocking:
  echo fmt"Indent is {indented} and allowBlock is {allowBlock}"
  if allowBlock and indented:
    movedent
    moveline
    result = Block(label:"", stmts: self.parseStmtSeq())
  elif indented:
    err "Didn't expect indentation here"
  else:
    result = self.parseAtom()

proc parse*(lexer: Lexer): seq[Stmt] =
  var self = Parser(lexer: lexer, blocking: true)
  # Kick .cur and .pos
  discard self.advance()
  moveline
  movedent
  return self.parseStmtSeq()
