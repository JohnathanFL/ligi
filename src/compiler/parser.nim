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
  if newlined: return false
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
  if not nextIs t:
    if newlined: err "Expected " & $t & ", but found a newline"
    else: err "Expected " & $t & ", but found " & $self.cur.tag
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
proc parseBlock(self: Parser, label=""): Block = withBlocking:
  movedent
  moveline
  result = Block(label:label, stmts: self.parseStmtSeq())

proc parseTypeDesc(self: Parser, trailing=false): Expr = preserveBlocking:
  if tryMatch tColon:
    result = self.parseExpr(allowBlock=false)
    if trailing: match tColon

# (word | '(' bindLoc {',' bindLoc | ','}')')
proc parseBindLoc(self: Parser): BindLoc = preserveBlocking:
  if nextIs tWord:
    result = BindName(name:tWord.take.str)
  elif tryMatch tLParen:
    let loc = BindTuple(locs: @[])
    withoutBlocking:
      while not tryMatch tRParen:
        loc.locs.add self.parseBindLoc()
        if not tryMatch tComma: break
  result.ty = self.parseTypeDesc


proc parseCapts(self: Parser, capt: var BindLoc) =
  if tryMatch tStoreIn:
    capt = self.parseBindLoc()
proc parseCapts(self: Parser, capt1, capt2: var BindLoc) =
  if tryMatch tStoreIn:
    capt1 = self.parseBindLoc()
    if tryMatch tComma: capt2 = self.parseBindLoc()
template parseCapts(capt: var BindLoc) = self.parseCapts capt
template parseCapts(capt1, capt2: var BindLoc) = self.parseCapts capt1, capt2


proc parseThenOrBlock(self: Parser): Expr = withBlocking:
  if tryMatch tThen:
    result = self.parseExpr(allowBlock=false)
  elif indented:
    result = self.parseBlock()

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

template parseElseFinally() = withBlocking:
  if tryMatch tElse:
    parseCapts result.defCapt
    result.default = self.parseThenOrBlock
  if tryMatch tFinally:
    parseCapts result.finCapt
    result.final = self.parseThenOrBlock

proc parseIfArm(self: Parser): IfArm =
  result.cond = self.parseExpr(allowBlock=false)
  parseCapts result.capt
  echo "capt is " & repr result.capt
  result.val = self.parseThenOrBlock
proc parseIf(self: Parser): If = withBlocking:
  new result
  match tIf
  while true:
    result.arms.add self.parseIfArm()
    if not tryMatch tElIf: break
  parseElseFinally()

proc parseWhen(self: Parser): When = withBlocking: return

proc parseLoop(self: Parser): Loop = withBlocking: return
proc parseFor(self: Parser): For = withBlocking: return
proc parseWhile(self: Parser): While = withBlocking: return

proc parseControlStructure(self: Parser): ControlStructure = preserveBlocking:
  result =
    if nextIs tIf: self.parseIf()
    elif nextIs tWhen: self.parseWhen()
    elif nextIs tLoop: self.parseLoop()
    elif nextIs tFor: self.parseFor()
    elif nextIs tWhile: self.parseWhile()
    else: err "Expected a control structure!"

# return and break are included here so you can do things like
# let x = optional or return false
# (since they're divergent, they're fine type-wise)
# return | break | controlStructure | access
proc parseAtom(self: Parser): Expr = preserveBlocking:
  result =
    if nextIs tReturn: self.parseReturn()
    elif nextIs tBreak: self.parseBreak()
    elif nextIs {tIf, tWhen, tWhile, tFor, tLoop}: self.parseControlStructure()
    else: self.parseAccess()

proc parseUnary(self: Parser, allowBlock=false): Expr = preserveBlocking:
  result =
    if nextIs UnaOps: Unary(op: UnaOps.take.tag.UnaOp, val: self.parseUnary(allowBlock=true))
    elif allowBlock and indented: self.parseBlock()
    elif indented: err "Unexpected indentation"
    else: self.parseAtom()

proc parseBinary(self: Parser, level=0): Expr = preserveBlocking:
  template nextLayer(): Expr {.dirty.} =
    if level + 1 == BinOps.len: self.parseUnary()
    else: self.parseBinary(level+1)
  result = nextLayer
  while nextIs BinOps[level]:
    let op = take BinOps[level]
    result = Binary(op: op.tag.BinOp, lhs: result, rhs: nextLayer())


proc parseStmt(self: Parser): Stmt = withBlocking:
  result = self.parseExpr(allowBlock=false)
proc parseExpr(self: Parser, allowBlock: bool): Expr = preserveBlocking:
  #echo fmt"Indent is {indented} and allowBlock is {allowBlock}"
  if allowBlock and indented:
    movedent
    moveline
    result = Block(label:"", stmts: self.parseStmtSeq())
  elif indented:
    err "Didn't expect indentation here"
  else:
    result = self.parseBinary()

proc parse*(lexer: Lexer): seq[Stmt] =
  var self = Parser(lexer: lexer, blocking: true)
  # Kick .cur and .pos
  discard self.advance()
  moveline
  movedent
  return self.parseStmtSeq()
