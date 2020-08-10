import ast
import lexing
import lexast
import pretty


import options
import strformat
import tables
import sets
import sequtils
import json



# Everything in Ligi is either blocking or non-blocking (indentation stuff).
# By convention, explicitly indicate the blockiness of every parser when i matters.

# Because of this, we also must be careful not to return from inside any functions, lest
# we skip preserveBlocking/withBlocking/etc

# I try to keep a record of the syntax for each proc. Some conventions for those:
# - I mostly ignore whitespace in these. The comments are for the big picture
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


# Commands for working with indentation
template newlined() : bool {.dirty.} = self.blocking and self.newlined
template samelined(): bool {.dirty.} = not newlined 
template indented() : bool {.dirty.} = self.blocking and self.curLevel > self.ourLevel
template dedented() : bool {.dirty.} = self.blocking and self.curLevel < self.ourLevel
template moveline() {.dirty.} = self.newlined = false
# Move to the current line's indent
template movedent() {.dirty.} = self.ourLevel = self.curLevel

proc advance(self: Parser): Token =
  result = self.cur
  let lastPos = self.pos
  (self.pos, self.cur) = self.lexer.scan()

  if lastPos.line != self.pos.line:
    self.newlined = true
    self.curLevel = self.pos.col
  if self.cur.tag == tEOF:
    self.curLevel = 0

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

# All parse* functions should use one of these for their level preservation

template preserveBlocking(body: untyped): untyped =
  # Deceptively named. This actually preserves level
  let oldLevel = self.ourLevel
  body
  self.ourLevel = oldLevel
template setBlocking(b:bool, body: untyped): untyped = preserveBlocking:
  let oldBlocking = self.blocking
  self.blocking = b
  body
  self.blocking = oldBlocking
template withBlocking(body: untyped): untyped = setBlocking(true, body)
template withoutBlocking(body: untyped): untyped = setBlocking(false, body)
template setdent(body: untyped) {.dirty.} =
  let (oldOur, oldCur) = (self.ourLevel, self.curLevel)
  self.curLevel = self.pos.col
  self.ourLevel = self.curLevel
  body
  (self.ourLevel, self.curLevel) = (oldOur, oldCur)

proc parseStmt(self: Parser): Stmt
proc parseExpr(self: Parser, allowBlock: bool): Expr


####
# Below are small helper parsers
####

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
  else: err fmt"Unexpected {self.cur} on level {self.curLevel}. Expected a then (`=>`) or an indented block"


####
# Here begins the actual AST parsing
####

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

proc parseAccessPath(self: Parser, path: var seq[AccessOp], startImplicitAccess=false)

# After the `.` has been matched
proc parseAccess(self: Parser, path: var seq[AccessOp], implicitAccess: bool) = preserveBlocking:
  setBlocking(not implicitAccess):
    # Just a plain word
    if nextIs tWord:
      path.add AccessValue(name: tWord.take.str)
    # Swizzled
    elif tryMatch tLParen:
      withoutBlocking:
        let op = AccessSwizzle(paths: @[])
        while not nextIs tRParen:
          var s: seq[AccessOp]
          # echo fmt"Pos is {self.pos}"
          self.parseAccessPath(s, startImplicitAccess=true)
          # echo fmt"s is {pretty jsonifyAll s}"
          op.paths.add s
          if not tryMatch tComma: break
        match tRParen
        path.add op
    # Sugared `x.y.z` as
    # x.
    #   y
    #   z
    elif not implicitAccess and indented:
      movedent
      while not dedented:
        # echo fmt"{self.cur} {self.ourLevel} {self.curLevel}"
        moveline
        self.parseAccessPath(path, startImplicitAccess=true)
    else:
      err fmt"Expected a word, indent, or swizzle after the access! Found {self.cur}"

# {access | call | index}
proc parseAccessPath(self: Parser, path: var seq[AccessOp], startImplicitAccess=false) = preserveBlocking:
  var implicitAccess = startImplicitAccess
  # echo fmt"Got into parseAccessPath. Next is {self.cur}, implicit is {implicitAccess}"
  while implicitAccess or nextIs {tLParen, tLBracket, tAccess}:
    if tryMatch tLParen:
      let call = AccessCall(kind: ckCall, args: @[])
      withoutBlocking:
        while not nextIs tRParen:
          call.args.add self.parseExpr(allowBlock=false)
          if not tryMatch tComma: break
        moveline # Set our line to the tRParen's line
        match tRParen
      path.add call
    elif tryMatch tLBracket:
      let index = AccessCall(kind: ckIndex, args: @[])
      withoutBlocking:
        while not nextIs tRBracket:
          index.args.add self.parseExpr(allowBlock=false)
          if not tryMatch tComma: break
        match tRBracket
      path.add index
    # One of a word, tuple, or a block with an accessPath per line
    elif implicitAccess or tryMatch tAccess: self.parseAccess(path, implicitAccess)
    else:
      err fmt"Unexpected {self.cur}"
    implicitAccess = false


# (tuple|compund|word|str) [accessPath]
proc parseAccessible(self: Parser): Expr = preserveBlocking:
  if nextIs tWord: result = Word(word: tWord.take.str)
  elif nextIs tStr: result = String(str: tStr.take.str)
  else:
    echo fmt"Dedented is {dedented}"
    err fmt"Expected a tuple, compound, word, or string, got {self.cur.tag}"
  self.parseAccessPath result.path


# ['else' [capt]] ['finally' [capt]]
template parseElseFinally() = withBlocking:
  if tryMatch tElse:
    parseCapts result.defCapt
    result.default = self.parseThenOrBlock
  if tryMatch tFinally:
    parseCapts result.finCapt
    result.final = self.parseThenOrBlock

# expr [capt] thenOrBlock
proc parseIfArm(self: Parser): IfArm =
  result.cond = self.parseExpr(allowBlock=false)
  parseCapts result.capt
  result.val = self.parseThenOrBlock
  moveline
# if ifArm {'elif' ifArm} [elseFinally]
proc parseIf(self: Parser): If = withBlocking: setdent:
  new result
  match tIf
  while true:
    result.arms.add self.parseIfArm()
    if not tryMatch tElIf: break
  parseElseFinally()


proc parseWhenArm(self: Parser): WhenArm = withBlocking:
  match tIs
  if nextIs WhenOps:
    result.op = BinOp take(WhenOps).tag
  else:
    result.op = opEq
  result.rhs = self.parseExpr(allowBlock=false)
  parseCapts result.capt
  result.val = self.parseThenOrBlock()
proc parseWhen(self: Parser): When = withBlocking: setdent:
  new result
  match tWhen
  result.lhs = self.parseExpr(allowBlock=false)
  parseCapts result.lhsCapt
  if not indented: err "Expected indentation to start a list of `is` arms"
  moveline
  movedent
  
  if not nextIs tIs: err "Expected a list of `is` arms"
  movedent
  moveline
  while nextIs tIs:
    result.arms.add self.parseWhenArm()
  parseElseFinally()



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
    else: self.parseAccessible()

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


template parseBindInit() =
  var b = Bind()
  b.loc = self.parseBindLoc()
  if tryMatch tAssg: b.init = self.parseExpr(allowBlock=true)
  result.binds.add b
# bindSpec (blockOfBinds | bind {',' bind})
proc parseBindGroup(self: Parser): BindGroup = withBlocking:
  new result
  result.spec = BindSpec take(BindSpecs).tag
  result.binds = @[]
  if indented:
    movedent
    while not dedented:
      moveline
      parseBindInit()
  else:
    while true:
      parseBindInit()
      if not tryMatch tComma: break


proc parseStmt(self: Parser): Stmt = withBlocking:
  result =
    if nextIs BindSpecs: self.parseBindGroup()
    else: self.parseExpr(allowBlock=false)
proc parseExpr(self: Parser, allowBlock: bool): Expr = preserveBlocking:
  #echo fmt"Indent is {indented} and allowBlock is {allowBlock}"
  if allowBlock and indented:
    result = self.parseBlock()
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
