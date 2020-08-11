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
    let startLine = self.pos.line
    let next = self.parseStmt()
    echo pretty jsonify next
    result.add next
    if (self.pos.line == startLine) and not nextIs tEOF: err "Expected a newline"
    moveline
  if indented: err "Unexpected indent"
proc parseBlock(self: Parser, label=""): Block = withBlocking:
  movedent
  moveline
  result = Block(label:label, stmts: self.parseStmtSeq())

proc parseTypeDesc(self: Parser, assgDelimited=false): Expr = preserveBlocking:
  if assgDelimited and tryMatch tAssg:
    result = self.parseExpr(allowBlock=false)
    match tAssg
  elif tryMatch tColon:
    result = self.parseExpr(allowBlock=false)

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

proc parseArrayLit(self: Parser): ArrayLit = withoutBlocking:
  new result
  while not nextIs tRBracket:
    result.vals.add self.parseExpr(allowBlock=false)
    if not tryMatch tComma: break

proc parseStructLit(self: Parser): StructLit = withoutBlocking:
  new result
  while tryMatch tAccess:
    var b = Bind()
    b.loc = self.parseBindLoc()
    var wasIndented = false
    if tryMatch tAssg: withBlocking:
      wasIndented = indented
      b.init = self.parseExpr(allowBlock=true)
    result.fields.add b
    # Allow indentation to serve as a comma if we parsed an indented block
    if not (wasIndented or tryMatch tComma): break

proc parseCompound(self: Parser): Compound = withoutBlocking:
  match tLBracket
  let ty = self.parseTypeDesc(assgDelimited=true)
  # []
  if tryMatch tRBracket: result = Compound()
  # [.name = blah]
  elif nextIs tAccess: result = self.parseStructLit()
  # [val, val]
  else:  result = self.parseArrayLit()
  result.ty = ty
  match tRBracket

proc parseTuple(self: Parser): Tuple = withoutBlocking:
  result = Tuple(ty:nil, vals: @[])
  match tLParen
  result.ty = self.parseTypeDesc(assgDelimited=true)
  while not nextIs tRParen:
    result.vals.add self.parseExpr(allowBlock=false)
    if not tryMatch tComma: break
  match tRParen


# TODO: The access/pipe code is all kinds of messed up.
# However, the syntax we're parsing is also messed up.
# Revisit the syntax itself, then rewrite this section for the 4th time.

proc parseAccessPath(self: Parser, path: var seq[AccessOp], assumeAccess=false)
proc parseCall(self: Parser, path: var seq[AccessOp]) =
  var
    kind: CallKind
    closer: Tag
  if tryMatch tLBracket:
    kind = ckIndex
    closer = tRBracket
  elif tryMatch tLParen:
    kind = ckCall
    closer = tRParen
  else:
    err "Expected a call or index"

  var args: seq[Expr] = @[]
  withoutBlocking:
    while not nextIs closer:
      args.add self.parseExpr(allowBlock=false)
      if not tryMatch tComma: break
    match closer
  path.add AccessCall(kind:kind, args:args)


proc parseAccess(self: Parser, path: var seq[AccessOp]) =
  # '.' is already matched. Next is (swizzle | word | indented accesses)
  if indented:
    withBlocking:
      movedent
      while not dedented:
        moveline
        self.parseAccessPath(path, assumeAccess=true)
  elif nextIs tWord:
    path.add AccessName(name:take(tWord).str)
  elif tryMatch tLParen:
    withoutBlocking:
      var pathList: seq[seq[AccessOp]] = @[]
      var ty = self.parseTypeDesc(assgDelimited=true)

      while not nextIs tRParen:
        var innerPath: seq[AccessOp] = @[]
        self.parseAccessPath(innerPath, assumeAccess=true)
        pathList.add innerPath
        if not tryMatch tComma: break
      match tRParen
      path.add AccessSwizzle(ty:ty, paths:pathList)
  else:
    err fmt"Expected indent, a word, or a swizzle, but found {self.cur}"


template commonPipe(): Expr =
  var into: Expr
  if nextIs tLParen:
    into = self.parseTuple()
  elif nextIs tWord:
    into = Word(word: take(tWord).str)
  else:
    err fmt"Expected a word or tuple, found a {self.cur}"

  if nextIs {tLParen, tLBracket}:
    self.parseCall into.path
  else:
    into.path.add AccessCall(kind: ckCall, args: @[])

  into

proc parsePipe(self: Parser, path: var seq[AccessOp]) =
  # '::' is already matched. Next is (tuple | word | indentedPipes), optionally followed by
  # a AccessCall. Even if there's more after that, we don't parse it in this proc.
  if indented: # Many pipes at once
    withBlocking:
      movedent
      while not dedented:
        moveline
        path.add AccessPipe(into: commonPipe())
        # Parse the rest of the stuff on the line
        self.parseAccessPath path
  else:
    path.add AccessPipe(into: commonPipe())

  



# Each is:
# '.' (blockOfAccess | name | swizzle) | `::` (blockOfAccess | tuple | name)
proc parseAccessPath(self: Parser, path: var seq[AccessOp], assumeAccess=false) = preserveBlocking:
  # For kickstarting inside swizzles, mainly
  if assumeAccess: self.parseAccess path
  while nextIs {tAccess, tPipe, tLParen, tLBracket}:
    if tryMatch tAccess: self.parseAccess(path)
    elif tryMatch tPipe: self.parsePipe(path)
    elif nextIs {tLParen, tLBracket}: self.parseCall path
  if tryMatch tColon:
    withBlocking:
      let arg = self.parseExpr(allowBlock=true)
      if path.len > 0 and path[path.high] of AccessCall:
        path[path.high].AccessCall.args.add arg
      else:
        path.add AccessCall(kind: ckCall, args: @[arg])

# (tuple|compund|word|str) [accessPath]
proc parseAccessible(self: Parser, restrict=false): Expr = preserveBlocking:
  if nextIs tWord: result = Word(word: tWord.take.str)
  elif nextIs tStr: result = String(str: tStr.take.str)
  elif nextIs tLParen: result = self.parseTuple()
  elif nextIs tLBracket: result = self.parseCompound()
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



proc parseLoop(self: Parser): Loop = withBlocking: setdent:
  new result
  match tLoop
  parseCapts result.counter
  result.body = self.parseThenOrBlock()
  parseElseFinally()

# for/while have the exact same syntax, so why not?
template parseCondLoop() =
  result.expr = self.parseExpr(allowBlock=false)
  parseCapts result.capt, result.counter
  result.body = self.parseThenOrBlock()
  parseElseFinally()
proc parseFor(self: Parser): For = withBlocking: setdent:
  new result
  match tFor
  parseCondLoop()
proc parseWhile(self: Parser): While = withBlocking: setdent:
  new result
  match tWhile
  parseCondLoop()

proc parseControlStructure(self: Parser): ControlStructure = preserveBlocking:
  result =
    if nextIs tIf: self.parseIf()
    elif nextIs tWhen: self.parseWhen()
    elif nextIs tLoop: self.parseLoop()
    elif nextIs tFor: self.parseFor()
    elif nextIs tWhile: self.parseWhile()
    else: err "Expected a control structure!"


# TODO: Refactor common elements from parseMacro and parseFn
# 'macro' [arglist] (block | '=>' stmt)
proc parseMacro(self: Parser): Macro = withBlocking:
  new result
  match tMacro

  # Macros are the same as functions but can't have a `->` ret bind
  if indented:
    withBlocking:
      result.body = self.parseBlock()
  # No args but `=>` or `->` specified
  elif tryMatch tThen: result.body = self.parseExpr(allowBlock=true)
  else:
    withoutBlocking:
      while not nextIs tThen:
        result.args.add self.parseBindLoc()
        if not tryMatch tComma: break
    moveline
    result.body = self.parseExpr(allowBlock=true)


template parseVoidBody() =
  result.ret = Bind(
    loc: BindName(
      name:"_",
      ty: Word(word:"void"),
    ),
    init: self.parseExpr(allowBlock=true),
  )
template parseRetBody() =
  if tryMatch tThen: parseVoidBody()
  elif tryMatch tStoreIn:
    result.ret = Bind(
      loc: self.parseBindLoc()
    )
    match tAssg
    result.ret.init = self.parseExpr(allowBlock=true)
  else: err "Expected either `=>` and a function body or `->` and a return binding"
proc parseFn(self: Parser): Fn = withBlocking:
  new result
  match tFn

  # A void function with no args
  if indented:
    withBlocking:
      parseVoidBody()
  # No args but `=>` or `->` specified
  elif nextIs {tThen, tStoreIn}:
    parseRetBody()
  else:
    withoutBlocking:
      while not nextIs {tThen, tStoreIn}:
        result.args.add self.parseBindLoc()
        if not tryMatch tComma: break
    moveline
    parseRetBody()

# return and break are included here so you can do things like
# let x = optional or return false
# (since they're divergent, they're fine type-wise)
# return | break | fn | macro | controlStructure | pipeline
proc parseAtom(self: Parser): Expr = preserveBlocking:
  result =
    if nextIs tReturn: self.parseReturn()
    elif nextIs tBreak: self.parseBreak()
    elif nextIs {tIf, tWhen, tWhile, tFor, tLoop}: self.parseControlStructure()
    elif nextIs tFn: self.parseFn()
    elif nextIs tMacro: self.parseMacro()
    else: self.parseAccessible()


# {unary} (unary ':' exprOrBlock | pipable)
proc parseUnary(self: Parser): Expr = preserveBlocking:
  if nextIs UnaOps:
    let op = take(UnaOps).tag.UnaOp
    if tryMatch tColon:
      withBlocking:
        result = Unary(op: op, val: self.parseExpr(allowBlock=true))
    elif indented:
      err fmt"Unexpected indentation after a unary. Did you mean to use a colon?"
    else:
      result = Unary(op: op, val: self.parseUnary())
  else:
    result = self.parseAtom()

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

proc parseAssert(self: Parser): Assert = withBlocking:
  new result
  match tAssert
  result.val = self.parseExpr(allowBlock=false)
  if tryMatch tColon:
    result.msg = take(tStr).str

proc parseStmt(self: Parser): Stmt = withBlocking:
  result =
    if nextIs BindSpecs: self.parseBindGroup()
    elif nextIs tAssert: self.parseAssert()
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
