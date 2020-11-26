import ast
import lexing

import strformat, tables, macros, options, sugar, sets, sequtils

template err(msg: string) = quit fmt"{getStackTrace()}{self.pos.line}:{self.pos.col}: " & msg  # TODO

type
  # All of this is to be handled exclusively from the `parseExpr` function.
  # Individual parsing functions then check `.exprStyle` via `thisExprCan`.
  ExprCan* = enum
    canColon,        # Can use `:` calls
    canAssg,         # Can do assignments
    canBlock,        # Can have an implicit block *at all*
    canComma,        # Can use un-parenthesized function calls of multiple args
    canImplicitBlock,# Can start with an Indent (i.e the entire expr is one implicit block)
  ExprStyle* = set[ExprCan]

const
  NormalExpr*: ExprStyle = { canColon, canBlock, canComma }
  AssingableExpr*: ExprStyle = NormalExpr + { canAssg }
  UnblockedExpr* = NormalExpr - { canBlock }
  # Anywhere you can implicit block, you could also pass an assignment.
  MaybeBlockExpr* = NormalExpr + { canAssg, canImplicitBlock }

type Parser* = ref object
  exprStyle*: ExprStyle
  lexer*: Lexer

  cur*: Token
  # Position of cur
  pos*: Pos
  # Reference position. Use it to figure out if we in/dedented or newlined.
  # Must be manually set.
  # Must be cleared by one of:
  #   - Compound parser
  #   - Block parser
  refPos*: tuple[line: int, level: int]
  # Was cur directly (no whitespace) adjacent to the previous token?
  attached*: bool
  prec*: int
  isControl*: bool
  isBin*: bool
  isAssg*: bool
  isAccess*: bool

  # Do we currently care about newlines/indents/dedents?
  blocking*: bool
  # Did we parse something like a control statement which unambiguously
  # ends the current expression?
  stopped*: bool

macro makePrecs*(body: untyped): untyped =
  result = newCall(ident "toTable", newTree(nnkTableConstr, []))

  var i = 0
  for level in body:
    for op in level:
      result[1].add newColonExpr(op, newIntLitNode i)
    inc i

# Binary operator precedence cache
# Words must be manually added, new sigils will be added based on
# what they look like when they're found.
var Precedences* = makePrecs [
  # Level 0 is assignment
  [iLambda],
  [iOr, iXor],
  [iAnd],
  [iEq, iNeq, iGtEq, iLtEq, iLt, iGt],
  [iAdd, iSub],
  [iMul, iDiv, iMod],
]
# Don't treat pure `:` as a binop

proc getMaxPrec*(): int =
  result = 0
  for v in Precedences.values:
    if v > result: result = v
  result.inc
var MaxPrec* = getMaxPrec()


var AssgOps*: HashSet[StrID] = toHashSet [
  iAssg, iAddAssg, iSubAssg, iMulAssg, iDivAssg
]


# Although technically binary, access ops are treated differently
var AccessOps*: HashSet[StrID] = toHashSet [
  iAccess, iAccessPipe, iOptAccess, iOptAccessPipe,
]



# Words that split into their own special handlers
# Each is added directly below its function

type ControlFunc* = (proc(self: Parser): Atom)
var ControlWords* = initTable[StrID, ControlFunc]()
proc registerHandler*(words: openarray[StrID], handler: ControlFunc) =
  for word in words:
    if ControlWords.contains word:
      quit "ERROR: Re-registering control word " & $word
    ControlWords[word] = handler

# -1 is no prec
proc precOf*(t: Token): int =
  result = -1
  case t.kind:
    of tkWord:
      if Precedences.contains t.id:
        result = Precedences[t.id]
    of tkSigil:
      if Precedences.contains t.id:
        result = Precedences[t.id]
      else:
        if ControlWords.contains(t.id) or AccessOps.contains(t.id) or AssgOps.contains(t.id): result = -1
        else:
          # echo fmt"Precedence calculation is TODO ({t})"
          Precedences[t.id] = -1
    else: discard

# I try to keep a record of the syntax for each proc. Some conventions for those:
# - I mostly ignore whitespace in these. The comments are for the big picture
# - I use a mixture of EBNF, regexes, and custom syntax
# - Actual words refer to something else
# - Things in quotes refer to the literal text
# - {} is 0+
# - [] is 0-1
# - ?word predicates an optional [] or {} on word's value
# - ~word means find but do not match word

proc advance*(self: Parser): Token =
  result = self.cur
  # In ligi, could be
  # self.(pos, attached, cur) = self.lexer.scan
  (self.pos, self.attached, self.cur) = self.lexer.scan

  self.isBin = false
  self.isAssg = false
  self.isControl = false
  self.isAccess = false

  if self.cur.kind in { tkWord, tkSigil }:
    self.prec = precOf self.cur
    if self.prec != -1:
      self.isBin = true
    elif ControlWords.contains self.cur.id:
      self.isControl = true
    elif AccessOps.contains self.cur.id:
      self.isAccess = true
    elif AssgOps.contains self.cur.id:
      self.isAssg = true
  # echo "Advanced to ", self.cur, fmt"({self.pos}, {self.refPos})"

# Commands for working with indentation
template newlined*(): bool {.dirty.} =
  self.blocking and self.pos.line != self.refPos.line
#template samelined(): bool {.dirty.} = not newlined 
template indented*(): bool {.dirty.} =
  self.blocking and self.pos.level > self.refPos.level
template dedented*(): bool {.dirty.} =
  self.blocking and self.pos.level < self.refPos.level

proc nextIs*(self: Parser, i: StrID, respectNewline = true): bool =
  result = not (newlined and respectNewline) and self.cur.kind in {tkWord, tkSigil, tkPunc} and self.cur.id == i
proc nextIs*(self: Parser, tags: HashSet[StrID], respectNewline = true): bool =
  result = not (newlined and respectNewline) and self.cur.kind in {tkWord, tkSigil, tkPunc} and self.cur.id in tags
proc nextIs*(self: Parser, ids: openArray[StrID], respectNewline = true): bool =
  if newlined and respectNewline: return false
  result = false
  if self.cur.kind in {tkWord, tkSigil, tkPunc}:
    for i in ids:
      if self.cur.id == i:
        return true
proc nextIs*(self: Parser, kinds: set[TokenKind], respectNewline = true): bool =
  not (newlined and respectNewline) and self.cur.kind in kinds
proc nextIs*(self: Parser, kind: TokenKind): bool = not newlined and self.cur.kind == kind


template nextLineIs*(thing: untyped): bool = self.nextIs(thing, false)
template nextIs*(thing: untyped): bool = self.nextIs(thing)
template take*(thing: untyped): Token =
  if not self.nextIs thing:
    when thing is StrID:
      err "Expected " & thing.lookup
    else:
      err "Expected " & $thing
  self.advance
template match*(thing: untyped) = discard take thing
template tryMatch*(thing: untyped): bool =
  if nextIs thing:
    match thing
    true
  else:
    false


template setDent*() {.dirty.} =
  # echo "SetDent to ", self.pos.level
  self.refPos.level = self.pos.level
template setLine*() {.dirty.} =
  # echo "SetLine to ", self.pos.line
  self.refPos.line = self.pos.line

template pushRef*(body: untyped) =
  let oldRefPos = self.refPos
  body
  self.refPos = oldRefPos
  # echo fmt"Back to {self.refPos}"

template setBlocking(body: untyped, wth: bool): untyped =
  let oldBlocking = self.blocking
  self.blocking = wth
  block:
    body
  self.blocking = oldBlocking
template withBlocking*(body: untyped): untyped = setBlocking(body, true)
template withoutBlocking*(body: untyped): untyped = setBlocking(body, false)

# Simplifies making sure we check stopped each time
template descend*(body: untyped) =
  body
  if self.stopped: return

template thisExprCan*(can: set[ExprCan]): bool = can <= self.exprStyle

# The fundamental funcs
proc parseExpr*(self: Parser, kind: ExprStyle): Atom
proc parseTypeDesc*(self: Parser, trailing: bool): Atom
proc parseBlock*(self: Parser): Atom


template parseDelimited*(delim: StrID, endCond: untyped): seq[Atom] =
  var result: seq[Atom]
  # echo "Parsing a `" & delim.lookup & "` delimited list"
  while not endCond:
    if nextIs delim:
      match iComma
      continue
    # echo fmt"Parsing expr when next is {self.cur}"
    result.add(self.parseExpr(UnblockedExpr))
    self.stopped = false
    if not nextIs delim:
      # echo "Breaking"
      break
    match delim
  result

proc parseTypeDesc*(self: Parser, trailing: bool): Atom =
  if not nextIs iColon:
    return iSink.toAtom

  let innerKind =
    if trailing:
      NormalExpr - { canBlock, canColon }
    else:
      NormalExpr

  match iColon
  result = self.parseExpr innerKind

  if trailing: match iColon

proc parseBlock*(self: Parser): Atom =
  result = list(ibBlock, [])
  withoutBlocking:
    result.add self.parseTypeDesc(true)

  # Note that we setDent *after* the typedesc.
  # Thus you can do `{ :i32:
  #   code...
  # }`, where normally you couldn't put the `:i32:` at a "different" level from the main body.
  # 
  withBlocking: pushRef:
    setDent
    while not (dedented or self.cur.kind == tkEOF):
      setLine
      result.children.add self.parseExpr(NormalExpr)
      self.stopped = false




proc parseAccessible*(self: Parser): Atom =
  if nextIs { tkWord, tkStrop }:
    result = Atom(
      kind: akWord,
      id: self.advance.id,
    )
  elif nextIs { tkStr }:
    result = Atom(
      kind: akStr,
      str: self.advance.str,
    )
  # For the purposes of unaries, treat as words
  elif nextIs { tkSigil }:
    result = Atom(
      kind: akWord,
      id: self.advance.id,
    )
  else:
    if newlined:
      err "Hit a newline. Expected a word or a string."
    else:
      err fmt"Expected a word, sigil, or a string. Found {self.cur}"
  # echo fmt"Got a {result}"

template handlePostOps() =
  # echo fmt"Parsing a postop on {self.cur}"
  if nextIs iLParen:
    withoutBlocking:
      match iLParen
      result = list(
        result,
        parseDelimited(iComma, nextIs iRParen)
      )
      match iRParen
  elif nextIs iLBracket:
    withoutBlocking:
      match iLBracket
      result = list(ibAt, result)
      result.children = result.children.concat parseDelimited(iComma, nextIs iRBracket)
      match iRBracket
  elif self.isAccess:
    result = list(
      self.advance.id, # `.`, `?.`, etc
      result,
      self.parseRedirectable(mayAccess=false)
    )
  else:
    err "Improper usage of handlePostOps"
# Handles all table driven stuff
proc parseRedirectable*(self: Parser, mayAccess = true): Atom =
  # First check needed to stop `"if"` (a string) from being a keyword
  if self.cur.kind in {tkWord, tkPunc} and ControlWords.contains self.cur.id:
    # `if`, `while`, `let`, `{} blocks`, etc
    # All control structures set stopped, but stuff like `{}` or `()` don't.
    descend:
      # echo "Parsing control ", self.cur.id.lookup
      result = ControlWords[self.cur.id](self)
      if self.stopped and not mayAccess:
        err "May not have an unambiguous stop in a non-accessible redirectable."
  else:
    # Default handlers for strings, chars, and non-control words
    result = self.parseAccessible()
  while mayAccess and not newlined and (self.isAccess or (self.attached and nextIs [iLParen, iLBracket])):
    handlePostOps()

  if nextIs(iColon) and thisExprCan {canColon}:
    match iColon
    result = list(
      result,
      self.parseExpr (NormalExpr + {canImplicitBlock})
    )
    self.stopped = true

  elif indented and self.isAccess:
    # echo "In here"
    pushRef:
      setDent
      while not dedented:
        if not self.isAccess:
          err "Expected an access op"
        setLine
        handlePostOps()
    self.stopped = true
  elif nextIs iColon:
    self.stopped = true

proc parseUnary(self: Parser): Atom =
  descend: result = self.parseRedirectable()
  # Parsing things like `if` or `when` will self.stopped and properly prevent
  # us from treating them as unary operators. Thus, we can fearlessly treat anything
  # that gets here as "unary operator"-able
  # 
  # If the next op is a marked bin/assgop, we need to fall back to parseBinary
  if not (self.isBin or self.isAssg or newlined) and self.cur.kind notin { tkPunc }:
    result = list( result, self.parseUnary() )
    if nextIs(iComma) and thisExprCan {canComma}:
      self.exprStyle = self.exprStyle - {canComma}
      while tryMatch iComma:
        result.children.add self.parseUnary()
      self.stopped = true

proc parseBinary(self: Parser, prec = 0): Atom =
  template nextLevel(): Atom =
    if prec == MaxPrec: self.parseUnary
    else: self.parseBinary(prec + 1)
  descend: result = nextLevel()
  while self.cur.kind in {tkWord, tkSigil} and Precedences.getOrDefault(self.cur.id, -1) == prec:
    descend:
      result = list( self.advance.id, result, nextLevel() )

proc parseExpr*(self: Parser, kind: ExprStyle): Atom =
  let oldStyle = self.exprStyle
  self.exprStyle = kind


  if indented and thisExprCan {canImplicitBlock}:
    result = self.parseBlock()
    self.stopped = true
  else:
    result = self.parseBinary()
    if nextIs(AssgOps) and thisExprCan {canAssg}:
      # Cannot chain assignments.
      result = list( self.advance.id, result, self.parseExpr(self.exprStyle - {canAssg}) )

  self.exprStyle = oldStyle


proc parse*(lexer: Lexer): Atom =
  var self = Parser(
    lexer: lexer,
    pos: (-1, -1, 0),
    refPos: (-1, 0),
    attached: false,
    prec: -1,
    isControl: false,
    isBin: false,
    isAccess: false,
    blocking: true,
    stopped: false,
  )
  discard self.advance
  self.parseBlock
