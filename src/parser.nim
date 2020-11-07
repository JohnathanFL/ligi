import ast
import lexing

import strformat, tables, macros, options, sugar, sets, sequtils


type Parser* = ref object
  lexer*: Lexer

  cur*: Token
  # Position of cur
  pos*: Pos
  # Reference position. Use it to figure out if we in/dedented or newlined.
  # Must be manually set.
  # Must be cleared by one of:
  #   - Compound parser
  #   - Block parser
  refPos*: Pos
  # Was cur directly (no whitespace) adjacent to the previous token?
  attached*: bool
  prec*: int
  isControl*: bool
  isBin*: bool
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
  [iAssg, iAddAssg, iSubAssg, iMulAssg, iDivAssg],
  # Technically, you could say `:` is a binop that sits just above iLambda.
  # However, `:` also allows an implicit block as its rhs. In order to not allow
  # other binops to do the same, we'll split it out as its own special case.
  [iLambda],
  [iOr, iXor],
  [iAnd],
  [iEq, iNeq, iGtEq, iLtEq, iLt, iGt],
  [iAdd, iSub],
  [iMul, iDiv, iMod],
]
# Don't treat pure `:` as a binop
Precedences[iColon] = -1

proc getMaxPrec*(): int =
  result = 0
  for v in Precedences.values:
    if v > result: result = v
  result.inc
var MaxPrec* = getMaxPrec()

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
        quit "Precedence calculation is TODO"
    else: discard

# Although technically binary, access ops are treated differently
var AccessOps* = toHashSet [
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

# Commands for working with indentation
template newlined*(): bool {.dirty.} =
  self.blocking and self.pos.line != self.refPos.line
#template samelined(): bool {.dirty.} = not newlined 
template indented*(): bool {.dirty.} =
  self.blocking and self.pos.level > self.refPos.level
template dedented*(): bool {.dirty.} =
  self.blocking and self.pos.level < self.refPos.level

proc nextIs*(self: Parser, i: StrID): bool =
  result = not newlined() and self.cur.kind in {tkWord, tkSigil, tkPunc} and self.cur.id == i
  # if result:
  #   echo fmt"self.cur was {i.lookup}"
  # else:
  #   echo fmt"{self.cur} wasn't {i.lookup}"
proc nextIs*(self: Parser, ids: openArray[StrID]): bool =
  if newlined: return false
  result = false
  if self.cur.kind in {tkWord, tkSigil, tkPunc}:
    for i in ids:
      if self.cur.id == i:
        return true
proc nextIs*(self: Parser, kinds: set[TokenKind]): bool = not newlined and self.cur.kind in kinds
proc nextIs*(self: Parser, kind: TokenKind): bool = not newlined and self.cur.kind == kind

template err(msg: string) = quit fmt"{getStackTrace()}{self.pos.line}:{self.pos.col}: " & msg  # TODO

template nextIs*(thing: untyped): bool = self.nextIs(thing)
template take*(thing: untyped): Token =
  if not self.nextIs thing:
    when thing is StrID:
      err "Expected " & thing.lookup
    else:
      err "Expected " & $thing
  self.advance
template match*(thing: untyped) = discard take thing


template setRef(body: untyped) =
  # echo "Set ref to ", self.pos
  let oldRefPos = self.refPos
  self.refPos = self.pos
  body
  self.refPos = oldRefPos

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


# Each implies all restrictions below it.
# Thus, you can do `<=` and `>=` checks. For example, `kind >= ekNoColon` to ensure colon calls are allowed.
type ExprKind* = enum
  ekNoBlock
  ekNoColon
  ekNoComma # Can't put multiple args without parens (think Nim's command mode)
  ekFull
  ekImplicitBlock # Normal(full) expressions can't be implicit blocks. Must specify.

# The fundamental funcs
proc parseExpr*(self: Parser, kind: ExprKind): Atom
proc parseTypeDesc*(self: Parser, trailing: bool): AtomRef
proc parseBlock*(self: Parser): Atom
proc parseDelimited*(self: Parser, delim: StrID, ender: StrID): seq[Atom]



proc parseDelimited*(self: Parser, delim: StrID, ender: StrID): seq[Atom] =
  echo fmt"Parsing a `{delim.lookup}` delimited list ended by `{ender.lookup}`"
  while not nextIs ender:
    if nextIs delim:
      match iComma
      continue
    echo fmt"Parsing expr when next is {self.cur}"
    result.add(self.parseExpr ekNoBlock)
    self.stopped = false
    if not nextIs delim:
      echo "Breaking"
      break
    match delim


proc parseBlock*(self: Parser): Atom =
  result = Atom(
    kind: akCompound,
    compoundKind: ckBlock,
    typeSpec: nil,
    children: @[],
  )
  withoutBlocking:
    if nextIs iColon:
      result.typeSpec = self.parseTypeDesc(true)

  withBlocking:
    while not (dedented or self.cur.kind == tkEOF):
      setRef:
        result.children.add self.parseExpr(ekFull)
        self.stopped = false

proc parseTypeDesc*(self: Parser, trailing: bool): AtomRef =
  if not nextIs iColon:
    return nil

  let innerKind = if trailing: ekNoColon else: ekNoBlock

  match iColon
  new result
  result[] = self.parseExpr innerKind

  if trailing: match iColon



proc parseAccessible*(self: Parser): Atom =
  if nextIs { tkWord, tkStrop }:
    result = Atom(
      kind: akWord,
      str: self.advance.id,
    )
  elif nextIs { tkStr }:
    result = Atom(
      kind: akStr,
      str: self.advance.id,
    )
  else:
    err fmt"Expected either a word or a string. Found {self.cur}"
  # echo fmt"Got a {result}"


# Handles all table driven stuff
proc parseRedirectable*(self: Parser, mayAccess = true): Atom =
  # First check needed to stop `"if"` (a string) from being a keyword
  if self.cur.kind in {tkWord, tkPunc} and ControlWords.contains self.cur.id:
    # `if`, `while`, `let`, `{} blocks`
    # All control structures set stopped, but stuff like `{}` or `()` don't.
    descend:
      result = ControlWords[self.cur.id](self)
      if self.stopped and not mayAccess:
        err "May not have an unambiguous stop in a non-accessible redirectable."
  else:
    # Default handlers for strings, chars, and non-control words
    result = self.parseAccessible()

  while self.isAccess or (not newlined and self.attached and nextIs [iLParen, iLBracket]):
    echo fmt"Parsing a postop on {self.cur}"
    if nextIs iLParen:
      withoutBlocking:
        match iLParen
        result = Atom(
          kind: akCmd,
          cmd: iCall,
          args: @[result].concat self.parseDelimited(delim=iComma, ender=iRParen),
        )
        match iRParen
    elif nextIs iLBracket:
      withoutBlocking:
        match iLBracket
        result = Atom(
          kind: akCmd,
          cmd: iIndex,
          args: self.parseDelimited(delim=iComma, ender=iRBracket)
        )
        match iRBracket
    else: #self.isAccess:
      result = Atom(
        kind: akCmd,
        cmd: self.advance.id,
        # mayAccess=false guards against self.stopped.
        # Doing it recursively with a "base-case" mayAccess=false allows tuples and the like
        # to be parsed from this.
        args: @[result, self.parseRedirectable(mayAccess=false)]
      )

proc parseUnary(self: Parser): Atom =
  descend: result = self.parseRedirectable()
  # Parsing things like `if` or `when` will self.stopped and properly prevent
  # us from treating them as unary operators. Thus, we can fearlessly treat anything
  # that gets here as "unary operator"-able
  if not (self.isBin or newlined) and self.cur.kind notin { tkPunc }: # If the next op is a marked binop, we need to fall back to parseBinary
    result = Atom(
      kind: akCmd,
      cmd: iCall,
      args: @[result, self.parseUnary()]
    )

proc parseBinary(self: Parser, prec = 0): Atom =
  template nextLevel(): Atom =
    if prec == MaxPrec: self.parseUnary
    else: self.parseBinary(prec + 1)
  descend: result = nextLevel()
  while self.cur.kind in {tkWord, tkSigil} and Precedences.getOrDefault(self.cur.id, -1) == prec:
    descend:
      result = Atom(
        kind: akCmd,
        cmd: self.advance.id,
        args: @[result, nextLevel()]
      )

proc parseExpr*(self: Parser, kind: ExprKind): Atom = self.parseBinary()


proc parse*(lexer: Lexer): Atom =
  var self = Parser(
    lexer: lexer,
    pos: (-1, -1, 0),
    refPos: (-1, -1, 0),
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
