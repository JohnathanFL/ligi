import tables
import options

import tokens
import tags
import lexer

import cmds
import nodes

type Parser = object
  indent: uint # For debug
  imIn: string
  lexer: Lexer
  cur: Token
  # The position of the last token
  # Only used for disambiguating stuff like
  # let x = foo
  # (x, blah).bar()
  # (i.e (), [], and {} must be on the same line as their access)
  lastPos: FilePos


type ReqLevel = enum
  Can, Cant, Must

type ParseError = ref object of Exception

# Returns the old token
proc advance(self: var Parser): Token =
  result = self.cur
  self.lastPos = self.cur.pos
  self.cur = self.lexer.scan()
  #echo "Advanced to ", self.cur


proc nextIs(self: var Parser, what: set[Tag]): bool =
  return self.cur.tag in what
proc tryMatch(self: var Parser, what: set[Tag]): bool = # Only for when we don't need pos or whatever
  if self.cur.tag in what:
    discard self.advance()
    return true
  else:
    return false
proc match(self: var Parser, what: set[Tag]): Token =
  if not self.nextIs what:
    echo "Expected a " & $what & " at " & $self.cur.pos
    assert false
  result = self.advance()
# Returns whether there's a '(' or '[' on the *same* line as the last token
proc canCall(self: var Parser): bool = self.cur.tag in {Tag.LParen, Tag.LBracket} and self.cur.pos.line == self.lastPos.line

# Assume we have self templates
# Just sugars so we don't have to write self. all the time
template nextIs(what: set[Tag]): bool = self.nextIs(what)
template nextIs(what: Tag): bool = nextIs {what}
template tryMatch(what: set[Tag]): bool = self.tryMatch(what)
template tryMatch(what: Tag): bool = tryMatch {what}
template match(what: set[Tag]): Token = self.match(what)
template match(what: Tag): Token = match {what}


proc parseBinLevel(self: var Parser, level: static[BinLevel] = Assignment): Expr
proc parseBlock(self: var Parser, braced: static[bool] = true): Block


# Only the location of the bind itself
# Note that this means you must specify types on a symbol-by-symbol basis. You can't do it for an entire tuple
# 'void' is allowed as a synonym for '_' for use in functions. Just don't be a dumdum.
# ('_'|'void') | ('bindloc{',' bindloc}')' | symbol ['*'][':' expr]
proc parseBindLoc(self: var Parser): BindLoc =
  if tryMatch {Tag.Sink, Tag.Void}:
    result = BindSink()
  elif tryMatch Tag.LParen:
    var res = BindTuple(locs: @[])
    while not nextIs Tag.RParen:
      res.locs.add self.parseBindLoc()
      if not tryMatch Tag.Comma: break
    if res.locs.len == 0: quit "Expected at least one BindLoc at " & $self.cur.pos
    discard match Tag.RParen
    result = res
  else:
    var res = BindSym()
    res.loc = match(Tag.Symbol).lexeme
    res.isPub = tryMatch Tag.Mul
    if tryMatch Tag.Separator:
      res.ty = self.parseBinLevel(below Assignment)
    result = res
    #echo "BindLoc was a symbol"
# Parse the bind itself
proc parseBind(self: var Parser, spec: BindCmd, mayDefault = true): Bind =
  result = Bind(pos: self.cur.pos, cmd: spec, loc: self.parseBindLoc())
  if tryMatch Tag.Assign:
    result.init = self.parseBinLevel(below Assignment)

# Not to be confused with parseBind
# This parses the top-level statement.
# ('let' | 'var' | 'cvar' | 'field' | 'property') bind { ',' bind }
proc parseBindStmt(self: var Parser, into: var Block) =
    let tok = match BindSpecs
    while true:
      into.add self.parseBind(spec=tok.tag.BindCmd)
      if not tryMatch Tag.Comma: break

# TODO: Investigate syntactic backlash if I also allowed `if expr: val` syntax (no braces needed)
  # If I allowed that, I could actually skip the {} form altogether, since {} is an expression
# Parses only the arm of the ifarm, not the 'if' or 'elif'
# expr [ '->' bind ] block
proc parseIfArm(self: var Parser): IfArm =
  result.cond = self.parseBinLevel(below Assignment)
  if tryMatch Tag.StoreIn:
    let spec = if tryMatch Tag.Var: BindCmd.Var else: BindCmd.Let
    result.capture = self.parseBind(spec, mayDefault=false)
  result.val = self.parseBlock()
  

# 'if' ifArm {ifArm} [ 'else' block ] [ 'finally' block ]
proc parseIf(self: var Parser): If =
  let pos = match(Tag.If).pos
  result = If(pos: pos, arms: @[])
  while true:
    result.arms.add self.parseIfArm()
    if not tryMatch Tag.ElIf: break

  if tryMatch Tag.Else:
    result.default = self.parseBlock()
  if tryMatch Tag.Finally:
    result.final = self.parseBlock()

# 'loop' [ '->' bind ] block [ 'finally' block ]
proc parseLoop(self: var Parser): Loop =
  let pos = match(Tag.Loop).pos
  result = Loop(pos: pos)
  if tryMatch Tag.StoreIn:
    result.counter = self.parseBind(spec=BindCmd.Let, mayDefault=false)
  result.body = self.parseBlock()

# ('while'|'for') expr [ '->' bind [ ',' bind ] ] block [ 'finally' block ]
proc parseCondLoop(self: var Parser): CondLoop =
  let tok = match {Tag.While, Tag.For}
  let pos = tok.pos
  let exprIsRange = tok.tag == Tag.For
  result = CondLoop(pos: pos, exprIsRange:exprIsRange)
  result.expr = self.parseBinLevel(below Assignment)
  if tryMatch Tag.StoreIn:
    # First comes capture, then counter
    if tryMatch Tag.Var:
      result.capture = self.parseBind(spec=BindCmd.Var, mayDefault=false)
    else:
      result.capture = self.parseBind(spec=BindCmd.Let, mayDefault=false)
    if tryMatch Comma:
      result.counter = self.parseBind(spec=BindCmd.Let, mayDefault=false)
  result.body = self.parseBlock()
proc parseUntilLoop(self: var Parser): Until =
  let pos = match(Tag.Until).pos
  result = Until(pos: pos, cond: self.parseBinLevel(below Assignment))
  if tryMatch Tag.StoreIn: result.counter = self.parseBind(spec=BindCmd.Let, mayDefault=false)
  result.body = self.parseBlock()
# Thus sinks, nulls, symbols, ints, and tuples are the 'atoms' that make up the language
# symbol | string | int | '('expr{ ',' expr }')'
proc parseAtom(self: var Parser): Callable =
  if nextIs Tag.LParen: # Tuple
    # Tuple parsing has 3 forms:
      # (): NillTup. No value at all.
      # (expr): We skip making this a tuple.
      # (expr, expr,...): A normal tuple
    let pos = match(Tag.LParen).pos
    var children: seq[Expr] = @[]
    # Thus Tuple(children=[]) is valid (null tuple)
    while not nextIs Tag.RParen:
      children.add self.parseBinLevel(below Assignment)
      # Require a comma after each item to continue
      if not tryMatch Tag.Comma: break
    discard match Tag.RParen
    if children.len == 0: return NillTup(pos: pos)
    else: return Tuple(pos: pos, children: children)
  else:
    let atom = match Atoms
    case atom.tag:
      # All this need for Command., Tag., etc is exactly why Zag has #EnumLit
      of Tag.Symbol: return Symbol(pos: atom.pos, sym: atom.lexeme)
      of Tag.IntLit: return Int(pos: atom.pos, val: atom.val)
      of Tag.StringLit: return String(pos: atom.pos, val: atom.lexeme)
      else: discard # Unreachable

# Parses a call or index **after** the subject of the call has been parsed
# I.e it parses (x,y) in foo(x,y), but not foo
# It returns a call without a subject, so the caller must take care of that.
# '(' expr {',' expr}  ')' | '[' expr {',' expr} ']'
proc parseCall(self: var Parser): Call =
  # This is only ever called when we know we are calling
  let opener = match CallOps
  let closer = if opener.tag == Tag.LParen: Tag.RParen else: Tag.RBracket
  let isIndex = opener.tag == Tag.LBracket
  result = Call(pos: opener.pos, isIndex:isIndex, args: @[])

  while not nextIs closer:
    result.args.add self.parseBinLevel(below Assignment)
    if not tryMatch Tag.Comma: break
  discard match closer

# Glorified binary expression that's lower than any other binary expression
# Note that as it is now, we allow arbitrary expressions inside swizzles.
# atom [ call | index ] { '.' swizzle }
proc parseSwizzle(self: var Parser): Callable =
  # TODO: When we add '$', there needs to be another layer in between swizzle and atom
  result = self.parseAtom()
  if self.canCall: result.call = self.parseCall()
  if nextIs Tag.Access:
    let pos = match(Tag.Access).pos
    # All praise the great recursion!
    # May its glory be greater than its glory!
    result = Swizzle(pos: pos, subject: result, path: self.parseSwizzle())

# 'fn' [bind{',', bind}] -> bind
proc parseFn(self: var Parser): Fn =
  result = Fn(pos: match(Tag.Fn).pos, args: @[])
  while not nextIs StoreIn:
    # TODO: Look into allowing default args and its implications
    result.args.add self.parseBind(spec=BindCmd.Let, mayDefault=false)
    if not tryMatch Comma: break
  discard match StoreIn
  result.ret = self.parseBind(spec=BindCmd.Var, mayDefault=true)

# Thus only atoms and parenthesized expressions can be accessed/called/indexed
# if | for | while | loop | block | swizzle | undef | null | sink
proc parseBase(self: var Parser): Expr =
  case self.cur.tag:
    of Tag.If: return self.parseIf()
    of Tag.For, Tag.While: return self.parseCondLoop()
    of Tag.Fn: return self.parseFn()
    of Tag.Until: return self.parseUntilLoop()
    of Tag.Loop: return self.parseLoop()
    of Tag.Label, Tag.LBrace: return self.parseBlock()
    of Tag.Undef: return Undef(pos: match(Tag.Undef).pos)
    of Tag.NullLit: return Null(pos: match(Tag.NullLit).pos)
    of Tag.Sink, Tag.Void: return Sink(pos: match(Tag.Sink).pos)
    else: return self.parseSwizzle()

# {unaryop} access
proc parseUnary(self: var Parser): Expr =
  if nextIs UnaryOps:
    let op = match UnaryOps
    result = UnaryExpr(pos: op.pos, cmd: op.tag.UnaryCmd, target: self.parseUnary())
  else:
    result = self.parseBase()

# binlevel[level-1] {binop[level] binlevel[level-1]}
proc parseBinLevel(self: var Parser, level: static[BinLevel] = Assignment): Expr =
  template term(): Expr =
    when level == BinLevel.high: self.parseUnary() else: self.parseBinLevel((level.uint + 1).BinLevel)
  result = term()
  if nextIs BinOps[level]:
    var prevOp = self.cur.tag
    while nextIs BinOps[level]:
      let op = match BinOps[level]
      var res = BinExpr(pos: op.pos, cmd: op.tag.BinCmd, lhs: result, rhs: term())
        
      result = res
      prevOp = op.tag

# We can assume that a return will always be immediately before a '}'
# because no code could logically be found after it, and a return is always
# inside a function, which uses {}
# TODO: Maybe just change this to a 'if next token is on next line' thing?
# return (expr | '}')
proc parseReturn(self: var Parser): Return =
  let pos = match(Tag.Return).pos
  result = Return(pos:pos)
  if not nextIs RBrace:
    result.val = self.parseBinLevel(below Assignment)

# See parseReturn for '}' logic
# break [label[,expr]] # (Can only break with an expr if there's a label)
proc parseBreak(self: var Parser): Break =
  let pos = match(Tag.Break).pos
  result = Break(pos:pos)
  if nextIs Label:
    result.label = match(Label).lexeme
    if tryMatch Comma:
      result.val = self.parseBinLevel(below Assignment)
  elif not nextIs RBrace:
    result.val = self.parseBinLevel(below Assignment)

# 'assert' expr
proc parseAssert(self: var Parser): Assert =
  let pos = match(Tag.Assert).pos
  result = Assert(pos: pos, expr: self.parseBinLevel(below Assignment))
proc parseBlock(self: var Parser, braced: static[bool] = true): Block =
  new result
  when braced:
    # Only braced blocks may be labeled.
    if nextIs Tag.Label:
      let label = match(Tag.Label)
      result.label = label.lexeme
      result.pos = label.pos
      discard match Tag.LBrace
    else:
      result.pos = match(Tag.LBrace).pos
  const closer = when braced: Tag.RBrace else: Tag.EOF

  # Mmmmmmm that's sugary sweet
  while not nextIs {closer}:
    case self.cur.tag:
      of Tag.Let, Tag.Var, Tag.CVar, Tag.Property, Tag.Field, Tag.Enum, Tag.Alias:
        self.parseBindStmt(result)
      of Tag.Assert: result.add self.parseAssert()
      of Tag.Break: result.add self.parseBreak()
      of Tag.Return: result.add self.parseReturn()
      else: result.add self.parseBinLevel()

  discard match closer

# The only public view into this module
proc parse*(lexer: var Lexer): Block =
  let cur = lexer.scan()
  var parser = Parser(lexer: lexer, cur: cur, lastPos: (line: 0.uint, col: 0.uint), indent: 0, imIn: "BLOCK")
  return parser.parseBlock(braced=false)
