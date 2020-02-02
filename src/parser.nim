import tables
import options

import tokens
import lexer
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
  echo "Advanced to ", self.cur


proc nextIs(self: var Parser, what: set[Tag]): bool =
  return self.cur.tag in what
proc tryMatch(self: var Parser, what: set[Tag]): Option[Token] =
  if self.nextIs what:
    return some(self.advance())
  else:
    return none(Token)
proc match(self: var Parser, what: set[Tag]): Token =
  let res = self.tryMatch(what)
  if not isSome res:
    echo "Expected a " & $what & " at " & $self.cur.pos
    assert false
  result = res.get

# Assume we have self templates
# Just sugars so we don't have to write self. all the time
template nextIs(what: set[Tag]): bool = self.nextIs(what)
template nextIs(what: Tag): bool = nextIs {what}
template tryMatch(what: set[Tag]): Option[Token] = self.tryMatch(what)
template tryMatch(what: Tag): Option[Token] = tryMatch {what}
template match(what: set[Tag]): Token = self.match(what)
template match(what: Tag): Token = match {what}

proc parseBinLevel(self: var Parser, level: static[BinLevel] = Assignment): Expr


# Not to be confused with parseBind
# ('let' | 'var' | 'cvar' | 'field' | 'property') bind { ',' bind }
proc parseBindStmt(self: var Parser):Bind =
    return

# Parses only the arm of the ifarm, not the 'if' or 'elif'
# expr [ '->' bind ] block
proc parseIfArm(self: var Parser): Expr = return

# 'if' ifArm {ifArm} [ 'else' block ] [ 'finally' block ]
proc parseIf(self: var Parser): Expr = return

# 'loop' [ '->' bind ] block [ 'finally' block ]
proc parseLoop(self: var Parser): Loop = return

# ('while'|'for') expr [ '->' bind [ ',' bind ] ] block [ 'finally' block ]
proc parseCondLoop(self: var Parser): Loop = return

# Thus sinks, nulls, symbols, ints, and tuples are the 'atoms' that make up the language
# sink | null | symbol | int | '('expr{ ',' expr }')'
proc parseAtom(self: var Parser): Expr =
  if nextIs Tag.LParen: # Tuple
    let pos = match(Tag.LParen).pos
    var res = Tuple(pos: pos, cmd: Command.Tuple, children: @[])
    result = res
    # Thus Tuple(children=[]) is valid (null tuple)
    while not nextIs Tag.RParen:
      res.children.add self.parseBinLevel(below Assignment)
      # Require a comma after each item to continue
      if tryMatch(Tag.Comma).isNone: break
    discard match Tag.RParen
  else:
    let atom = match Atoms
    case atom.tag:
      # All this need for Command., Tag., etc is exactly why Zag has #EnumLit
      of Tag.Sink: return Sink(pos: atom.pos, cmd: Command.Sink)
      of Tag.NullLit: return Null(pos: atom.pos, cmd: Command.Null)
      of Tag.Symbol: return Symbol(pos: atom.pos, cmd: Symbol, sym: atom.lexeme)
      of Tag.IntLit: return Int(pos: atom.pos, cmd: Int, val: atom.val)
      else: discard # Unreachable
      

# Parses a call or index **without** the subject of the call.
# I.e it parses (x,y) in foo(x,y), but not foo
# It returns a call without a subject, so the caller must take care of that.
# '(' expr {',' expr}  ')' | '[' expr {',' expr} ']'
proc parseCall(self: var Parser): Expr = return

# Just parses a glorified binary expression that can only have atoms and '.'s inside
# ( '('swizzle{',' swizzle}')' | atom ) [ call|index ] [ '.' swizzle ]
proc parseSwizzle(self: var Parser): Expr = return

# Basically just a glorified parseBinLevel that restricts the rhs (and all interior lhs-s)
# to swizzles
# atom {'.' swizzle}
proc parseAccess(self: var Parser): Expr =
  result = self.parseAtom()
  if nextIs Tag.Access:
    let op = match Tag.Access
    result = BinExpr(pos: op.pos, cmd: Access, lhs: result, rhs: self.parseSwizzle())

# Thus only atoms and parenthesized expressions can be accessed/called/indexed
# if | for | while | loop | block | access
proc parseBase(self: var Parser): Expr =
  case self.cur.tag:
    of Tag.If: return self.parseIf()
    of Tag.For, Tag.While: return self.parseCondLoop()
    of Tag.Loop: return self.parseLoop()
    of Tag.Label, Tag.LBrace: return self.parseBlock()
    else: return self.parseAccess()

# {unaryop} access
proc parseUnary(self: var Parser): Expr =
  if nextIs UnaryOps:
    let op = match UnaryOps
    result = UnaryExpr(pos: op.pos, cmd: op.tag.Command, target: self.parseUnary())
  else:
    result = self.parseAccess()

# binlevel[level-1] {binop[level] binlevel[level-1]}
proc parseBinLevel(self: var Parser, level: static[BinLevel] = Assignment): Expr =
  result = when level == BinLevel.high: self.parseUnary() else: self.parseBinLevel((level.uint + 1).BinLevel)
  if nextIs BinOps[level]:
    let op = match(BinOps[level])
    result = BinExpr(pos: op.pos, cmd: op.tag.Command, lhs: result, rhs: self.parseBinLevel(level))

# We can assume that a return will always be immediately before a '}'
# because no code could logically be found after it, and a return is always
# inside a function, which uses {}
# TODO: Maybe just change this to a 'if next token is on next line' thing?
# return (expr | '}')
proc parseReturn(self: var Parser): Return =
  discard match Tag.Return
  new result
  if not nextIs RBrace:
    result.val = self.parseBinLevel(below Assignment)

# See parseReturn for '}' logic
# break [label[,expr]] # (Can only break with an expr if there's a label)
proc parseBreak(self: var Parser): Break =
  discard match Tag.Break
  new result
  if nextIs Label:
    result.label = match(Label).lexeme
    if tryMatch(Comma).isSome:
      result.val = self.parseBinLevel(below Assignment)
  elif not nextIs RBrace:
    result.val = self.parseBinLevel(below Assignment)

# 'assert' expr
proc parseAssert(self: var Parser): Assert =
  discard match Tag.Assert
  new result
  result.expr = self.parseBinLevel(below Assignment)


proc parseStmt(self: var Parser): Stmt =
  case self.cur.tag:
    of Tag.Let, Tag.Var, Tag.CVar, Tag.Property, Tag.Field:
      return self.parseBindStmt()
    of Tag.Assert: return self.parseAssert()
    of Tag.Break: return self.parseBreak()
    of Tag.Return: return self.parseReturn()
    else: return self.parseBinLevel()

proc parseBlock(self: var Parser, braced: static[bool] = false): Block =
  new result
  when braced:
    # Only braced blocks may be labeled.
    if nextIs Tag.Label:
      result.label = match(Tag.Label).lexeme
      
    discard match Tag.LBrace
  const closer = when braced: Tag.RBrace else: Tag.EOF

  # Mmmmmmm that's sugary sweet
  while not nextIs {closer}:
    result.add self.parseStmt()

  discard match closer

# The only public view into this module
proc parse*(lexer: var Lexer): Block =
  let cur = lexer.scan()
  var parser = Parser(lexer: lexer, cur: cur, lastPos: (line: 0.uint, col: 0.uint), indent: 0, imIn: "BLOCK")
  return parser.parseBlock(braced=false)
