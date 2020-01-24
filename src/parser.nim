import tables
import options

import tokens
import lexer
import nodes
template dowhile(cond, body: untyped): untyped =
  body
  while cond:
    body

# Must be defined here because I want nothing beyond the parser to know about Tags/Tokens/Lexers
proc toCommand(t: Tag, inBind: static[bool] = true): Command =
  case t:
    of Tag.Let: return Let
    of Tag.Var: return Var
    of Tag.CVar: return CVar
    of Tag.Property: return Property
    of Tag.Field: return Field
    of Tag.Enum:
      # Enum bindtype vs the operator
      when inBind: return Enum
      else: return EnumDef
    of Tag.Alias: return Alias
    of Tag.Add: return Add
    of Tag.AddAssign: return AddAssign
    of Tag.And: return And
    of Tag.Array: return Array
    of Tag.AShr: return AShr
    of Tag.Assert: return Assert
    of Tag.Assign: return Assign
    of Tag.BitAnd: return BitAnd
    of Tag.BitAndAssign: return BitAndAssign
    of Tag.BitNot: return BitNot
    of Tag.BitNotAssign: return BitNotAssign
    of Tag.BitOr: return BitOr
    of Tag.BitOrAssign: return BitOrAssign
    of Tag.BitXor: return BitXor
    of Tag.BitXorAssign: return BitXorAssign
    of Tag.Block: return RawBlock
    of Tag.Break: return Break
    of Tag.ClosedRange: return ClosedRange
    of Tag.Comptime: return Comptime
    of Tag.Const: return Const
    of Tag.Div: return Div
    of Tag.DivAssign: return DivAssign
    of Tag.DoWhile: return DoWhile
    of Tag.Equal: return Eq
    of Tag.FieldAccess: return Access
    of Tag.Fn: return FnDef
    of Tag.For: return For
    of Tag.Greater: return Greater
    of Tag.GreaterEql: return GreaterEq
    of Tag.If: return If
    of Tag.In: return In
    of Tag.NotIn: return NotIn
    of Tag.Inline: return Inline
    of Tag.Less: return Less
    of Tag.LessEql: return LessEq
    of Tag.Loop: return Loop
    of Tag.Mod: return Mod
    of Tag.Mul: return Mul
    of Tag.MulAssign: return MulAssign
    of Tag.NotEqual: return NotEq
    of Tag.Not: return Not
    of Tag.NullLit: return Null
    of Tag.OpenRange: return OpenRange
    of Tag.Optional: return Optional
    of Tag.Or: return Or
    of Tag.Proc: return Proc
    of Tag.Pure: return Pure
    of Tag.Return: return Return
    of Tag.Shl: return Shl
    of Tag.ShlAssign: return ShlAssign
    of Tag.Shr: return Shr
    of Tag.ShrAssign: return ShrAssign
    of Tag.Sink: return Sink
    of Tag.Slice: return Command.Slice
    of Tag.Spaceship: return Spaceship # Spaceship only tentative
    of Tag.Struct: return StructDef
    of Tag.Sub: return Sub
    of Tag.SubAssign: return SubAssign
    of Tag.Test: return Test
    of Tag.Undef: return Undef
    of Tag.Use: return Use
    of Tag.While: return While
    of Tag.Xor: return Xor
    # Symbols/Labels/etc aren't handled here
    # 
    else: assert false

type FilePos = tokens.FilePos
type Parser = object
  indent: uint # For debug
  imIn: string
  lexer: Lexer
  cur: Token
  # The position of the last token
  # Only used for disambiguating stuff like
  # let x = foo
  # (x, blah).bar()
  # (i.e () and [] must be on the same line as their access expr)
  lastPos: FilePos

type ReqLevel = enum
  Can, Cant, Must

template imParsing(what: string)=
  self.indent += 1
  let oldImIn = self.imIn
  self.imIn = what
  defer:
    self.indent -= 1
    self.imIn = oldImIn

# Kick the lexer to give us the new token
# Returns the old token
proc advance(self: var Parser): Token =
  result = self.cur
  self.lastPos = self.cur.pos
  self.cur = self.lexer.scan()

proc nextIs(self: var Parser, what: set[Tag]): bool =
  return self.cur.tag in what
proc nextIs(self: var Parser, what: Tag): bool =
  return self.cur.tag == what

proc tryMatch(self: var Parser, what: set[Tag]): Option[Token] =
  for arg in what:
    if self.nextIs arg:
      var indent = ""
      for i in 0..self.indent: indent &= " "
      # echo indent, self.imIn, " matched a ", self.cur
      return some(self.advance())
proc tryMatch(self: var Parser, what: Tag): Option[Token] =
  return self.tryMatch({what})
proc match(self: var Parser, what: set[Tag]): Token =
  let res = self.tryMatch(what)
  if not res.isSome:
    echo "Expected a ", what, " at ", self.cur.pos
  result = res.get
proc match(self: var Parser, what: Tag): Token =
  return self.match({what})

proc parseBlock(self: var Parser, requireBraces: static[bool] = true): Expr
proc parseExpr(self: var Parser): Expr
proc parseBinExpr(self: var Parser, startAt: static[BinLevel]): Expr

# Recursive parser for tuple unpacks
# Returns that level's binds and types
# Parses either (bindLevel...) or a single bindLevel
# Note that the 'bottom' of types may have more nested layers (as they're expressions)
# Use either binds or pubs to determine nesting depth
proc parseBindLevel(self: var Parser): tuple[binds: Expr, types: Expr, pubs: Expr] =
  template tupleAtCurPos(): Expr =
    Expr(pos: self.cur.pos, cmd: Tuple, args: @[])

  # They want to unpack a tuple, gotta recurse
  if self.nextIs Tag.LParen:
    result.binds = tupleAtCurPos
    result.types = tupleAtCurPos
    result.pubs = tupleAtCurPos

    dowhile self.tryMatch(Tag.Comma).isSome:
      let lev = self.parseBindLevel()
      result.binds.args.add lev.binds
      result.types.args.add lev.types
      result.pubs.args.add lev.pubs
    discard self.match(Tag.RParen)
  else:
    result.binds = Expr(pos: self.cur.pos, cmd: Symbol, symbol: self.match(Tag.Symbol).lexeme)
    result.pubs = Expr(pos: self.cur.pos, cmd: Bool, boolVal: self.tryMatch(Tag.Mul).isSome)
    if self.tryMatch(Tag.Separator).isSome:
      result.types = self.parseBinExpr(below Assignment)
    else:
      # Using 'undef' as the new 'any' type
      result.types = Expr(pos: self.cur.pos, cmd: Undef)
      
# Parse a bindLevel, using bindType as the default type (type as in let/var/etc), along with a default
# Whatever calls this function must take care of parsing let/var/etc, as well as commas
proc parseBind(self: var Parser, assumeType: Command): Expr =
  result = Expr(pos: self.cur.pos, cmd: assumeType)
  let lev = self.parseBindLevel()
  result.args = @[lev.binds, lev.types, lev.pubs]

  # The default value
  if self.tryMatch(Tag.Assign).isSome:
    result.args.add self.parseBinExpr(below Assignment)
  else:
    result.args.add Expr(pos: self.cur.pos, cmd: Undef)
  

proc parseIf(self: var Parser): Expr =
  result = Expr(
    pos: self.cur.pos,
    cmd: If,
    args: @[Expr(pos: self.cur.pos, cmd: Tuple, args: @[])]
  )
  discard self.match(Tag.If)
  var arms: List = @[]
  while true:
    var arm = Expr(pos: self.cur.pos, cmd: Tuple, args: @[])
    arm.args.add self.parseExpr()
    if self.tryMatch(Tag.StoreIn).isSome:
      # Kinda clunky, I know, but I think it's better than marking them pub or whatever
      if self.tryMatch(Tag.Var).isSome:
        arm.args.add self.parseBind(assumeType=Var)
      else:
        arm.args.add self.parseBind(assumeType=Let)
    else:
      arm.args.add Expr(pos: self.cur.pos, cmd: Undef)
    arm.args.add self.parseBlock(requireBraces=true)
    arms.add  arm
    if self.tryMatch(Tag.ElIf).isNone: break
  result.args.add arms
  if self.tryMatch(Tag.Else).isSome:
    result.args.add self.parseBlock(requireBraces=true)
  else:
    result.args.add Expr(pos: self.cur.pos, cmd: Undef)
    

proc parseCondLoop(self: var Parser): Expr =
  return

proc parseLoop(self: var Parser): Expr =
  return

proc parseCompoundLit(self: var Parser): Expr =
  return

proc parseAccess(self: var Parser): Expr =
  return

proc parseCall(self: var Parser): Expr =
  result = self.parseAccess()

# Helps ensure you can only use an access/call on an Atom
proc parseBase(self: var Parser): Expr =
  case self.cur.tag:
    of Tag.If: return self.parseIf()
    of Tag.While, Tag.For: return self.parseCondLoop()
    of Tag.Symbol, Tag.IntLit, Tag.StringLit: return self.parseAccess()
    of Tag.LBracket: return self.parseCompoundLit()
    of Tag.LBrace: return self.parseBlock()
    else:
      quit "Didn't expect a " & $self.cur.tag

proc parseUnary(self: var Parser): Expr =
  result = self.parseBase()

# Welcome to the temple of recursion.
# Please go to the temple of recursion for further instructions.
proc parseBinExpr(self: var Parser, startAt: static[BinLevel]): Expr =
  template getNext():Expr =
    when startAt == BinLevel.high: self.parseUnary()
    else: self.parseBinExpr(BinLevel(startAt.ord + 1)) 
  result = getNext()
  var op = self.tryMatch(binOps[startAt])
  if op.isSome:
    result = Expr(pos: op.get.pos, cmd: op.get.tag.toCommand, args: @[result, self.parseBinExpr(startAt)])

proc parseExpr(self: var Parser): Expr =
  self.parseBinExpr(Assignment)

# Parse a sequence of expressions
proc parseBlock(self: var Parser, requireBraces: static[bool] = true): Expr =
  result = Expr(pos: self.cur.pos, cmd: Block, label: none(string), subtree: @[])
  if requireBraces:
    let label = self.tryMatch(Tag.Label)
    if label.isSome:
      result.label = some(label.get.lexeme)
    discard self.match(Tag.LBrace)

  let closer = when requireBraces: Tag.RBrace else: Tag.EOF

  while self.cur.tag != closer:
    let pos = self.cur.pos
    case self.cur.tag:
      of Tag.Let, Tag.Var, Tag.CVar, Tag.Property, Tag.Field, Tag.Enum, Tag.Alias:
        let assumeType = self.cur.tag.toCommand(inBind=true)
        discard self.match(bindSpecs)
        while true:
          result.subtree.add self.parseBind(assumeType)
          if self.tryMatch(Tag.Comma).isNone: break
      of Tag.Assert:
        discard self.match(Tag.Assert)
        result.subtree.add Expr(pos: pos, cmd: Command.Assert, args: @[self.parse_expr()])
      of Tag.Test:
        discard self.match(Tag.Test)
        let nameStr = self.match(Tag.StringLit)
        let name = Expr(pos: nameStr.pos, cmd: String, strVal: nameStr.lexeme)
        let body = self.parseBlock()
        # Thus we technically discard any extra label given to a test block
        # TODO: Should we just make the label of the block the test block's name?
        result.args.add Expr(pos: pos, cmd: Test, label: some(nameStr.lexeme), subtree: body.subtree)
      else:
        result.subtree.add self.parseExpr()
    
  discard self.match(closer)

# The only public view into this module
proc parse*(lexer: sink Lexer): Expr =
  let cur = lexer.scan()
  var parser = Parser(lexer: lexer, cur: cur, lastPos: (0.uint, 0.uint), indent: 0)
  return parser.parseBlock(requireBraces=false)
