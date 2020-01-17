import options

import tokens
import lexer
import nodes

type Parser = object
  lexer: Lexer
  cur: Token
  curPos: FilePos

type
  BlockStyle{.pure.} = enum
    NoBraces, # A file. Implies NoLabel, NoLabel, NoSpec
    NoLabel, # Implies braces
    NoSpec, # No 'struct' and such. Implies NoLabel
    Normal # May be Braced/Labeled/Specced
type ReqLevel = enum
  Can, Cant, Must

type ParseError = ref object of Exception
proc exception(msg: string): ParseError =
  result.new()
  result.msg = msg


# Returns the old token
proc advance(self: var Parser): Token =
  result = self.cur
  (self.curPos, self.cur) = self.lexer.scan()

proc try_match(self: var Parser, what: set[Tag]): Option[Token] =
  for arg in what:
    if self.cur.tag == arg:
      return some(self.advance())

proc match(self: var Parser, what: set[Tag]): Token =
  let res = self.try_match(what)
  if not res.isSome: raise exception("Failed to match " & $what)

# something.something.something....
proc parse_access(self: var Parser): Expr =
  result.new()

# {something}(args) or {something}[args], over and over
proc parse_call(self: var Parser): Expr =
  result = self.parse_access()

proc parse_unary(self: var Parser): Expr =
  var curOp = self.try_match(unaryOps)
  if curOp.isNone:
    return self.parse_call()
  
  while curOp.isSome:
    result = result
    
  result = new(Call)

proc parse_bin_level(self: var Parser, level: static[BinLevel]): Expr =
  let lhs = when level == BinLevel.high: self.parse_unary() else: self.parse_bin_level(BinLevel(level.ord + 1))

proc parse_expr(self: var Parser): Expr =
  return self.parse_bin_level(BinLevel.Assignment)

# TODO
proc parse_test(self: var Parser): Stmt =
  result.new()

proc parse_assert(self: var Parser): Assert =
  result.new()
  result.expr = self.parse_expr()

proc parse_bind(self: var Parser, spec, typ, init: static[ReqLevel]): Bind =
  result.new()

proc parse_block(self: var Parser, style: static[BlockStyle]): Block =
  result.new()

  when style != BlockStyle.NoSpec:
    let spec = self.try_match({Tag.Struct, Tag.Enum})
    if spec.isSome:
      result.interpret = case spec.get.tag:
        of Tag.Struct: BlockInterpret.Struct
        of Tag.Enum: BlockInterpret.Enum
        else: BlockInterpret.Sequence # Unreachable

  when style != BlockStyle.NoLabel:
    # Mmmmmm that's satisfying
    result.label = self.try_match({Tag.Label})

  when style != BlockStyle.NoBraces: discard self.match({Tag.LBrace})

  let closer = when style == BlockStyle.NoBraces: Tag.EOF else: Tag.RBrace
  while self.cur.tag != closer:
    case self.cur.tag:
      of Tag.Let, Tag.Var, Tag.Property, Tag.Field, Tag.Enum: result.children.add self.parse_bind(spec=Must, typ=Can, init=Can)
      of Tag.Assert: result.children.add self.parse_assert()
      of Tag.Test: result.children.add self.parse_test()
      else: result.children.add self.parse_expr()
  discard self.match({closer})



# The only public view into this module
proc parse*(lexer: sink Lexer): Block =
  let (curPos, cur) = lexer.scan()
  var parser = Parser(lexer: lexer, cur: cur, curPos: curPos)
  return parser.parse_block(BlockStyle.NoBraces)
