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

type ParseError = ref object of Exception
proc exception(msg: string): ParseError =
  result.new()
  result.msg = msg

# Returns the old token
proc advance(self: var Parser): Token =
  result = self.cur
  (self.curPos, self.cur) = self.lexer.scan()

proc try_match(self: var Parser, what: varargs[Tag]): Option[Token] =
  for arg in what:
    if self.cur.tag == arg:
      return some(self.advance())

proc match(self: var Parser, what: varargs[Tag]): Token =
  let res = self.try_match(what)
  if not res.isSome: raise exception("Failed to match " & $what)



proc parse_block(self: var Parser, style: BlockStyle): Block =
  result.new()

  if style != BlockStyle.NoSpec:
    let spec = self.try_match(Tag.Struct, Tag.Enum)
    if spec.isSome:
      result.interpret = case spec.get.tag:
        of Tag.Struct: BlockInterpret.Struct
        of Tag.Enum: BlockInterpret.Enum
        else: BlockInterpret.Sequence # Unreachable

  if style != BlockStyle.NoLabel:
    # Mmmmmm that's satisfying
    result.label = self.try_match(Tag.Label)

  if style != BlockStyle.NoBraces: discard self.match(Tag.LBrace)

  let closer = if style == BlockStyle.NoBraces: Tag.EOF else: Tag.RBrace
  while self.cur.tag != closer:
    case self.cur.tag:
      of Tag.Let, Tag.Var, Tag.Property, Tag.Field, Tag.Enum: self.
  discard self.match(closer)


# The only public view into this module
proc parse*(lexer: sink Lexer): Block =
  let (curPos, cur) = lexer.scan()
  var parser = Parser(lexer: lexer, cur: cur, curPos: curPos)
  return parser.parse_block(BlockStyle.NoBraces)
