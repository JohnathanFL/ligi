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
  # (i.e () and [] must be on the same line as their access
  lastPos: FilePos

type
  BlockStyle{.pure.} = enum
    NoBraces, # A file. Implies NoLabel, NoLabel, NoSpec
    NoLabel, # Implies braces
    NoSpec, # No 'struct' and such. Implies NoLabel
    Normal # May be Braced/Labeled/Specced
type ReqLevel = enum
  Can, Cant, Must

type ParseError = ref object of Exception

template imParsing(what: string)=
  self.indent += 1
  let oldImIn = self.imIn
  self.imIn = what
  defer:
    self.indent -= 1
    self.imIn = oldImIn

# Returns the old token
proc advance(self: var Parser): Token =
  result = self.cur
  self.lastPos = self.cur.where
  self.cur = self.lexer.scan()

proc try_match(self: var Parser, what: set[Tag]): Option[Token] =
  for arg in what:
    if self.cur.what.tag == arg:
      var indent = ""
      for i in 0..self.indent: indent &= " "
      #echo indent, self.imIn, " at ", self.cur.where, " matched a ", self.cur.what
      return some(self.advance())

proc match(self: var Parser, what: set[Tag]): Token =
  let res = self.try_match(what)
  if not isSome res:
    echo "Expected a ", what, " at ", self.cur.where
  result = res.get

proc parse_expr(self: var Parser): Expr
proc parse_bin_level(self: var Parser, level: static[BinLevel]): Expr
proc parse_block(self: var Parser, style: static[BlockStyle]): Block
proc parse_bind(self: var Parser, spec: BindType, init: static[ReqLevel]): Bind


proc parse_struct_literal(self: var Parser, ty: Option[Expr]): StructLiteral =
  result.new()
  result.ty = ty
  while self.cur.what.tag != Tag.RBracket:
    discard self.match({Tag.FieldAccess})
    let name = self.match({Tag.Symbol}).what.lexeme
    assert not result.fields.hasKey(name)
    discard self.match({Tag.Assign})
    let val = self.parse_bin_level(below BinLevel.Assignment)
    result.fields[name] = val
    if self.cur.what.tag != Tag.RBracket: discard self.match({Tag.Comma})

proc parse_array_literal(self: var Parser, ty: Option[Expr]): ArrayLiteral =
  result.new()
  result.ty = ty
  while self.cur.what.tag != Tag.RBracket: # Allows for trailing commas
    result.children.add self.parse_bin_level(below BinLevel.Assignment)
    if self.try_match({Tag.Comma}).isNone: break # But don't allow 'x y'. Must be 'x,y'

proc parse_compound_lit(self: var Parser): CompoundLiteral =
  imParsing "CompoundLiteral"
  discard self.match({Tag.LBracket})
  let ty =
    if self.try_match({Tag.Separator}).isSome:
      let t = some(self.parse_bin_level(below BinLevel.Assignment))
      discard self.match({Tag.Separator})
      t
    else: none(Expr)
  case self.cur.what.tag:
    of Tag.FieldAccess: result =self.parse_struct_literal(ty)
    else: result = self.parse_array_literal(ty)
  discard self.match({Tag.RBracket})

proc parse_tuple(self: var Parser): Tuple =
  imParsing "Tuple"
  result.new()
  discard self.match({Tag.LParen})
  var ender = self.try_match({Tag.RParen})
  while ender.isNone:
    result.children.add self.parse_expr()
    if self.try_match({Tag.Comma}).isNone: break
    ender = self.try_match({Tag.RParen})
  # Forgive me father, for this is ugly.
  if ender.isNone: discard self.match({Tag.RParen})
  
  # We already matched the RParen by ending the loop
proc parse_enum_lit(self: var Parser): EnumLit =
  imParsing "EnumLit"
  discard self.match({Tag.Pound})
  result = new(EnumLit)
  result.tok = self.match({Tag.Symbol})
  if self.cur.what.tag == Tag.LParen: result.val = some(cast[Expr](self.parse_tuple()))

# For and While are literally the same thing when you're just parsing
# Why have 2 funcs?
proc parse_cond_loop(self: var Parser): CondLoop =
  imParsing "CondLoop"
  result.new()
  result.interpret = case self.match({Tag.While, Tag.For, Tag.DoWhile}).what.tag:
    of Tag.For: LoopType.For
    of Tag.While: LoopType.While
    of Tag.DoWhile: LoopType.DoWhile
    else: LoopType.For # Unreachable.
  result.cond = self.parse_bin_level(below BinLevel.Assignment)
  if self.try_match({Tag.StoreIn}).isSome: # They want to capture something
    # First comes counter. This is non-negotiable
    result.counter = some(self.parse_bind(BindType.Let, init=Cant))
    if self.try_match({Tag.Comma}).isSome: # They want to capture a value
      let spec =
        if self.try_match({Tag.Var}).isSome: # They want to be able to mutate it
          BindType.Var
        else:
          BindType.Let
      result.capture = some(self.parse_bind(spec, init=Cant))
  result.body = self.parse_block(Normal)
  if self.try_match({Tag.Finally}).isSome:
    result.final = some(self.parse_block(BlockStyle.Normal))
    
proc parse_loop(self: var Parser): Loop =
  imParsing "Loop"
  result.new()
  discard self.match({Tag.Loop})
  if self.try_match({Tag.StoreIn}).isSome: # They want to capture a counter
    result.counter = some(self.parse_bind(BindType.Let, init=Cant))
  result.body = self.parse_block(Normal)

proc parse_if_arm(self: var Parser): IfArm =
  imParsing "IfArm"
  result.cond = self.parse_bin_level(below BinLevel.Assignment)
  if self.try_match({Tag.StoreIn}).isSome:
    let spec =
      if self.try_match({Tag.Var}).isSome: BindType.Var
      else: BindType.Let
    result.capture = some(self.parse_bind(spec, init=Cant))
  result.val = self.parse_block(BlockStyle.Normal)
  
proc parse_if(self: var Parser): If =
  imParsing "If"
  result.new()
  discard self.match({Tag.If})
  result.arms = @[self.parse_if_arm()]
  while self.try_match({Tag.ElIf}).isSome:
    result.arms.add self.parse_if_arm()
  if self.try_match({Tag.Else}).isSome:
    result.default = some(self.parse_block(Normal))
  if self.try_match({Tag.Finally}).isSome:
    result.final = some(self.parse_block(Normal))

proc parse_fn(self: var Parser): Fn =
  # The sweetest function in this Hellish Codescape
  # ^ good name for a programming band, that
  # Disregard the sweestest bit above. The function's changed.
  imParsing "Fn"
  result.new()
  result.args = @[]
  discard self.match({Tag.Fn})
  while self.cur.what.tag notin {Tag.StoreIn, Tag.LBrace}:
    result.args.add self.parse_bind(BindType.Let, init=Cant)
    if self.try_match({Tag.Comma}).isNone: break

  if self.try_match({Tag.StoreIn}).isSome:
    result.ret = some(self.parse_bind(BindType.Var, init=Cant))

  result.body = self.parse_block(BlockStyle.NoLabel)
      
# Note I use 'atom' here in a different context than an Expr-Atom
# It just means something below operator-level
proc parse_atom(self: var Parser): Expr =
  imParsing "Atom"
  case self.cur.what.tag:
    of LParen: return self.parse_tuple()
    of IntLit, NullLit, StringLit, CharLit, Symbol: return Atom(tok: self.match(atoms))
    of LBracket: return self.parse_compound_lit()
    of Pound: return self.parse_enum_lit()
    of LBrace: return self.parse_block(BlockStyle.Normal)
    of Tag.Struct, Tag.Enum: return self.parse_block(BlockStyle.NoLabel)
    of Tag.Void: return Atom(tok: self.match({Tag.Void}))
    of Tag.Undef: return Atom(tok: self.match({Tag.Undef}))
    of Tag.Sink: return Atom(tok: self.match({Tag.Sink}))
    of Tag.For, Tag.While: return self.parse_cond_loop()
    of Tag.Loop: return self.parse_loop()
    of Tag.If: return self.parse_if()
    of Tag.Fn: return self.parse_fn()
    else:
      echo "Didn't expect a ", self.cur
      assert false


# Essentially a stripped down version of parsing a symbol/lit/tuple
proc parse_swizzle(self: var Parser): Path =
  # The '.' has already been matched
  imParsing "Swizzle"

  # Either it's foo.bar, or it's foo.(...)
  if self.try_match({Tag.LParen}).isSome:
    var res = new SwizzlePath
    result = res
    # Can't do a foo.()
    # Must be at least one swizzler inside
    res.paths = @[]
    # As always, this style allows for foo.(bar,) (trailing comma)
    while self.try_match({Tag.RParen}).isNone:
      res.paths.add self.parse_swizzle()
      if self.try_match({Tag.Comma}).isNone: break
    if self.try_match({Tag.FieldAccess}).isSome:
      res.next = some(self.parse_swizzle())
  elif self.cur.what.tag in validSwizzles: # foo.(bar[.0])
    result = AccessPath(name: self.match(validSwizzles))
    if self.try_match({Tag.FieldAccess}).isSome:
      result.next = some(self.parse_swizzle())
  else:
    echo "INVALID SWIZZLE"
    assert false
      
# The lhs of an access may be any valid expression
# The rhs, however, must be one of:
  # Symbol, IntLit, StringLit
    # Each may be further accessed
  # Tuple of more rhs
proc parse_access(self: var Parser): Expr =
  imParsing "Access"
  # Could be: tuple, symbol, IntLit, etc
  result = self.parse_atom()
  if self.try_match({Tag.FieldAccess}).isSome:
    result = Access(accessed: result, path: self.parse_swizzle())


# {something}(args) or {something}[args], over and over
proc parse_call(self: var Parser): Expr =
  imParsing "Call"
  result = self.parse_access()

  # This one gets us infinite ()()()[][]()()
  while self.lastPos.line == self.cur.where.line and self.cur.what.tag in callOps:
    let op = self.match(callOps)
    let closer = if op.what.tag == Tag.LParen: Tag.RParen else: Tag.RBracket
    # Thus ( and [ are used as the 'functions' to be called
    # Also note this allows us to do stuff like matrix[0, 0] (multi index params)
    result = Call(fn: op, args: @[result])
    # This one gets us infinite arg1, arg2, arg3,
    while true:
      if self.cur.what.tag == closer: break
      cast[Call](result).args.add self.parse_bin_level(below BinLevel.Assignment)
      if self.try_match({Tag.Comma}).isNone: break
      # Thus we allow trailing commas, but not space separated args
    discard self.match({closer})
        
    

proc parse_unary(self: var Parser): Expr =
  imParsing "Unary"
  var curOp = self.try_match(unaryOps)
  if curOp.isNone:
    result = self.parse_call()
  else:
    # I put the recursive in recursive descent
    result = Call(fn: curOp.get, args: @[self.parse_unary])
# Parse an expr that can have any binops at/below level
# Use the templte 'below' to specify only a binop below that level, as in
# parse_bin_level(below BinLevel.Assignment)
proc parse_bin_level(self: var Parser, level: static[BinLevel]): Expr =
  imParsing "Expr." & $level
  result = when level == BinLevel.high:
    self.parse_unary()
  else: self.parse_bin_level(BinLevel(level.ord + 1))
  
  var op = self.try_match(binOps[level])
  # As far as I can tell on paper at least, this should preserve associativity
  if op.isSome:
    result = Call(fn: op.get, args: @[result, self.parse_bin_level(level)])

# Parse an expr that can have any binops in it (i.e starts from Assignment down)
proc parse_expr(self: var Parser): Expr =
  imParsing "Expr"
  return self.parse_bin_level(BinLevel.Assignment)

# TODO
proc parse_test(self: var Parser): Stmt =
  imParsing "Test"
  result.new()

proc parse_assert(self: var Parser): Assert =
  imParsing "Assert"
  result.new()
  discard self.match({Tag.Assert})
  result.expr = self.parse_expr()

# Each bindloc is either a tuple (bindloc,+) or a single location sym[:type]
# Initialization is delegated to the bind itself
proc parse_bindloc(self: var Parser): BindLoc =
  imParsing "BindLoc"
  if self.try_match({Tag.LParen}).isSome: # (bindloc,+)
    let res = new(BindTup)
    result = res
    res.children.add self.parse_bindloc()
    while self.try_match({Tag.Comma}).isSome:
      res.children.add self.parse_bindloc()
    discard self.match({Tag.RParen})
  else: # sym[: type]
    let res = new(BindSym)
    result = res
    res.loc = self.match({Tag.Symbol, Tag.Sink})
    res.pub = self.try_match({Tag.Mul}).isSome
    if self.try_match({Tag.Separator}).isSome:
      # Can't allow assignments here anyway. It'd be ambiguous
      res.ty = some(self.parse_bin_level(below BinLevel.Assignment))

proc parse_bind(self: var Parser, spec: BindType, init: static[ReqLevel]): Bind =
  imParsing "Bind"
  result.new()
  result.interpret = spec

  result.loc = self.parse_bindloc()
  when init != Cant:
    if self.try_match({Tag.Assign}).isSome:
      # Start parsing below an Assignment, since assigns shall always return void
      result.default = some(self.parse_bin_level(below BinLevel.Assignment))

# Basically just here so we don't have to use a hacky doWhile template
# Parses a series of Binds, each with its own set of BindLoc(s) and a single initializer, separated by commas
template parse_binds(self: var Parser, into: var Block, spec, init: static[ReqLevel]) =
  imParsing "Binds"
  assert spec != Can # Should always be Cant or Must
  let interpret = when spec != Cant:
    self.match(bindSpecs).what.tag.asBindType
  else:
    # As with functions, any bind not explicitly var'd is immutable
    BindType.Let
    
  into.children.add self.parse_bind(interpret, init)
  while self.try_match({Tag.Comma}).isSome:
    into.children.add self.parse_bind(interpret, init)

proc parse_break(self: var Parser): Break =
  result.new()
  discard self.match({Tag.Break})
  result.label = self.try_match({Tag.Label})
  if self.try_match({Tag.Comma}).isSome:
    result.val = some(self.parse_bin_level(below BinLevel.Assignment))

proc parse_return(self: var Parser): Return =
  result.new()
  discard self.match({Tag.Return})
  # Since a return is only valid in a function, and nothing is valid after a return,
  # we can assume that the next tokens are either the return expression or the end of a block
  if self.cur.what.tag != Tag.RBrace:
    result.val = some(self.parse_bin_level(below BinLevel.Assignment))

proc parse_block(self: var Parser, style: static[BlockStyle]): Block =
  imParsing "Block(" & $style & ")"
  result.new()
  result.interpret = BlockInterpret.Sequence
  when style != BlockStyle.NoSpec:
    let spec = self.try_match({Tag.Struct, Tag.Enum})
    if spec.isSome:
      result.interpret = case spec.get.what.tag:
        of Tag.Struct: BlockInterpret.Struct
        of Tag.Enum: BlockInterpret.Enum
        else: BlockInterpret.Sequence # Unreachable

  when style != BlockStyle.NoLabel:
    # Mmmmmm that's satisfying
    result.label = self.try_match({Tag.Label})

  when style != BlockStyle.NoBraces: discard self.match({Tag.LBrace})

  let closer = when style == BlockStyle.NoBraces: Tag.EOF else: Tag.RBrace
  while self.cur.what.tag != closer:
    case self.cur.what.tag:
      # SeparatorSemicolons are just optional punctuation / disambiguators. Just run through them
      of Tag.Semicolon:
        discard self.match({Tag.Semicolon})
      of Tag.Let, Tag.Var, Tag.Property, Tag.Field, Tag.Enum, Tag.CVar:
        self.parse_binds(into=result, spec=Must, init=Can)
      of Tag.Assert:
        result.children.add self.parse_assert()
      of Tag.Test:
        result.children.add self.parse_test()
      of Tag.Break:
        result.children.add self.parse_break()
      of Tag.Return:
        result.children.add self.parse_return()
      else:
        #echo "Falling back to an expr"
        result.children.add self.parse_expr()
    #echo $result.children[result.children.len - 1]

  discard self.match({closer})



# The only public view into this module
proc parse*(lexer: sink Lexer): Block =
  let cur = lexer.scan()
  var parser = Parser(lexer: lexer, cur: cur, lastPos: FilePos(line: 0, col: 0), indent: 0, imIn: "BLOCK")
  return parser.parse_block(BlockStyle.NoBraces)
