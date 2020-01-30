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
    quit "Expected a " & $what & " at " & $self.cur.pos
  result = res.get

# Assume we have self templates
# Just sugars so we don't have to write self. all the time
template nextIs(what: set[Tag]): bool = self.nextIs(what)
template nextIs(what: Tag): bool = nextIs {what}
template tryMatch(what: set[Tag]): Option[Token] = self.tryMatch(what)
template tryMatch(what: Tag): Option[Token] = tryMatch {what}
template match(what: set[Tag]): Token = self.match(what)
template match(what: Tag): Token = match {what}
template prev(lev: BinLevel): BinLevel =
    var l = lev
    l.

proc parseBinLevel(self: var Parser, level: static[BinLevel] = Assignment): Expr

proc parseIf(self: var Parser): Expr
proc parseLoop(self: var Parser): Loop
proc parseCondLoop(self: var Parser): Loop

proc parseCall(self: var Parser): Expr
proc parseAccess(self: var Parser): Expr
proc parseUnary(self: var Parser): Expr =
    
proc parseBinLevel(self: var Parser, level: static[BinLevel] = Assignment): Expr =
    result = when level == BinLevel.high: self.parseUnary() else: self.parseBinLevel(level

proc parseReturn(self: var Parser): Return =
    discard match Return
    new result
    # Because we can assume a return will always be right before a }
    if not nextIs RBracket:
        result.val = self.parseBinLevel(below Assignment)

# break [label[,expr]] # (Can only break with an expr if there's a label)
proc parseBreak(self: var Parser): Break =
    discard match Break
    new result
    if nextIs Label:
        result.label.new
        result.label[] = match(Label).lexeme
        if self.tryMatch(Comma).isSome:
            result.val = self.parseBinLevel(below Assignment)
    
proc parseAssert(self: var Parser): Assert =
    discard match Assert
    new result
    result.val = self.parseBinLevel(below Assignment)
    
proc parseStmt(self: var Parser): Stmt =
    case self.cur.tag:
        of Let, Var, CVar, Property, Field:
            return self.parseBindStmt()
        of Assert: return self.parseAssert()
        of Break: return self.parseBreak()
        of Return: return self.parseReturn()
        else: return self.parseBinLevel()

# Only braced blocks may be labeled.
proc parseBlock(self: var Parser, braced: static[bool] = false): Block =
  new result
  when braced:
    if nextIs Tag.Label:
      result.label.new
      result.label[] = match(Tag.Label).lexeme
      
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
  return parser.parseBlock(braced=true)
