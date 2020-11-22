# Largely just calls to registerHandler and the like.

import options, sugar

import parser, ast

proc parseBind(self: Parser): Atom =
  result = Atom(
    kind: akCmd,
    cmd: self.advance.id,
    args: @[]
  )
  if indented: # Series of binds, 1 per line
    pushRef:
      setDent
      while not dedented:
        setLine
        # We leave it to the interpreter to know that `=`(`()`(`x`, `int`), `1`) is `x: int = 1`
        result.args.add self.parseExpr (NormalExpr - {canComma})
  else: # Series of binds, comma-separated, 1 line
    while not newlined:
      result.args.add self.parseExpr (AssingableExpr - {canComma})
      if not tryMatch iComma: break

registerHandler [iLet, iVar, iField, iCase, iCVar], parseBind

registerHandler [iFn, iMacro], proc(self: Parser): Atom =
  result = Atom(
    kind: akCmd,
    cmd: self.advance.id,
    args: @[]
  )
  var args: seq[Atom] = @[]
  withoutBlocking:
    while not nextIs iStoreIn:
      if nextIs iComma:
        match iComma
        continue
      else:
        args.add self.parseExpr UnblockedExpr
  result.args.add args
  # In case the `->` was on a different line
  setLine
  setDent
  match iStoreIn
  result.args.add self.parseExpr UnblockedExpr
  match iAssg
  result.args.add self.parseExpr (NormalExpr + {canImplicitBlock})



# ':' block_or_expr | expr_noColon [ ':' block_or_expr ] | expr block
# into must have `args` accessible
# `doBody` controls whether there can be a colon and body
# If there is a body, it may be an implicit block
proc parseArm(self: Parser, doBody = true): seq[Atom] = withBlocking:
  if not nextIs iColon:
    result.add self.parseExpr (UnblockedExpr - {canColon})
    if doBody and not nextIs iColon:
      result.add self.parseBlock
    elif nextIs iColon:
      result.add self.parseExpr UnblockedExpr
  elif doBody:
    match iColon
    result.add:
      if indented:
        self.parseBlock
      else:
        self.parseExpr NormalExpr
  else:
    quit "Expected one of the forms `expr:body`, `:body`, or `expr <block>`"

# word arm { slave arm } [ "else" arm ] [ "finally" arm ]
# "Slave" is the generic term for control words like "elif" or "is"
# which may only appear after their masters ("if" or "when")
proc parseSimpleControl*(self: Parser, slave = none[StrID]()): Atom =
  template newArm(c: StrID, a: seq[Atom]): Atom = Atom(
    kind: akCmd,
    cmd: c,
    args: a
  )
  result = newArm(self.cur.id, @[newArm(self.advance.id, self.parseArm)])
  if slave.isSome:
    while nextIs slave.get:
      result.args.add newArm(self.advance.id, self.parseArm)
  if nextIs iElse:
    result.args.add newArm(self.advance.id, self.parseArm)
  if nextIs iFinally:
    result.args.add newArm(self.advance.id, self.parseArm)
registerHandler [iIf], self => self.parseSimpleControl(some iElse)
registerHandler [iWhile, iFor, iLoop], self => self.parseSimpleControl()
registerHandler [iWhen], self => self.parseSimpleControl(some iIs)

# TODO: Make expect/assert distinct
registerHandler [iExpect, iAssert, iBreak, iReturn, iDelete, iContinue], self => Atom(
  kind: akCmd,
  cmd: self.advance.id,
  args: @[self.parseExpr NormalExpr]
)



registerHandler [iLBrace], proc(self: Parser): Atom =
  match iLBrace
  result = self.parseBlock()
  match iRBrace

# For the next two, note that parseUnary already checks 

registerHandler [iLBracket], proc(self: Parser): Atom = withoutBlocking:
  match iLBracket
  result = Atom(
    kind: akCompound,
    compoundKind: ckArray,
    typeSpec: self.parseTypeDesc(trailing=true),
    children: parseDelimited(iComma, nextIs iRBracket)
  )
  match iRBracket

registerHandler [iLParen], proc(self: Parser): Atom = withoutBlocking:
  match iLParen
  result = Atom(
    kind: akCompound,
    compoundKind: ckTup,
    typeSpec: self.parseTypeDesc(trailing=true),
    children: parseDelimited(iComma, nextIs iRParen)
  )
  match iRParen
