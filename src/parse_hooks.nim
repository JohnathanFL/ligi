# Largely just calls to registerHandler and the like.

import options, sugar, sequtils

import parser, ast

proc parseBind(self: Parser): Atom =
  result = list(
    ibBind,
    self.advance.id,
  )
  if indented: # Series of binds, 1 per line
    pushRef:
      setDent
      while not dedented:
        setLine
        # We leave it to the interpreter to know that `=`(`()`(`x`, `int`), `1`) is `x: int = 1`
        result.children.add self.parseExpr (NormalExpr - {canComma})
  else: # Series of binds, comma-separated, 1 line
    while not newlined:
      result.children.add self.parseExpr (AssingableExpr - {canComma})
      if not tryMatch iComma: break
registerHandler [iLet, iVar, iField, iCase, iCVar], parseBind


# TODO: Will we just translate `fn` into `$x => {}` directly?
proc parseFn*(self: Parser): Atom =
  result = list(
    ibFunc,
    self.advance.id,
  )
  var args: seq[Atom] = @[]
  withoutBlocking:
    while not nextIs iStoreIn:
      if nextIs iComma:
        match iComma
        continue
      else:
        args.add list(ibBind, iLet.toAtom, self.parseExpr UnblockedExpr)
  result.children.add args
  # In case the `->` was on a different line
  setLine
  setDent
  match iStoreIn
  result.children.add self.parseExpr UnblockedExpr
  match iAssg
  result.add self.parseExpr(NormalExpr + {canImplicitBlock})
registerHandler [iFn, iMacro], parseFn



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
# which may only appear after their masters ("if" or "when") and which denote another arm.
proc parseSimpleControl*(self: Parser, slave = none[StrID]()): Atom =
  result = list(self.cur.id, list(self.advance.id, self.parseArm))
  if slave.isSome:
    while nextIs slave.get:
      result.children.add list(self.advance.id, self.parseArm)
  if nextIs iElse:
    result.children.add list(self.advance.id, self.parseArm)
  if nextIs iFinally:
    result.children.add list(self.advance.id, self.parseArm)
registerHandler [iIf], self => self.parseSimpleControl(some iElse)
registerHandler [iWhile, iFor, iLoop], self => self.parseSimpleControl()
registerHandler [iWhen], self => self.parseSimpleControl(some iIs)

# TODO: Make expect/assert distinct
registerHandler [iExpect, iAssert, iBreak, iReturn, iDelete, iContinue], self => list(
  self.advance.id,
  self.parseExpr NormalExpr
)



registerHandler [iLBrace], proc(self: Parser): Atom =
  match iLBrace
  result = self.parseBlock()
  match iRBrace

# For the next two, note that parseUnary already checks 

registerHandler [iLBracket], proc(self: Parser): Atom = withoutBlocking:
  match iLBracket
  result = list(
    ibArray,
    self.parseTypeDesc(trailing=true),
  )
  result.children.add parseDelimited(iComma, nextIs iRBracket)
  match iRBracket

registerHandler [iLParen], proc(self: Parser): Atom = withoutBlocking:
  match iLParen
  result = list(
    ibTuple,
    self.parseTypeDesc(trailing=true),
  )
  result.children.add parseDelimited(iComma, nextIs iRParen)
  match iRParen
