# Largely just calls to registerHandler and the like.

import options, sugar, sequtils

import parser, ast

proc parseBind(self: Parser): Atom =
  result = list(
    list(
      ibBind,
      self.advance.id,
    )
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
      result.children.add self.parseExpr (AssignableExpr - {canComma})
      if not tryMatch iComma: break
registerHandler [iLet, iVar, iField, iCase, iCVar], parseBind


# TODO: Will we just translate `fn` into `$x => {}` directly?
proc parseFn*(self: Parser): Atom =
  result = list(
    ibFunc,
    self.advance.id,
    list(ibBind, iLet), # Args
    list(ibBind, iVar), # Body/result loc
  )
  withoutBlocking:
    while not nextIs iStoreIn:
      if nextIs iComma:
        match iComma
        continue
      else:
        result[2].add self.parseExpr(UnblockedExpr)
  # In case the `->` was on a different line
  setLine
  setDent
  match iStoreIn
  result[3].add self.parseExpr(AssignableExpr + {canImplicitBlock})
registerHandler [iFn, iMacro], parseFn


# ':' block_or_expr | expr_noColon [ ':' block_or_expr ] | expr block
# into must have `args` accessible
# `doBody` controls whether there can be a colon and body
# If there is a body, it may be an implicit block
proc parseArm(self: Parser): Atom = withBlocking:
  result = list(ibArm)
  if not nextIs iColon:
    result.add self.parseExpr(UnblockedExpr - {canColon})
  match iColon
  result.add self.parseExpr(MaybeBlockExpr)

# word arm { slave arm } [ "else" arm ] [ "finally" arm ]
# "Slave" is the generic term for control words like "elif" or "is"
# which may only appear after their masters ("if" or "when") and which denote another arm.
proc parseSimpleControl*(self: Parser, ty: StrID, slave = none[StrID]()): Atom =
  discard self.advance # Skip the control's word
  result = list(ty)
  while true:
    result.children.add list(ibArm, self.parseArm)
    if slave.isSome and tryMatch slave.get: continue
    else: break
  if tryMatch iElse:
    result.children.add list(ibElse, self.parseArm)
  if tryMatch iFinally:
    result.children.add list(ibFinally, self.parseArm)
registerHandler [iIf], self => self.parseSimpleControl(ibIf, some iElIf)
registerHandler [iWhile], self => self.parseSimpleControl(ibWhile)
registerHandler [iFor], self => self.parseSimpleControl(ibFor)
registerHandler [iLoop], self => self.parseSimpleControl(ibLoop)

proc parseWhen*(self: Parser): Atom =
  result = list(ibWhen)
  match iWhen
registerHandler [iWhen], parseWhen

# TODO: Make expect/assert distinct
registerHandler [iExpect, iAssert, iBreak, iReturn, iDelete, iContinue], self => list(
  ("@" & self.advance.id.lookup).toID,
  self.parseExpr NormalExpr,
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
    self.parseTypeDesc(trailing = true),
  )
  result.children.add parseDelimited(iComma, nextIs iRBracket)
  match iRBracket

registerHandler [iLParen], proc(self: Parser): Atom = withoutBlocking:
  match iLParen
  result = list(
    ibTuple,
    self.parseTypeDesc(trailing = true),
  )
  result.children.add parseDelimited(iComma, nextIs iRParen)
  match iRParen
