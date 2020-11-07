# Largely just calls to registerHandler and the like.

import options, sugar

import parser, ast

proc parseBind(self: Parser): Atom = return
registerHandler [iLet, iVar, iField, iCase, iCVar], parseBind

proc parseFn(self: Parser): Atom = return
registerHandler [iFn, iMacro], parseFn

# ':' block_or_expr | expr_noColon [ ':' block_or_expr ]
# into must have `args` accessible
proc parseArm(self: Parser, doBody = true): seq[Atom] = withBlocking:
  if not nextIs iColon:
    result.add self.parseExpr ekNoBlock
    if doBody and not nextIs iColon:
      result.add self.parseBlock
    elif nextIs iColon:
      result.add self.parseExpr ekNoBlock
  elif doBody:
    match iColon
    result.add:
      if indented:
        self.parseBlock
      else:
        self.parseExpr ekFull
  else:
    quit "Expected one of `expr:body`, `:body`, or `expr <block>`"

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
  args: @[self.parseExpr ekFull]
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
    children: self.parseDelimited(delim=iComma, ender=iRBracket)
  )
  match iRBracket

registerHandler [iLParen], proc(self: Parser): Atom = withoutBlocking:
  match iLParen
  result = Atom(
    kind: akCompound,
    compoundKind: ckArray,
    typeSpec: self.parseTypeDesc(trailing=true),
    children: self.parseDelimited(delim=iComma, ender=iRParen)
  )
  match iRParen
