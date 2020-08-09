import system
import os
import json
import macros

import compiler/[lexing, parser, ast]

macro pretty(T, body: untyped): untyped =
  result = newProc(
    name=ident "jsonify",
    params=[
      ident "JsonNode",
      newIdentDefs(ident "self", T),
    ],
    body=newStmtList(
      body,
    ),
    procType=nnkMethodDef,
  )

template defaultBase(): untyped =
  echo "UNIMPLEMENTED FOR " & $typeof(self) & " " & repr(self)
  return newJNull()
template defaultOr(t: untyped, d: untyped): untyped =
  if t != nil: t
  else: d

template doJsonify(t: ref object): untyped =
  if t != nil: jsonify t
  else: newJNull()
template doJsonify(t: object): untyped = jsonify t

method jsonify(self: Stmt): JsonNode {.base.} = defaultBase()
method jsonify(self: BindLoc): JsonNode {.base.} = defaultBase()
proc jsonify(self: IfArm): JsonNode = %*{
  "cond": jsonify self.cond,
  "capt": doJsonify self.capt,
  "val": jsonify self.val
}
proc jsonify(self: WhenArm): JsonNode = %*self


# Cannot assume that all things are not nil, so we guard
# USE THIS ONE whenever it may be nil

proc jsonifyAll[T](things: seq[T]): JsonNode =
  result = newJArray()
  for s in things:
    # We can assume none of these are null
    result.elems.add jsonify s

pretty BindName: %*{
  "name": %*self.name,
  "ty": doJsonify self.ty,
}
pretty BindTuple: %*{
  "locs": jsonifyAll self.locs,
  "ty": doJsonify self.ty,
}

pretty Word: %*self
pretty Return: %*{
  "return": doJsonify self.val,
}
pretty Block: %*{
  "label": self.label,
  "stmts": jsonifyAll self.stmts
}
pretty If: %*{
  "control": "if",
  "arms": jsonifyAll self.arms,
  "elseCapt": doJsonify self.defCapt,
  "else": doJsonify self.default,
  "finallyCapt": doJsonify self.finCapt,
  "finally": doJsonify self.final
}
pretty Binary: %*{
  "op": %*self.op,
  "lhs": jsonify self.lhs,
  "rhs": jsonify self.rhs,
}
pretty Unary: %*{
  "op": %*self.op,
  "val": jsonify self.val,
}

when isMainModule:
  var exText = readFile "example.li"
  let o = exText.lex.parse
  for i in o:
    echo pretty jsonify i
  # var lexer = exText.lex
  # var (pos, tok)= lexer.scan()
  # while tok.tag != tEOF:
  #   echo tok
  #   (pos, tok) = lexer.scan()
