import system
import os
import json
import macros

import ast

macro pretty(T, body: untyped): untyped =
  result = newProc(
    name=postfix(ident"jsonify", "*"),
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

template doJsonify(t: ref object): untyped =
  if t != nil: jsonify t
  else: newJNull()
template doJsonify(t: object): untyped = jsonify t

method jsonify*(self: Stmt): JsonNode {.base.} = defaultBase()
method jsonify*(self: BindLoc): JsonNode {.base.} = defaultBase()
method jsonify*(self: AccessOp): JsonNode {.base.} = defaultBase()

proc jsonify*(self: IfArm): JsonNode = %*{
  "cond": jsonify self.cond,
  "capt": doJsonify self.capt,
  "val": jsonify self.val
}
proc jsonify*(self: WhenArm): JsonNode = %*self


# Cannot assume that all things are not nil, so we guard
# USE THIS ONE whenever it may be nil

proc jsonifyAll*[T](things: seq[T]): JsonNode =
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

pretty AccessCall: %*{
  "call": if self.kind == ckCall: "call" else: "index",
  "args": jsonifyAll self.args,
}
pretty AccessValue: %*self.name
pretty AccessSwizzle: jsonifyAll self.paths

pretty String: %*self
pretty Word: %*{
  "word": self.word,
  "path": jsonifyAll self.path,
}
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