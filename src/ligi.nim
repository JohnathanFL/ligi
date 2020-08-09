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

method jsonify(self: Stmt): JsonNode {.base.} = echo "UNIMPLEMENTED FOR " & repr(self)
proc jsonify(stmts: seq[Stmt]): JsonNode =
  result = newJArray()
  for s in stmts:
    result.elems.add jsonify s

pretty Word: %*self
pretty Return: %*{
  "return": jsonify self.val,
}
pretty Block: %*{
  "label": self.label,
  "stmts": jsonify self.stmts
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
