import streams, strformat

import ast

import common_tags

type Prettifier* = object
  curDent*: int
  stepDent*: int
  output*: Stream

proc pretty*(p: var Prettifier, a: Atom) =
  template write(a: untyped) =
    p.output.write a
  template indent(p: var Prettifier) =
    p.curDent += p.stepDent
  template dedent(p: var Prettifier) =
    p.curDent -= p.stepDent
  template writeDent(p: Prettifier) =
    write '\n'
    for i in 0..p.curDent:
      write ' '
  case a.kind:
    of akList:
      var isBlock = a[0] == ibBlock
      write '('
      if isBlock: indent p
      for i, c in a.children.pairs:
        if isBlock: p.writeDent
        p.pretty c
        if isBlock or i == a.children.len - 1: discard
        else: write ' '
      if isBlock:
        dedent p
        writeDent p
      write ')'
    of akWord:
      write a.id.lookup
    of nkString:
      write '"'
      write a.str
      write '"'
    of nkVoid:
      write "()"
    of nkUInt:
      write a.uinteger
    of nkTuple:
      write "(@@tuple@@ "
      for i, c in a.innerCtx.items.pairs:
        p.pretty c
        if i < a.innerCtx.items.len - 1: write ' '
      write ')'
    of nkFn:
      write "(@@fn@@ "

      write ')'
    else:
      write fmt"FOUND UNKNOWN KIND {a.kind}"

proc pretty*(a: Atom) =
  var prettifier = Prettifier(
    curDent: 0,
    stepDent: 2,
    output: newFileStream stdout,
  )
  prettifier.pretty a

