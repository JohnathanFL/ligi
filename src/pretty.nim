import streams

import ast

type Prettifier = object
  curDent: int
  stepDent: int
  output: StringStream

template write(a: untyped) =
  p.output.write a

proc indent(p: var Prettifier) =
  p.curDent += p.stepDent
proc dedent(p: var Prettifier) =
  p.curDent -= p.stepDent
proc writeDent(p: Prettifier) =
  write '\n'
  for i in 0..p.curDent:
    write ' '

proc pretty(p: var Prettifier, a: Atom) =
  case a.kind:
    of akList:
      p.writeDent
      write '('
      p.indent
      p.writeDent
      for c in a.children:
        p.pretty c
      p.dedent
      p.writeDent
      write ')'
    of akWord:
      write a.id.lookup
    of akStr:
      write '"'
      write a.str
      write '"'
    else: discard

proc pretty*(a: Atom): string =
  var prettifier = Prettifier(curDent: 0, stepDent: 2, output: newStringStream "")
  prettifier.pretty a
  return prettifier.output.readAll

