import streams

import ast

type Prettifier = object
  curDent: int
  stepDent: int

template write(a: untyped) =
  stdout.write a

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
    of akNative:
      case a.native.kind:
        of nkString:
          write '"'
          write a.native.str
          write '"'
        else: discard
    else: discard

proc pretty*(a: Atom) =
  var prettifier = Prettifier(
    curDent: 0,
    stepDent: 2,
  )
  prettifier.pretty a

