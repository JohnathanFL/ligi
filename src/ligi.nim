import system, tables, sugar, strformat

import lexing, parser, ast, pretty, parse_hooks


proc evalBlock(self: var Atom, context: Context) =
  reduce self[1], context # Expected type

  var us = Context(
    parent: context,
    values: initTable[StrID, Atom](0)
  )

  # Evaluate each child, then delete it.
  # TODO: If we keep doing things like this, it may make sense to store in
  # opposite (e.g stack) order.
  while self.len > 3:
    reduce(self[2], us)
    self.children.delete(2)
  reduce(self[2], us)
  self = self[2]

proc evalPrint(self: var Atom, context: Context) =
  while self.len > 1:
    reduce self[1], context
    echo $self[1]
    self.children.delete(1)
  self = VoidAtom

proc evalSink(self: var Atom, context: Context) =
  self = Atom(kind: akNative, native: Native(kind: nkSink))

let baseCtx = Context(
  values: toTable {
    ibBlock: procAtom evalBlock,
    "print".toID: procAtom evalPrint,
    iSink: procAtom evalSink
  }
)

when isMainModule:
  var exText = readFile "example.li"
  # let p = exText.lex.parse
  # echo pretty jsonifyAll p.parseStmtSeq
  var main = exText.lex.parse
  reduce(main, baseCtx)
  pretty main
