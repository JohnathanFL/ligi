import system, tables, sugar, strformat

import lazy

import lexing, parser, ast, pretty, parse_hooks


# Eval type, eval each child, replace self with final value
proc evalBlock(self: var Atom, context: Context) =
  var ourType = move self[1]
  # Must be replaced after a move to ensure it's not zeroed out.
  self[1] = SinkAtom
  # TODO: This must be comptime
  reduce ourType, context

  var us = Context(
    parent: context,
    values: initTable[StrID, Atom](0)
  )

  # Evaluate each child, then delete it.
  # TODO: If we keep doing things like this, it may make sense to store in
  # opposite (e.g stack) order.
  for c in self.children.mitems:
    reduce c, us
  reduce(self[self.len-1], us)
  self = self[self.len-1]
  # TODO: perform typecheck and conversion to ourType

# Eval type, eval each child, leave self in place.
proc evalTuple(self: var Atom, ctx: Context) =
  var ourType = move self[1]
  self[1] = SinkAtom
  # TODO: This must be comptime
  reduce ourType, ctx

  for c in self.children.mitems:
    reduce c, ctx
  if ourType.kind != akNative or ourType.native.kind != nkSink:
    # TODO
    raise newException(
      ValueError,
      fmt"Tuple conversions not yet implemented!"
    )
  else:
    self = Atom(
      kind: akNative,
      native: Native(
        kind: nkTuple,
        items: move self.children
      )
    )
    # Remove the @tuple and the typedesc
    for _ in 0..1: self.native.items.delete 0

proc evalBind(self: var Atom, ctx: Context) =
  discard
proc evalPrint(self: var Atom, context: Context) =
  while self.len > 1:
    reduce self[1], context
    echo $self[1]
    self.children.delete(1)
  self = VoidAtom

let baseCtx = Context(
  values: toTable {
    ibBlock: procAtom evalBlock,
    "print".toID: procAtom evalPrint,
    iSink: Atom(kind: akNative, native: Native(kind: nkSink)),
    ibTuple: procAtom evalTuple,
  }
)

when isMainModule:
  var exText = readFile "example.li"
  var main = exText.lex.parse
  echo "\nCode:"
  pretty main
  # echo dumpCache()
  echo "\nResult:"
  reduce(main, baseCtx)
  echo "\nLeftover:"
  pretty main
