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

# spec is in [1] as an akWord. For each subsequent atom:
# - That is an `(= lhs rhs)`, create the binding described in lhs and set it to the reduction of rhs
# - That is anything else, assume the same format as `lhs` from above and create it
# - When evaluating a lhs, assume the form `(name type)` (i.e function applications of `name(type)`, `name:type`, etc)
proc evalBind(self: var Atom, ctx: Context) =
  # TODO: Respect bind specs and mutability rules
  let spec = self[1].id # `let`, `var`, etc
  # TODO: Implement another table (in context?) to store bind specs beyond let/var/cvar
  for i, child in self.children.mpairs:
    if i in 0..1: continue
    # TODO: A full AST pattern matching library.
    # For now, assume one of the forms `(= bind rhs)` or `bind`, where bind
    # is of the form `name` or `(name type)`
    var
      name: Atom
      ty: Atom
      val: Atom

    if child.kind == akList and child[0] == iAssg: # `(= bind value)`
      name = move child[1]
      val = move child[2]
    else: # `bind`
      name = move child
      val = SinkAtom # Not yet specified (distinct from undef)

    if name.kind == akList: # `(name type)`
      ty = move name[1]
      name = move name[0]
    else:
      ty = SinkAtom # Not yet specified

    # Always starts as SinkAtom
    var ctxVal: ptr Atom = ctx.bindName(name.id, ty).addr
    # Note that ctx contains the binding before we evaluate val,
    # allowing val to be a block which has assignments to name, as in
    # `let x = { let y = 1; x = y + 1 }`
    reduce val, ctx
    # TODO: If ctxVal is no longer SinkAtom and val is now VoidAtom, consider it initialized.
    #       Otherwise, replace ctxVal with val
    ctxVal[] = move val
  self = VoidAtom # Binds return nothing
  # echo "After bind, ctx is", ctx.values

proc evalPrint(self: var Atom, context: Context) =
  for i, child in self.children.mpairs:
    if i in 0..0: continue
    reduce child, context
    echo child
  self = VoidAtom

proc evalAdd(self: var Atom, context: Context) =
  if self.len notin 2..3: # Either unary `+a` or binary `a + b`
    raise newException(ValueError, "`+` works only on one or two values.")
  # TODO: Type conversions. For now, assume binary with strings
  let resStr = self[1].native.str & self[2].native.str
  self = Atom(kind: akNative, native: Native(kind: nkString, str: resStr))


# Essentially, each mode (interpreter, comptime interpreter, codegen) will have a different base context
# that provides the appropriate implementations. That's the idea, anyway.
let interpreterCtx = Context(
  values: toTable {
    ibBlock: procAtom evalBlock,
    "print".toID: procAtom evalPrint,
    iSink: Atom(kind: akNative, native: Native(kind: nkSink)),
    ibTuple: procAtom evalTuple,
    ibBind: procAtom evalBind,
    iAdd: procAtom evalAdd,
  }
)

when isMainModule:
  while true:
    var input = stdin.readAll()
    var main = input.lex.parse
    # echo "\nCode:"
    # pretty main
    echo dumpCache()
    stdout.write ">>>"
    reduce(main, interpreterCtx)
    # echo "\nLeftover:"
    # pretty main
