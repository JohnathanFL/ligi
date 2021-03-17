import system, tables, sugar, strformat

import lazy

import lexing, parser, ast, pretty, parse_hooks, common_tags


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
  if ourType.kind != nkSink:
    # TODO
    raise newException(
      ValueError,
      fmt"Tuple conversions not yet implemented!"
    )
  else:
    self = Atom(
      kind: nkTuple,
      innerCtx: Context(
        items: move self.children
      )
    )
    # Remove the @tuple and the typedesc
    for _ in 0..1: self.innerCtx.items.delete 0

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

proc evalAdd*(self: var Atom, context: Context) =
  if self.len notin 2..3: # Either unary `+a` or binary `a + b`
    raise newException(ValueError, "`+` works only on one or two values.")
  # TODO: Type checks/conversions. For now, assume binary with strings
  for child in self.children.mitems:
    reduce child, context
  echo "Adding ", self[1], " and ", self[2]
  let resStr = self[1].str & self[2].str
  self = Atom(kind: nkString, str: resStr)

# @complog(msg)
proc evalCompileLog*(self: var Atom, context: Context) =
  for child in self.children.mitems: reduce child, context
  # Must be a unary op/function call of arity 1
  if self.len != 2 or self[1].kind != nkString:
    raise newException(ValueError, "@complog only takes one string input!")
  echo self[1].str
  self = VoidAtom

# @complog(msg)
proc evalCompileError*(self: var Atom, context: Context) =
  for child in self.children.mitems: reduce child, context
  # Must be a unary op/function call of arity 1
  if self.len != 2 or self[1].kind != nkString:
    raise newException(ValueError, "@complog only takes one string input!")
  quit "ERROR: " & self[1].str
  self = VoidAtom

# @as(item, type)
proc evalAs*(self: var Atom, context: Context) =
  if self.len != 3:
    raise newException(ValueError, "@as takes two arguments (item, type)")

# $(loc:type) or $loc (or $(_:type))
proc evalDollar*(self: var Atom, context: Context) =
  if self.len != 1:
    raise newException(ValueError, "$ takes only one argument")

proc evalIf*(self: var Atom, context: Context) =
  # Structure of an if is (@if (@arm cond body)+ else finally).
  # cond, else, and finally may be void if not present.
  # Thus, we have a minimum length of 4 (since there's at least one arm)
  # Thus, [1..<^2] is also always the list of arms
  var res: Atom
  var foundTrue = false
  for i in 1..<(self.len - 2):
    reduce self[i][1], context # Check the condition
    if self[i][1].kind != nkBool:
      raise newException(ValueError, "Expected a bool for the if arm's condition!")
    if self[i][1].boolean:
      reduce self[i][2], context
      res = self[i][2]
      foundTrue = true
      break
  if foundTrue: # finally
    reduce self[self.len - 1], context
  else: # else
    reduce self[self.len - 2], context
    res = self[self.len - 1]

proc evalEval*(self: var Atom, context: Context) =
  reduce self[1], context
  var main = self[1].str.lex.parse
  echo "Code:"
  pretty main
  echo ""
  # echo dumpCache()
  reduce(main, context)
  echo ""
  self = main

proc evalEmbedFile*(self: var Atom, context: Context) =
  reduce self[1], context
  self = Atom(
    kind: nkString,
    str: open(self[1].str).readAll()
  )

# make sure all words become (word _) and (x y) are left alone
proc ensureLocTypeForm*(self: var Atom) =
  if self.kind == akWord:
    self = list(self, SinkAtom)
  elif self.kind == akList: discard
  else:
    raise newException(ValueError, "Expected `loc` or `(loc type)` form")

proc evalFn*(self: var Atom, context: Context) =
  # Structure is (@fn kind args ret)
  # where `kind` is either `#fn` or `#macro` and args/ret are let and var binds, respectively
  var res = Atom(kind: nkFn, fnCtx: Context(parent: context))
  # Need to take all the args and put them into fnArgs
  for i in 2..<self[2].children.len:
    ensureLocTypeForm self[2][i]
    reduce self[2][i][1], context # All args are in (name type) form
    res.fnCtx.values[self[2][i][0].id] = self[2][i][1]
  let ret = self[3][2]
  if ret.kind != akList or ret[0] != iAssg or ret.len != 3:
    raise newException(ValueError, "Must assign to the function's result location!")
  # ret is (= loc body) or (= (loc type) body)
  let retID = if ret[1].kind == akList: ret[1][0].id else: ret[1].id
  res.ret = retID
  res.fnCtx.values[retID] = ret
  self = res

# Essentially, each mode (interpreter, comptime interpreter, codegen) will have a different base context
# that provides the appropriate implementations. That's the idea, anyway.
let comptimeEvalCtx* = Context(
  values: toTable {
    ibBlock: procAtom evalBlock,
    iSink: SinkAtom,
    ibTuple: procAtom evalTuple,
    ibBind: procAtom evalBind,
    iAdd: procAtom evalAdd,
    ibCompileLog: procAtom evalCompileLog,
    ibCompileError: procAtom evalCompileError,
    ibIf: procAtom evalIf,
    iTrue: Atom(kind: nkBool, boolean: true),
    iFalse: Atom(kind: nkBool, boolean: false),
    ibEval: procAtom evalEval,
    ibEmbedFile: procAtom evalEmbedFile,
    ibFunc: procAtom evalFn,
  }
)
