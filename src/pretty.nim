import nodes


var level = 0
template indent(body: untyped):untyped =
  inc level
  body
  dec level

template pecho(args: varargs[string, `$`]) =
  stdout.write '\n'
  for i in 0..<level: stdout.write "  "
  stdout.write args
template becho(body: untyped) =
  stdout.write "("
  indent: body
  pecho ")"


method prettyPrint*(self: Stmt) {.base.} =
  quit "HIT BASE STMT"
method prettyPrint*(self: BindLoc) {.base.} =
  quit "HIT BASE BINDLOC"

method prettyPrint*(self: BindTuple) =
  pecho "Tuple"
  becho:
    for child in self.locs: prettyPrint child
method prettyPrint*(self: BindSym) =
  let pub = if self.isPub: "*" else: ""
  if self.ty != nil:
    pecho self.loc, pub, ":"
    becho:
      prettyPrint self.ty
  else:
    pecho self.loc, pub
method prettyPrint*(self: BindSink) =
  pecho "@_@"

method prettyPrint*(self: Block) =
  pecho "Block", self.label, self.pos
  becho:
    for child in self.children: prettyPrint child
method prettyPrint*(self: Assert) =
  pecho "Assert"
  becho:
    prettyPrint self.expr
method prettyPrint*(self: Break) =
  if self.val != nil:
    pecho "Break ", self.label, ":"
    becho:
      prettyPrint self.val
  else:
    pecho "Break ", self.label
    
method prettyPrint*(self: Return) =
  if self.val != nil:
    pecho "Return"
    becho: prettyPrint self.val
  else: pecho "Return"
method prettyPrint*(self: Bind) =
  pecho self.cmd
  becho:
    prettyPrint self.loc
    if self.init != nil:
      pecho "="
      becho:
        prettyPrint self.init
method prettyPrint*(self: Expr) =
  quit "HIT BASE EXPR"
method prettyPrint*(self: Tuple) =
  pecho "Tuple"
  becho:
    for child in self.children: prettyPrint child
method prettyPrint*(self: Atom) =
  quit "HIT BASE ATOM"
method prettyPrint*(self: Sink) = pecho "@_@"
method prettyPrint*(self: Null) = pecho "null"
method prettyPrint*(self: Undef) = pecho "undef"
method prettyPrint*(self: Symbol) = pecho self.sym
method prettyPrint*(self: Int) = pecho self.val
method prettyPrint*(self: String) = pecho '"', self.val, '"'
method prettyPrint*(self: Swizzle) =
  return
method prettyPrint*(self: Call) =
  return
method prettyPrint*(self: BinExpr) =
  pecho self.cmd, self.pos, "("
  indent:
    prettyPrint self.lhs
    prettyPrint self.rhs
  pecho ")"
method prettyPrint*(self: UnaryExpr) =
  return
method prettyPrint*(self: If) =
  return
method prettyPrint*(self: Loop) =
  return
method prettyPrint*(self: For) =
  return
method prettyPrint*(self: While) =
  return
method prettyPrint*(self: DoWhile) =
  return
method prettyPrint*(self: FnType) =
  return
method prettyPrint*(self: FnDef) =
  return

