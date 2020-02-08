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
proc prettyPrint*[T](self: seq[T]) =
  for stmt in self:
    prettyPrint stmt

method prettyPrint*(self: BindTuple) =
  pecho "Tuple"
  becho: prettyPrint self.locs
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
  becho: prettyPrint self.children
method prettyPrint*(self: Assert) =
  pecho "Assert", self.pos
  becho:
    prettyPrint self.expr
method prettyPrint*(self: Break) =
  if self.val != nil:
    pecho "Break ", self.label, self.pos, ":"
    becho:
      prettyPrint self.val
  else:
    pecho "Break ", self.label, self.pos
    
method prettyPrint*(self: Return) =
  pecho "Return", self.pos
  if self.val != nil:
    becho: prettyPrint self.val
method prettyPrint*(self: Bind) =
  pecho self.cmd, self.pos
  becho:
    prettyPrint self.loc
    if self.init != nil:
      pecho "="
      becho:
        prettyPrint self.init
method prettyPrint*(self: Expr) =
  quit "HIT BASE EXPR"
method prettyPrint*(self: Tuple) =
  pecho "Tuple", self.pos
  becho: prettyPrint self.children
method prettyPrint*(self: Atom) =
  quit "HIT BASE ATOM" & $self.pos
method prettyPrint*(self: Sink) = pecho "@_@"
method prettyPrint*(self: Null) = pecho "null"
method prettyPrint*(self: NillTup) = pecho "()"
method prettyPrint*(self: Undef) = pecho "undef"
method prettyPrint*(self: Symbol) = pecho self.sym
method prettyPrint*(self: Int) = pecho self.val
method prettyPrint*(self: String) = pecho '"', self.val, '"'
method prettyPrint*(self: Swizzle) =
  pecho "Swizzle", self.pos
  becho:
    prettyPrint self.subject
    pecho "."
    prettyPrint self.path
method prettyPrint*(self: Call) =
  pecho "Call", self.pos
  becho:
    prettyPrint self.subject
    becho: prettyPrint self.args
method prettyPrint*(self: BinExpr) =
  pecho self.cmd, self.pos
  becho:
    prettyPrint self.lhs
    prettyPrint self.rhs
method prettyPrint*(self: UnaryExpr) =
  pecho self.cmd
  becho: prettyPrint self.target
method prettyPrint*(self: If) =
  pecho "If"
  becho:
    for arm in self.arms:
      pecho "Arm"
      becho:
        prettyPrint arm.cond
        if arm.capture != nil:
          pecho "->"
          prettyPrint arm.capture
        pecho "=>"
        prettyPrint arm.val
    if self.default != nil:
      pecho "Else"
      becho: prettyPrint self.default
    if self.final != nil:
      pecho "Finally"
      becho: prettyPrint self.final
method prettyPrint*(self: Loop) =
  pecho "Loop"
  becho:
    if self.counter != nil:
      pecho "->"
      prettyPrint self.counter
    prettyPrint self.body
method prettyPrint*(self: CondLoop) =
  if self.exprIsRange: pecho "For"
  else: pecho "While"
  becho:
    prettyPrint self.expr
    if self.capture != nil: # If self.counter != nil, then self.capture !== nil
      pecho "->"
      becho:
        prettyPrint self.capture
        if self.counter != nil: prettyPrint self.counter
    prettyPrint self.body
method prettyPrint*(self: Until) =
  pecho "Until"
  becho:
    prettyPrint self.cond
    if self.counter != nil:
      pecho "->"
      becho: prettyPrint self.counter
    prettyPrint self.body
    
method prettyPrint*(self: Fn) =
  pecho "Fn"
  becho:
    prettyPrint self.args
    pecho "->"
    prettyPrint self.ret

