import strutils
import parseutils
import re
import tokens
import nodes
# const
#   UIntRegex = re(r"^u\d{1,3}$", {})
#   IntRegex = re(r"^i\d{1,3}$", {})

# Eval takes care of flattening comptime stuff, including types
method eval*(s: Stmt) {.base.} =
  echo "FELL BACK TO STMT FOR ", repr(s)
method eval*(e: Expr) =
  echo "FELL BACK TO EXPR"
method eval*(a: Atom) =
  if a.tok.what.tag in literals:
    a.isComptimeKnown = true
    case a.tok.what.tag:
      of IntLit: a.typeOf = TypeId(class: USize)
      of StringLit: a.typeOf = TypeId(class: Str)
      # Can't know the concrete type of a null yet
      of NullLit: a.typeOf = TypeId(class: Any)
      of CharLit: a.typeOf = TypeId(class: Char)
      of Symbol:
        # Recognize builtins
        let lex = a.tok.what.lexeme
        if lex[0] in {'u', 'i'} and lex.len in {2..4}: # May be a u{} or i{}
          let num = lex[1..].parseInt
          let class = if lex[0] == 'u': UInt else: Int
          if num != 0:
            a.typeOf = TypeId(class: class,  numBits: num)
        else:
          # TODO: Look up the symbol
      else: discard
method eval*(a: Assert) =
  # TODO: If both sides are comptime known, then completely eval the assertion
  a.expr.eval()
method eval*(r: Return) =
  # Nothing to be done here
  if r.val.isSome:
    r.val.get.eval()
method eval*(b: Break) =
  if b.label.isSome:
    prettyEcho "Breaking from ", b.label.get.what.lexeme
  else:
    prettyEcho "Breaking"
  if b.val.isSome:
    b.val.get.eval()
method eval*(b: Block) =
  for child in b.children:
    child.eval()
  # The one that called this will take care of converting us based on interpret
method eval*(b: BindLoc) {.base.} =
  quit "FELL BACK TO BINDLOC"
method eval*(b: BindSym) =
  if b.ty.isSome:
    b.ty.get.eval
    
  prettyEcho msg
  if b.ty.isSome:
    b.ty.get.eval()
method eval*(b: BindTup) =
  prettyEcho "BindTup"
  for binding in b.children:
    binding.eval()
method eval*(b: Bind) =
  prettyEcho $b.interpret
  indent:
    prettyEcho "Loc:"
    b.loc.eval()
    if b.default.isSome:
      prettyEcho "Value:"
      b.default.get.eval()
method eval*(e: EnumLit) =
  prettyEcho "EnumLit: #", e.tok.what.lexeme
  if e.val.isSome: e.val.get.eval()
method eval*(l: Loop) =
  prettyEcho "Loop"
  indent:
    if l.counter.isSome:
      prettyEcho "Counter:"
      l.counter.get.eval()
    prettyEcho "Body:"
    l.body.eval()
method eval*(l: CondLoop) =
  prettyEcho $l.interpret
  indent:
    if l.counter.isSome:
      prettyEcho "Counter:"
      l.counter.get.eval()
    prettyEcho "Body:"
    l.body.eval()

proc eval*(arm: IfArm) =
  prettyEcho "Arm:"
  indent:
    if arm.capture.isSome:
      prettyEcho "Capture:"
      arm.capture.get.eval()
    prettyEcho "Cond:"
    arm.cond.eval()
    prettyEcho "Then:"
    arm.val.eval()
method eval*(i: If) =
  prettyEcho "If"
  indent:
    prettyEcho "Arms:"
    for arm in i.arms:
      arm.eval()

    if i.default.isSome:
      prettyEcho "Else:"
      i.default.get.eval()
    if i.final.isSome:
      prettyEcho "Finally:"
      i.final.get.eval() 


method eval*(t: Tuple) =
  prettyEcho "Tuple ("
  for child in t.children:
    child.eval()
  prettyEcho ")"
method eval*(c: Call) =
  prettyEcho "Calling ", $c.fn
  for arg in c.args:
    arg.eval()
method eval*(p: Path) {.base.} =
  echo "HIT BASE PATH"
method eval*(p: AccessPath) =
  # Since only symbols and Int/String lits can be in a path, this works
  stdout.write p.name.what.lexeme
  if p.next.isSome:
    stdout.write '.'
    p.next.get.eval()
method eval*(p: SwizzlePath) =
  stdout.write '('
  for path in p.paths:
    path.eval()
  stdout.write ')'
  if p.next.isSome:
    stdout.write '.'
    p.next.get.eval()
method eval*(a: Access) =
  prettyEcho "Access"
  indent:
    prettyEcho "From:"
    a.accessed.eval()
    prettyEcho "Take:"
    indent:
      # None of the AccessPaths will do a newline
      for i in 0..prettyIndent: stdout.write "  "
      a.path.eval()
      stdout.write '\n'
method eval*(c: CompoundLiteral) =
  echo "FALLING BACK TO COMPOUNDLIT"
method eval*(s: StructLiteral) =
  prettyEcho "StructLiteral"
  indent:
    if s.ty.isSome:
      prettyEcho "Type:"
      s.ty.get.eval()
    prettyEcho "Fields:"
    for (name, val) in s.fields.pairs:
      indent:
        prettyEcho ".", name, " = "
        val.eval()
method eval*(s: ArrayLiteral) =
  prettyEcho "ArrayLiteral"
  indent:
    if s.ty.isSome:
      prettyEcho "Type:"
      s.ty.get.eval()
    prettyEcho "Members:"
    for val in s.children:
      val.eval()
method eval*(f: Fn) =
  prettyEcho "Fn"
  indent:
    if f.args.len > 0:
      prettyEcho "Args:"
      for arg in f.args:
        arg.eval()
    if f.ret.isSome:
      prettyEcho "Return:"
      f.ret.get.eval()
    prettyEcho "Do:"
    f.body.eval()
