import tables
import cmds


# Using pure refs instead of Options for slightly cleaner code (maybe)
# I'll try to annotate nillable with #?[, condition for nil]
# (And yeah, it is somewhat ironic to use this style while building a language without implicit nils)
type
  Stmt* = ref object of RootObj
    pos*: tuple[line: uint, col: uint]
  Assert* = ref object of Stmt
    expr*: Expr
  Break* = ref object of Stmt
    val*: Expr #?
    # label.len == 0 just means the next block up
    label*: string #?
  Return* = ref object of Stmt
    val*: Expr #?

  BindLoc* = ref object of RootObj
  BindTuple* = ref object of BindLoc
    locs*: seq[BindLoc]
  BindSym* = ref object of BindLoc
    loc*: string
    ty*: Expr
    isPub*: bool
  # We want to just discard any writes to this.
  # Has to be its own thing rather than Symbol(_) to avoid constant
  # 'is sym == _' checks
  # Note this technically means `let _ = 10` is just as valid as `_ = 10`
  BindSink* = ref object of BindLoc
  Bind* = ref object of Stmt
    cmd*: BindCmd
    # We know the bind type from Stmt.cmd
    loc*: BindLoc
    init*: Expr #?

  # Anything that can yield a value
  Expr* = ref object of Stmt
  Tuple* = ref object of Expr
    children*: seq[Expr]
  Atom* = ref object of Expr
  Sink* = ref object of Atom
  Null* = ref object of Atom
  Undef* = ref object of Atom
  Symbol* = ref object of Atom
    sym*: string
  Int*  = ref object of Atom
    val*: uint
  String* = ref object of Atom
    val*: string

  # Can yield a value from either a break or an unconsumed value
  Block* = ref object of Expr
    children*: seq[Stmt]
    # label.len == 0 just means unlabeled
    label*: string 

  # The atoms
  

  Swizzle* = ref object of Expr
    subject*: Expr
    path*: Expr

  Call* = ref object of Expr
    isIndex*: bool
    subject*: Expr
    args*: seq[Expr]

  # Note that this also includes field access.
  # This is because an access could include calls and such
  BinExpr* = ref object of Expr
    cmd*: BinCmd
    lhs*: Expr
    rhs*: Expr
  UnaryExpr* = ref object of Expr
    cmd*: UnaryCmd
    target*: Expr

  # TODO: When matches or whatever are added, why not just parse them directly into these?
  # They're the exact same idea anyway
  IfArm* = object
    cond*: Expr
    capture*: Bind
    val*: Block
  If* = ref object of Expr
    arms*: seq[IfArm]
    default*: Block #?
    final*: Block #?

  Loop* = ref object of Expr
    body*: Block
    counter*: Bind #?
  For* = ref object of Loop
    range*: Expr
    capture*: Bind #?
  While* = ref object of Loop
    cond*: Expr
    capture*: Bind #?
  DoWhile* = ref object of Loop
    cond*: Expr
    # Can't capture since the first run tests nothing.


  # Technically
  # `fn a, b: usize -> res:usize {...}`
  # is actually
  # `FnType Block`

  # This also means that when declaring a function type, you *must* give names to all (though they aren't checked)
  # Essentially, this is forcible documentation.
  # Compile errors will continue until documentation improves.
  FnType* = ref object of Expr
    args*: seq[Bind]
    ret*: Bind #?
  FnDef* = ref object of Expr
    ty*: FnType
    body*: Block

proc add*(self: Block, s: Stmt) =
  self.children.add s
