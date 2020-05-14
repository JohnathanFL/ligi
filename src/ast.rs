// Some conventions:
// - Only Box in the Expr enum, if possible.
//   - Obvious exception for Vecs
// - Everything's an expression. assert/etc will just be expressions with no value
// - use `default` to mean `else` to get around Rust's identifiers
// - If possible, Box together (i.e Box<(val1, val2)>)

pub type OptBox<T> = Option<Box<T>>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AssgOp {
  Assg, AddAssg, SubAssg, MulAssg, DivAssg, BitOrAssg, BitAndAssg, ShlAssg, ShrAssg,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinOp {
  Equal, NotEqual,
  Less, Greater, GreaterEq, LessEq, Spaceship,
  Or, Xor,
  And,
  In, NotIn,
  OpenRange, ClosedRange,
  Add, Sub,
  Mul, Div, Mod,
  BitOr, BitAnd, BitXor,
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaOp {
  Neg, BitNot, Not,
  Const, Comptime,
  Array, Slice, Opt,
  Pure, Inline, Struct, Enum, Overload, Property,
  Ptr
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BindSpec {
  Let, Var, CVar, Field, Enum
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Publicity {
  Private, ReadOnly, Public
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Option<String>, pub Vec<Expr>);

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
  Binding(Box<Bind>),
  Assert(Box<Expr>),
  Break(Option<String>, OptBox<Expr>),
  Return(OptBox<Expr>),
  Defer(Box<Expr>),
  
  Block(Block),
  
  AssgExpr(AssgOp, Box<(Expr, Expr)>),
  BinExpr(BinOp, Box<(Expr, Expr)>),
  UnaExpr(UnaOp, Box<Expr>),
  /// Includes solitary words.
  Access(Vec<AccessCmd>),

  If(Box<If>),
  When(Box<When>),

  Loop(OptBox<BindLoc>, Block),
  PreTestLoop(Box<PreTestLoop>),
  PostTestLoop(bool, Box<Expr>, OptBox<BindLoc>, Block),
  For(Box<For>),
}

/// Holds anything that can be "accessed" or called.
/// i.e: variables/tuples/`foo.bar`/etc
/// I intend for these to be a simple sequential parsing.
/// For example, `foo.bar(baz)` becomes
/// {Name(foo), Descend, Name(bar), Call({baz})}
/// which is read as the sequence:
///   Select foo from the current scope
///   Descend into the currently selected value's (foo's) scope
///   Select bar from the current scope
///   Call the currently selected value with these args
/// I hope that this new setup can ease some of the wackiness associated with the way
/// ligi handles swizzling.
#[derive(Debug, PartialEq, Clone)]
pub enum AccessCmd {
  /// `.` i.e "descend into the previous access"
  Descend,
  /// `(...)` i.e "form a tuple using these"
  Tuple(Option<Expr>, Vec<AccessCmd>),
  /// Access based on the direct name
  /// Includes numbers.
  Name(String),
  Array(Option<Expr>, Vec<Expr>),
  Struct(Option<Expr>, Vec<Expr>),
  
  /// Call the previous function with these args
  Call(Vec<Expr>),
  /// "Pass the lefthand value to the righthand function"
  /// In other words, it "resets" the scope. Thus, in
  ///   foo.bar::baz(car).zar
  /// `baz` is *not* a member of `bar`'s scope
  Pass,
  /// Index into the previous with these args
  Index(Vec<Expr>),
  
}

#[derive(Debug, PartialEq, Clone)]
pub struct Bind {
  pub spec: BindSpec,
  pub loc: BindLoc,
  pub init: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindLoc {
  Sink(Option<Expr>),
  Name(String, Option<Expr>),
  Tuple(Vec<BindLoc>, Option<Expr>)
}


#[derive(Debug, PartialEq, Clone)]
pub struct IfArm {
  pub cond: Expr,
  pub capt: Option<Bind>,
  pub val: Block
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
  pub arms: Vec<IfArm>,
  pub default: Option<Block>,
  pub finally: Option<Block>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhenArm {
  pub op: Option<BinOp>,
  pub expr: Expr,
  pub capt: Option<Bind>,
  pub val: Block
}

#[derive(Debug, PartialEq, Clone)]
pub struct When {
  pub expr: Expr,
  pub arms: Vec<WhenArm>,
  pub default: Option<Block>,
  pub finally: Option<Block>
}

#[derive(Debug, PartialEq, Clone)]
pub struct PreTestLoop {
  pub target: bool, // i.e `is while?`. The value we need to get to continue.
  pub expr: Expr,
  pub capt: Option<(BindLoc, Option<BindLoc>)>,
  pub val: Block,
  pub finally: Option<Block>
}
#[derive(Debug, PartialEq, Clone)]
pub struct PostTestLoop {
  // TODO
}
#[derive(Debug, PartialEq, Clone)]
pub struct For {
  pub range: Expr,
  pub capt: Option<(BindLoc, Option<BindLoc>)>,
  pub val: Block,
  pub finally: Option<Block>,
}
