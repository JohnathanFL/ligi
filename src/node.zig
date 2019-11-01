const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Tokens = @import("token.zig");
const Token = Tokens.Token;
const Tag = Tokens.Tag;

// Note: In the case of operators, the args are infinite.
// so "a or b or c" is Call { .func = "or", .args = {a, b, c}}
// Order of evaluation will depend on the operator, so, e.g, += works
// from right to left (eval [0]+=[1], ([0]+=[1])+=[2], ...)
// and + works as (eval [0]+[1], ([0]+[1])+[2],...)
// Note on the note: Even stuff like a.b or a[] gets parsed to a call
pub const Call = struct {
  func: []const u8,
  args: ArrayList(*Expr),
};

pub const Case = struct {
  cond: *Expr,
  val: *Expr,
};

pub const CaseExpr = struct {
  arms: ArrayList(Case),
  // it MUST have an else
  default: *Expr,
};

// Should be compared by address.
pub const void_expr: Expr = undefined;
pub const Expr = union(enum) {
  call: Call,
  caseExpr: CaseExpr,  
  val: Value,
};

pub const Value = union(enum) {
  symbol: []const u8,
  intLit: i128,
  boolLit: bool,
  nullLit: void,
  stringLit: []const u8,
  charLit: []const u8,
};

pub const Stmt = union(enum) {
  expr: Expr,
  loopStmt: LoopStmt,
  ifStmt: IfStmt,
  bindStmt: BindStmt,
  block: Block
};

// val is what can be returned by the block.
// defaults to void
pub const Block = struct {
  parent: ?*Block = null,
  label: ?[]const u8,
  symbols: StringHashMap(*BindStmt),
  stmts: ArrayList(*Stmt),

  pub fn init(alloc: *Allocator) *Block {
    var res = alloc.create(Block) catch unreachable;
    res.label = null;
    res.stmts = ArrayList(*Stmt).init(alloc);
    res.symbols = StringHashMap(*BindStmt).init(alloc);
    return res;
  }
};

pub const IfCondBlock = struct {
  cond: *Expr,
  if_so: *Block,
  capture_token: ?*BindStmt,
};

pub const IfStmt = struct {
  // else gets handled as CondBlock{true, {...}}
  conds: ArrayList(IfCondBlock),
 
  default: ?*Block, 
  // If present, is evaluated no matter what branch is taken (so long as a branch was taken)
  // Will probably make it an error to have an else and a finally at the same time
  finally: ?*Block,

  pub fn init(alloc: *Allocator) *Stmt {
    var res = try alloc.create(Stmt);
    res.* = Stmt{.ifStmt = try alloc.create(IfStmt)};
    res.ifStmt.* = IfStmt {
      .conds = ArrayList(IfCondBlock).init(alloc),
      .finally = null,
    };
    return res;
  }
};

pub const LoopStmt = struct {
  variant: enum {For, While},
  eval: *Expr,
  counter: ?*BindStmt,
  capture: ?*BindStmt,
  after: ?*Expr, // Only valid on while expressions

  each: *Block,
  finally: ?*Block,
};

pub const BindStmt = struct {
  // Debugging values:
  line: usize,
  col: usize,
  file_id: usize,  

  is_var: bool,
  binding: []const u8,
  // If it is to be inferred, then there will be a special global "auto" type.
  // "auto" would only be valid in 
  typeof: *Type = &auto_type,
  intitial: ?*Expr = null,
};
pub const Array = struct {
  size: usize,
  typeof: *Type
};


pub const Field = struct {
  is_pub: bool,
  name: []const u8,
  typeof: *Type,
  default: ?*Expr,
};

pub const StructDef = struct {
  fields: ArrayList(Field),
  bindings: ArrayList(*BindStmt),
};

pub const EnumTag = struct {
  val: i64,
  name: []const u8,
};

pub const EnumDef = struct {
  backer: []const u8, // Must be a [ui]\d+
  tags: ArrayList(EnumTag),
};

pub const TypeMod = union(enum) {
  slice: void,
  array: usize,
  opt: void,
  err: void,
};

pub const BaseType = struct {
  mods: ArrayList(TypeMod),
  base: []const u8,
};

pub const Type = union(enum) {
  structDef: StructDef,
  enumDef: EnumDef,
  unionDef: void,
  conceptDef: void,
  typeDef: BaseType,
};

pub const Lambda = struct {
  is_pure: bool,
  // The function's block's symbol table will have the same *BindStmts
  args: ArrayList(*BindStmt),
  ret: *Type,
  body: *Block,
};
