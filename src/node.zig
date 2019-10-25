const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const HashMap = std.AutoHashMap;
const Tokens = @import("token.zig");
const Token = Tokens.Token;
const Tag = Tokens.Tag;

pub const Call = struct {
  func: Token,
  args: ArrayList(*Expr),
};

pub const Expr = union(enum) {
  call: Call,
  ifExpr: *IfExpr,
  
  val: Token,  
};

pub const Stmt = union(enum) {
  expr: Expr,
  forStmt: ForStmt,
  whileStmt: WhileStmt,
  bindStmt: BindStmt,
};

// val is what can be returned by the block.
// defaults to void
pub const Block = struct {
  symbols: HashMap([]const u8, *BindExpr),
  stmts: ArrayList(*Stmt),
  // If the block returns nothing, this will just get pointed to a global "nullexpr" thingy
  val: *Expr
};

pub const IfCondBlock = struct {
  cond: *Expr,
  if_so: *Block,
  capture_token: ?*BindStmt,
}

pub const IfExpr = struct {
  // The value of the IfExpr is taken from the value of whichever block is evaluated as true.
  // If used in a spot needing a value, then the else must be present.

  // else gets handled as CondBlock{true, {...}}
  conds: ArrayList(IfCondBlock),
  finally: ?*Block,
};

pub const ForStmt = struct {
  iterable: *Expr,
  count_token: ?*BindStmt,
  capture_token: ?*BindStmt,
  loop: *Block, // Value must be void
  finally: ?*Block,  

  label: ?[]const u8,
};

pub const WhileStmt = struct {
  cond: *Expr,
  count_token: ?*BindStmt,
  capture_token: ?*BindStmt,
  loop: *Block,
  finally: *Block,
};

pub const BindStmt = struct {
  // Debugging values:
  line: usize,
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

// Comparison with this should be on an address basis
pub const auto_type = Type {.symbol = "____auto____"};
pub const Type = union(enum) {
  slice: *Type,
  array: Array,
  opt: *Type,
  // Errors will all be dynamicly decided at compile time, so no need for an explicit error type.
  err: *Type,
  symbol: []const u8,
  
};

pub const Lambda = struct {
  is_pure: bool,
  // The function's block's symbol table will have the same *BindStmts
  args: ArrayList(*BindStmt),
  ret: *Type,
};
