const std = @import("std");

const Token = @import("token.zig").Token;

pub const StmtBody = std.ArrayList(Stmt);

pub const Stmt = union(enum) {
  IfStmt: IfStmt,
  WhileStmt: WhileStmt,
  ForStmt: ForStmt,

  BindStmt: BindStmt,
  // Anything else is just a call.
  // Even something like 'i = 1;' or 'i;' get compiled to '`=`(i, 1)' and '`null`(i)', respectively
  Expr: Call, 
};

pub const ID = Token;

pub const IfStmt = struct {
  pub cond: Call,
  pub captID: ?ID,
  pub body: StmtBody,
  pub otherwise: StmtBody,
};

pub const WhileStmt = struct {
  pub cond: Call,
  pub cont: ?*Stmt,
  pub captID: ?ID,
  pub body: StmtBody,
  pub otherwise: StmtBody,
};

pub const ForStmt = struct {
  pub range: Call,
  pub captID: ?ID,
  pub body: StmtBody,
  pub otherwise: StmtBody,
};


pub const BindStmt = struct {
  pub mut: bool, // var = true, let = false
  pub id: Token,
  pub val: *Stmt, // Either undefined (TODO) or 
};


pub const Call = struct {
  // A No-Op (return the args directly) is achieved by a func == null
  pub func: ?[]const u8,
  pub args: *Call,
};

