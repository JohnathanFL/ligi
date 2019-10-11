pub const StmtBody = std.ArrayList(*Stmt);

pub const Stmt = union(enum) {
  IfStmt: IfStmt,
  WhileStmt: WhileStmt,
  ForStmt: ForStmt,
  Expr: Call, // If it's just 
};

pub const ID = []const u8;

pub const IfStmt = struct {
  pub cond: *Expr,
  pub captID: ?ID,
  pub body: StmtBody,
  pub otherwise: *Expr,
};

pub const WhileStmt = struct {
  pub cond: *Expr,
  pub cont: ?*Stmt,
  pub captID: ?ID,
  pub body: StmtBody,
  pub otherwise: *Expr,
};

pub const ForStmt = struct {
  pub range: *Expr,
  pub captID: ?ID,
  pub body: StmtBody,
};


pub const Call = struct {
  // A No-Op (return the args directly) is achieved by a func == null
  pub func: ?[]const u8,
  pub args: std.ArrayList(*Expr),
};

