const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;

const Tokens = @import("token.zig");
const Token = Tokens.Token;
const Tag = Tokens.Tag;

// There are 3 major types in the AST:
//  Stmt: Broken into If/Loop/Bind. Can also point to a Block/Expr
//  Block: Contains Stmts. May be labeled
//  Expr: Produces a value (may be void). Broken into Literal/Symbol/Case/Call.
//    A call is used for all operators. (e.g foo.bar => Call('.', 'foo', 'bar') )


///////////////////////////////////////////////////////////////////////////////////////////////////
// Stmt Stuff
///////////////////////////////////////////////////////////////////////////////////////////////////
pub const Stmt = union(enum) {
  If: If,
  /// Covers For and While
  Loop: Loop,
  Bind: Bind,
  Block: *Block,
  Expr: *Expr,

  pub fn fromExpr(expr: *Expr, alloc: *Allocator) *Stmt {
    var res = alloc.create(Stmt) catch unreachable;
    res.* = Stmt { .Expr = expr };
    return res;
  }

  pub fn fromBlock(block: *Block, alloc: *Allocator) *Stmt {
    var res = alloc.create(Stmt) catch unreachable;
    res.* = Stmt { .Block = block };
    return res;
  }
};

pub const IfArm = struct {
  cond: *Expr,
  val: *Block
};

pub const If = struct {
  cond: *Expr,
  arms: ArrayList(IfArm),
  default: ?*Block,
  finally: ?*Block,

  pub fn new(alloc: *std.mem.Allocator) *Stmt {
    var res = alloc.create(Stmt) catch unreachable;
    res.* = Stmt {
      .If = If {
        .cond = undefined,
        .arms = ArrayList(IfArm).init(alloc),
        .default = null,
        .finally = null,
      },
    };
    return res;
  }

  pub fn pushArm(self: *If, cond: *Expr, val: *Block) void {
    self.arms.append(IfArm{ .cond = cond, .val = val}) catch unreachable;
  }
};

pub const LoopType = enum {
  For, While, Infinite // Maybe
};
pub const Loop = struct {
  ty: LoopType,
  eval: *Expr, // Evaluated once for a for loop, each loop for a while
  counter: ?*Bind, // Loop iteration number
  capture: ?*Bind, // Capture from foreach or optional in a while
  do: *Block,
  finally: ?*Block, // Executed if the loop isn't broken

  pub fn new(alloc: *Allocator, ty: LoopType) *Stmt {
    var res = alloc.create(Stmt) catch unreachable;
    res.* = Stmt {
      .Loop = Loop {
        .ty = ty,
        .eval = undefined,
        .counter = null,
        .capture = null,
        .do = undefined,
        .finally = null
      }
    };
    return res;
  }

};

pub const Bind = struct {
  mut: bool, // True for var, false for let
  loc: Token,
  ty: Type,
  val: ?*Expr, // The default value

  pub fn new(alloc: *Allocator, mut: bool) *Stmt {
    var res = alloc.create(Stmt) catch unreachable;
    res.* = Stmt {
      .Bind = Bind {
        .mut = mut,
        .loc = undefined,
        .ty = undefined,
        .val = null,
      }
    };
    return res;
  }
};


///////////////////////////////////////////////////////////////////////////////////////////////////
// Block Stuff
///////////////////////////////////////////////////////////////////////////////////////////////////
pub const Block = struct {
  label: ?Token,
  stmts: ArrayList(*Stmt),
  binds: StringHashMap(*Bind),
  parent: ?*Block,
  pub fn new(parent: ?*Block, alloc: *Allocator) *Block {
    var res = alloc.create(Block) catch unreachable;
    res.* = Block {
      .label = null,
      .stmts = ArrayList(*Stmt).init(alloc),
      .binds = StringHashMap(*Bind).init(alloc),
      .parent = parent,
    };
    return res;
  }
  pub fn pushStmt(self: *Block, stmt: *Stmt) void {
      self.stmts.append(stmt) catch unreachable;
  }
  pub fn pushBind(self: *Block, bind: *Bind) !void {
      _ = self.binds.putNoClobber(bind.loc.lexeme, bind) catch return error.BindingAlreadyInScope;
  }
};


///////////////////////////////////////////////////////////////////////////////////////////////////
// Expr Stuff
///////////////////////////////////////////////////////////////////////////////////////////////////
pub const Expr = union(enum) {
  Call: Call,
  Literal: Token,
  Symbol: Token,
  Case: Case,
  Tuple: ArrayList(*Expr),
  pub fn newLiteral(tok: Token, alloc: *Allocator) *Expr {
    var res = alloc.create(Expr) catch unreachable;
    res.* = Expr {
      .Literal = tok,
    };
    return res;
  }

  pub fn newSymbol(tok: Token, alloc: *Allocator) *Expr {

    var res = alloc.create(Expr) catch unreachable;
    res.* = Expr {
      .Symbol = tok,
    };
    return res;
  }

  pub fn newTuple(alloc: *Allocator, first_val: *Expr) *Expr {
    var res = alloc.create(Expr) catch unreachable;
    res.* = Expr {
      .Tuple = ArrayList(*Expr).init(alloc)
    };
    res.Tuple.append(first_val) catch unreachable;
    return res;
  }
};

pub const Call = struct {
  func: Token,
  args: ArrayList(*Expr),
  
  pub fn new(func: Token, alloc: *Allocator) *Expr {
    var res = alloc.create(Expr) catch unreachable;
    res.* = Expr {
      .Call = Call {
        .func = func,
        .args = ArrayList(*Expr).init(alloc),
      }
    };

    return res;
  }

  pub fn pushArg(self: *Call, arg: *Expr) void {
    _ = self.args.append(arg) catch unreachable;
  }
};

pub const CaseArm = struct {
  cond: *Expr,
  val: *Expr,
};

pub const Case = struct {
  arms: ArrayList(CaseArm),
  default: *Expr,

  pub fn new(alloc: *Allocator) *Expr {
    var res = alloc.create(Expr) catch unreachable;
    res.* = Expr {
      .Case = Case {
        .arms = ArrayList(CaseArm).init(alloc),
        .default = undefined,
      }
    };
    return res;
  }
};

///////////////////////////////////////////////////////////////////////////////////////////////////
// Type Stuff
///////////////////////////////////////////////////////////////////////////////////////////////////
pub const BaseType = union(enum) {
  // Refers to another Bind somewhere
  Symbol: Token,
  Fn: FnType,
  StructDef: StructDef,
  EnumDef: EnumDef,
  UnionDef: UnionDef,
  // Zag will follow Nim's lead on concepts over interfaces
  ConceptDef: ConceptDef,
  VoidType: void,
};

pub const TypeMod = union(enum) {
  Optional: void,
  Slice: void,
  Array: usize,
};
// TODO: Error types, a la zig
pub const Type = struct {
  mods: ArrayList(TypeMod),
  base: BaseType,

  pub fn pushOpt(self: *Type) void {
    self.mods.append(TypeMod{ .Optional = {}}) catch unreachable;
  }
  pub fn pushSlice(self: *Type) void {
    self.mods.append(TypeMod{.Slice = {}}) catch unreachable;
  }
  pub fn pushArray(self: *Type, size: usize) void {
    self.mods.append(TypeMod{.Array = size}) catch unreachable;
  }
}; 

pub const FnType = struct {
  pure: bool,
  args: ArrayList(*Bind),
  // Zag does all returns with return value semantics
  ret: *Bind,
};

pub const EnumDef = void; // TODO
pub const StructDef = void; // TODO
pub const UnionDef = void; // TODO
pub const ConceptDef = void; // TODO


