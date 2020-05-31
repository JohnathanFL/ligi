const std = @import("std");
const StrHashMap = std.StringHashMap;
const mem = std.mem;

const ast = @import("ast.zig");
const lex = @import("lexer.zig");

const str = []const u8;

// Will eventually be a complete file-recursing parser
pub fn parse(input: str, alloc: *mem.Allocator) !*ast.Expr {
  var parser = try Parser.init(input, 0, alloc);
  return parser.parseBlock(false);
}

pub const Parser = struct {
  alloc: *mem.Allocator,
  lexer: lex.Lexer,
  cur: lex.Token,
  /// Did moving to the current token take us over a linebreak?
  newlined: bool,
  // I'd say 1024 expressions is a reasonable starting place
  exprs: std.SegmentedList(ast.Expr, 1024),

  const Error = error {
    Expected,
    Unimplemented,
    OutOfMemory,
  } || lex.Lexer.Error;

  pub fn init(main: str, file: usize, alloc: *mem.Allocator) Error!Parser {
    var res = Parser {
      .alloc = alloc,
      .lexer = lex.Lexer.init(main, file, alloc),
      .exprs = std.SegmentedList(ast.Expr, 1024).init(alloc),
      .cur = undefined,
      .newlined = false,
    };
    // There's no way for the first
    res.cur = try res.lexer.lex();
    return res;
  }

  fn newExpr(self: *Parser, e: ast.Expr) *ast.Expr {
    self.exprs.push(e) catch unreachable;
    return self.exprs.at(self.exprs.len - 1);
  }
  fn newCall(self: *Parser, tgt: ast.CallTarget, args: []const *ast.Expr) *ast.Expr {
    var list = std.ArrayList(*ast.Expr).init(self.alloc);
    list.appendSlice(args) catch unreachable;
    return self.newExpr(.{ .Call = .{ .what = tgt, .args = list } });
  }

  fn nextIs(self: *Parser, tag: lex.Tag) bool {
    return self.cur.tag == tag;
  }
  fn match(self: *Parser, tag: lex.Tag) Error!lex.Token {
    if(self.tryMatch(tag)) |t| {
      return t;
    } else {
      std.debug.warn("{}: Expected {}, but found {}\n", .{self.cur.pos, tag, self.cur.tag});
      return error.Expected;
    }
  }
  fn tryMatch(self: *Parser, tag: lex.Tag) ?lex.Token {
    if(self.nextIs(tag)) {
      const prev = self.cur;
      // TODO: Fix this. This could cause problems when it comes to .UnTermChar/Str
      self.cur = self.lexer.lex() catch return null;
      if (prev.pos.line != self.cur.pos.line) self.newlined = true;
      return prev;
    } else {
      return null;
    }
  }
  fn tryMatchOne(self: *Parser, tags: []const lex.Tag) ?lex.Token {
    for(tags) |tag| {
      if(self.nextIs(tag)) return self.tryMatch(tag);
    }
    return null;
  }
  fn matchOne(self: *Parser, tags: []const lex.Tag) !lex.Token {
    if(self.tryMatchOne(tags)) |tok| {
      return tok;
    } else {
      std.debug.warn("{}: Expected one of {}, but found {}\n", .{self.cur.pos, tags, self.cur.tag});
    }
  }

  fn expected(self: *Parser, comptime what: str) Error {
    std.debug.warn("{}: Expected {}", .{self.cur.pos, what});
    return error.Expected;
  }

  pub fn parseBlock(self: *Parser, comptime braces: bool) Error!*ast.Expr {
    var res = self.newExpr(.{
      .Block = .{
        .label = null,
        .body = std.ArrayList(*ast.Expr).init(self.alloc),
      }
    });
    var this = &res.Block;
    
    if(braces) _ = try self.match(.LBrace);
    const end: lex.Tag = if(braces) .RBrace else .EOF ;

    while(!self.nextIs(end))
      try this.body.append(try self.parseStmt());

    _ = try self.match(end);

    return res;
  }

  pub fn parseStmt(self: *Parser) Error!*ast.Expr {
    switch(self.cur.tag) {
      .Let, .Var, .CVar, .Field, .Property, .Using => return self.parseBindStmt(),
      .Assert => {
        _ = try self.match(.Assert);
        return self.newExpr(.{ .Assert = try self.parseExpr() });
      },
      // break [label [',' expr]]
      .Break => {
        _ = try self.match(.Break);
        var lab: ?str = null;
        var val: ?*ast.Expr = null;
        if (self.tryMatch(.Label)) |label| {
          lab = label.str;
          if (self.tryMatch(.Comma) != null)
            val = try self.parseExpr();
        }
        return self.newExpr(.{ .Break = .{ .label = lab, .val = val } });
      },
      .Return => {
        _ = try self.match(.Return);
        // A return can only come before a }, so anything else must be an expr
        const val = if(!self.nextIs(.RBrace)) try self.parseExpr() else null;
        return self.newExpr(.{ .Return = val });
      },
      .Defer => {
        _ = try self.match(.Defer);
        return self.newExpr(.{ .Defer = try self.parseExpr() });
      },
      else => return try self.parseAssg(),
    }
  }
  // [using] bindop {bindloc [= expr]}
  pub fn parseBindStmt(self: *Parser) Error!*ast.Expr {
    const using = if (self.tryMatch(.Using) != null) true else false;
    const op =
      if(self.tryMatch(.Let) != null) ast.BindOp.Let
      else if (self.tryMatch(.Var) != null) ast.BindOp.Var
      else if (self.tryMatch(.CVar) != null) ast.BindOp.CVar
      else if (self.tryMatch(.Field) != null) ast.BindOp.Field
      else if (self.tryMatch(.Enum) != null) ast.BindOp.Enum
      else return self.expected("a bind spec");
      

    const res = self.newExpr(.{.Bind = .{
      .op = op,
      .using = using,
      .locs = std.ArrayList(ast.LocInit).init(self.alloc),
    }});
    const this = &res.Bind;

    while(true) {
      const loc = try self.parseBindLoc();
      const initExpr =
        if(self.tryMatch(.Assg) != null) try self.parseExpr()
        else null;
      try this.locs.append(.{.loc = loc, .init = initExpr});
      
      if(self.tryMatch(.Comma) == null) break;
    }

    return res;
  }
  // name level [: expr] | '(' loc {',' loc} ')' [: expr]
  // Thus, you are allowed to specify the bind types on the inside or outside of a bind tuple.
  // Thus, you can unpack a tuple without specifying each inside variable's type while
  // still having type safety (for functions and the like)
  pub fn parseBindLoc(self: *Parser) Error!ast.BindLoc {
    if(self.tryMatch(.LParen) != null) {
      var locs = std.ArrayList(ast.BindLoc).init(self.alloc);
      // No trailing commas allowed for binds
      while(true) {
        try locs.append(try self.parseBindLoc());
        if(self.tryMatch(.Comma) == null) break;
      }
      
      _ = try self.match(.RParen);
      const ty = try self.parseTypeSpec(false);
        
      return ast.BindLoc { .Tuple = .{ .locs = locs, .ty = ty } };
    } else {
      const name = (try self.match(.Word)).str;
      const access =
        if(self.tryMatch(.Add) != null) ast.BindLevel.Pub
        else if (self.tryMatch(.Sub) != null) ast.BindLevel.Priv
        else if (self.tryMatch(.Mul) != null) ast.BindLevel.ReadOnly
        else ast.BindLevel.Priv;
      const ty = try self.parseTypeSpec(false);
      return ast.BindLoc{.Named = .{ .name = name, .access = access, .ty = ty }};
    }
  }

  // Just a special case of parseBin
  pub fn parseAssg(self: *Parser) Error!*ast.Expr {
    const lhs = try self.parseExpr();
    if(self.tryMatchOne(&[_]lex.Tag{.Assg, .AddAssg, .SubAssg, .MulAssg, .DivAssg})) |op| {
      const rhs = try self.parseExpr();
      return self.newCall(.{ .Op = ast.Op.fromTag(op.tag, true) }, &[_]*ast.Expr{lhs, rhs});
    } else return lhs;
  }
  // Just a sugar for a bin_expr
  pub fn parseExpr(self: *Parser) Error!*ast.Expr { return self.parseBin(0); }
  pub fn parseBin(self: *Parser, comptime prec: usize) Error!*ast.Expr {
    var lhs =
      if(prec + 1 == BIN_OPS.len) try self.parseUna()
      else try self.parseBin(prec + 1);
    // TODO: Verify that this has left associativity
    while(self.tryMatchOne(BIN_OPS[prec])) |tok| {
      const rhs =
        if(prec + 1 == BIN_OPS.len) try self.parseUna()
        else try self.parseBin(prec + 1);
      lhs = self.newCall(
        .{ .Op = ast.Op.fromTag(tok.tag, true) },
        &[_]*ast.Expr{ lhs, rhs }
      );
    }

    return lhs;
  }
  pub fn parseUna(self: *Parser) Error!*ast.Expr {
    if (self.tryMatchOne(&UNA_OPS)) |tok| {
      return self.newCall(
        .{.Op = ast.Op.fromTag(tok.tag, false)},
        &[_]*ast.Expr{ try self.parseUna() }
      );
    } else {
      return try self.parseAtom();
    }
  }

  pub fn parseAtom(self: *Parser) Error!*ast.Expr {
    switch(self.cur.tag) {
      .If => return try self.parseIf(),
      .When => return try self.parseWhen(),
      .Loop => return try self.parseLoop(),
      .While, .For => return try self.parseWhileFor(),
      .Label, .LBrace => return try self.parseBlock(true),
      .Tag => return try self.parseEnumLit(),
      else => return try self.parseAccess(),
    }
  }
  // '#' word [ tuple | compound ]
  pub fn parseEnumLit(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.Tag);
    const tag = (try self.match(.Word)).str;
    const inner = if (!self.newlined) switch(self.cur.tag) {
      .LParen => try self.parseTuple(),
      .LBracket => try self.parseCompound(),
      else => null
    } else null;

    return self.newExpr(.{.EnumLit = .{
      .tag = tag,
      .inner = inner
    }});
  }

  // '[' [':' expr ':'] {'.'word ['=' expr] | expr}  ']'
  pub fn parseCompound(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.LBracket);
    const as = try self.parseTypeSpec(true);

    // Note this technically means it's impossible to have an empty struct lit,
    // as it'll parse as an array lit
    if(self.nextIs(.Access)) { // struct lit
      var fields = StrHashMap(?*ast.Expr).init(self.alloc);
      while(self.tryMatch(.Access) != null) {
        const name = (try self.match(.Word)).str;
        const val =
          if(self.tryMatch(.Assg) != null) try self.parseExpr()
          else null;
        try fields.putNoClobber(name, val);

        // This combined with the while cond allows trailing commas
        if(self.tryMatch(.Comma) == null) break;
      }
      return self.newExpr(.{.Struct = .{
        .as = as,
        .fields = fields
      }});
    } else { // array lit
      return self.newExpr(.{.Array = .{
        .as = as,
        .vals = try self.parseExprList(.RBracket)
      }});
    }

    _ = try self.match(.RBracket);
  }

  pub fn parseTuple(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.LParen);
    var res = self.newExpr(.{.Tuple = .{
      .as = try self.parseTypeSpec(true),
      .vals = try self.parseExprList(.RParen)
    }});
    _ = try self.match(.RParen);
    return res;
  }

  /// Parse a csv list of expressions, which may have a trailing comma
  pub fn parseExprList(self: *Parser, closer: lex.Tag) Error!std.ArrayList(*ast.Expr) {
    var vals = std.ArrayList(*ast.Expr).init(self.alloc);
    while(!self.nextIs(closer)) {
      try vals.append(try self.parseExpr());
      if(self.tryMatch(.Comma) == null) break;
    }
    return vals;
  }

  /// Either `:expr` or `:expr:`, depending on trailing_colon
  pub fn parseTypeSpec(self: *Parser, comptime trailing_colon: bool) Error!?*ast.Expr {
    if(self.tryMatch(.Colon) != null) {
      const res = try self.parseExpr();
      if(trailing_colon) _ = try self.match(.Colon);
      return res;
    } else return null;
  }

  // (word|tuple|compound) ~ {('.' | '::') ~ (call | index | swizzle)}
  pub fn parseAccess(self: *Parser) Error!*ast.Expr {
    const what = switch(self.cur.tag) {
      .Word => self.newExpr(.{.Word = (try self.match(.Word)).str}),
      .LParen => try self.parseTuple(),
      .LBracket => try self.parseCompound(),
      else => return self.expected("word, tuple, or compound")
    };

    var access = std.ArrayList(ast.SwizzleOp).init(self.alloc);

    switch(self.cur.tag) {
      .Access, .Pipe => try self.parseSwizzle(&access),
      .LParen, .LBracket => if(!self.newlined) try self.parseSwizzle(&access),
      else => {}
    }

    return self.newExpr(.{.Access = .{
      .what = what,
      .access = access
    }});
  }

  pub fn parseSwizzle(self: *Parser, dest: *std.ArrayList(ast.SwizzleOp)) Error!void {
    if(self.tryMatch(.Word)) |w| {
      try dest.append(.{ .Name = w.str });
    } else if (self.tryMatch(.Access)) |_| {
      try dest.append(.{ .Descend = .{} });
      if(self.tryMatch(.LParen)) |_| {
        try dest.append(.{.GroupBegin = .{}});
        try self.parseSwizzle(dest);
        _ = try self.match(.RParen);
        try dest.append(.{.GroupEnd = .{}});
      }
    } else if (self.tryMatch(.Pipe)) |_| {
      try dest.append(.{ .Pipe = .{} });
      if(!self.nextIs(.Word)) {
      }
    } else if(!self.newlined and (self.nextIs(.LParen) or self.nextIs(.LBracket))) {
      const index = if(self.tryMatch(.LBracket)) |_| true else false;
      const closer: lex.Tag = if(index) .RBracket else .RParen;
      try dest.append(.{.Call = .{
        .index = index,
        .args = try self.parseExprList(closer)
      }});
      _ = try self.match(closer);
    }
  }
  
  pub fn parseIf(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.If);
    var arms = std.ArrayList(ast.IfArm).init(self.alloc);
    while(true) {
      const cond = try self.parseExpr();
      const capt = if(self.tryMatch(.StoreIn)) |_| try self.parseBindLoc() else null;
      const then = try self.parseBlock(true);
      try arms.append(.{ .cond = cond, .capt = capt, .then = then });

      if(self.tryMatch(.ElIf) == null) break;
    }
    const default = if(self.tryMatch(.Else)) |_| try self.parseBlock(true) else null;
    const finally = if(self.tryMatch(.Finally)) |_| try self.parseBlock(true) else null;
    return self.newExpr(.{.If = .{
      .arms = arms,
      .default = default,
      .finally = finally
    }});
  }
  pub fn parseWhen(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.When);
    const expr = try self.parseExpr();
    
    var arms = std.ArrayList(ast.WhenArm).init(self.alloc);
    while(self.tryMatch(.Is)) |_| {
      const op = if (self.tryMatchOne(&IS_OPS)) |tok| ast.Op.fromTag(tok.tag, true) else .Eq;
      const val = try self.parseExpr();
      const capt = if(self.tryMatch(.StoreIn)) |_| try self.parseBindLoc() else null;
      const then = try self.parseBlock(true);
      try arms.append(.{ .op = op, .val = val, .capt = capt, .then = then });
    }
    const default = if(self.tryMatch(.Else)) |_| try self.parseBlock(true) else null;
    const finally = if(self.tryMatch(.Finally)) |_| try self.parseBlock(true) else null;
    return self.newExpr(.{.When = .{
      .expr = expr,
      .arms = arms,
      .default = default,
      .finally = finally
    }});
  }
  pub fn parseLoop(self: *Parser) Error!*ast.Expr {
    _ = try self.match(.Loop);
    const counter = if(self.tryMatch(.StoreIn)) |_| try self.parseBindLoc() else null;
    const body = try self.parseBlock(true);
    return self.newExpr(.{.Loop = .{
      .expr = null,
      .cond = .NOP,
      .capt = null,
      .counter = counter,
      .body = body,
      .finally = null,
    }});
  }
  pub fn parseWhileFor(self: *Parser) Error!*ast.Expr {
    const range =
      if(self.tryMatch(.For)) |_| true
      else if(self.tryMatch(.While)) |_| false
      else return self.expected("while or for");
    const expr = try self.parseExpr();

    var capt: ?ast.BindLoc = null;
    var counter: ?ast.BindLoc = null;
    if(self.tryMatch(.StoreIn)) |_| {
      capt = try self.parseBindLoc();
      if(self.tryMatch(.Comma)) |_| counter = try self.parseBindLoc();
    }
    const body = try self.parseBlock(true);
    const finally = if(self.tryMatch(.Finally)) |_| try self.parseBlock(true) else null;
    return self.newExpr(.{.Loop = .{
      .expr = expr,
      .cond = if (range) .For else .While,
      .capt = capt,
      .counter = counter,
      .body = body,
      .finally = finally,
    }});
  }
  

  const BIN_OPS = .{
    &[_]lex.Tag{.Eq, .NotEq, .Gt, .Lt, .GtEq, .LtEq, .Spaceship},
    &[_]lex.Tag{.Or, .Xor},
    &[_]lex.Tag{.And},
    &[_]lex.Tag{.In, .NotIn},
    &[_]lex.Tag{.OpenRange, .ClosedRange},
    &[_]lex.Tag{.Add, .Sub},
    &[_]lex.Tag{.Mul, .Div, .Mod},
    &[_]lex.Tag{.BitOr, .BitAnd, .BitXor},
  };
  const UNA_OPS = [_]lex.Tag{
    .Sub, .Enum, .Mul,
    .Struct, .Ref, .Slice, .Array, .Const, .Comptime,
    .BitNot, .Not, .Opt, .Pure, .Inline, .Overload, .Property,
  };

  const IS_OPS = [_]lex.Tag {
    .Eq, .NotEq, .In, .NotIn
  };
};
