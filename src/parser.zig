const std = @import("std");
const ArrayList = std.ArrayList;

const LexerMod = @import("lexer.zig");
const Lexer = LexerMod.Lexer;
const Token = LexerMod.Token;
const Tag = LexerMod.Tag;
const LexVal = LexerMod.LexVal;

pub usingnamespace @import("node.zig");

pub const Parser = struct {
  lexer: Lexer,
  cur: Token,

  alloc: *std.mem.Allocator,

  cur_block: *Block = undefined,

  const ParseError = error {
      UnexpectedToken,
      BindingAlreadyInScope,
      ContinuationInvalidOnFor,
      
      CantDefaultValue,
      MustDefaultValue,
      
      NotImplemented,
  };

  pub fn parse(lexer: Lexer, alloc: *std.mem.Allocator) ParseError!*Block {
    var parser = Parser {
      .lexer = lexer,
      .cur = undefined,
      .alloc = alloc,
    };

    parser.cur = parser.lexer.scan();

    return try parser.block(null, false, false);
  }


  // Note: match_braces should imply may_name.
  fn block(self: *Parser, parent: ?*Block, comptime match_braces: bool, comptime may_name: bool) ParseError!*Block {
    var res = Block.new(parent, self.alloc);
    self.cur_block = res;
    var s: Stmt = undefined; 
    if(may_name) res.label = try self.matchGet(.Label);

    if(match_braces) try self.match(.LBrace);
        
    const end_tag = if (match_braces) Tag.RBrace else Tag.EOF;
    while(!self.opt(end_tag)) {
      // stmt() will add binds to self.cur_block
      res.pushStmt(try self.stmt());
    }

    if(match_braces) try self.match(.LBrace);
    return res;
  }

  fn stmt(self: *Parser) ParseError!*Stmt {
    var res: *Stmt = undefined;

    switch(self.cur.tag) {
      .If => res = try self.ifStmt(),
      .While, .For => res = try self.loopStmt(),
      .Let, .Var => {
        res = try self.bindStmt(true, false, true);
        // To get to this, we must be in some block
        try self.cur_block.pushBind(&res.Bind);
        try self.match(.Semicolon);
      },
      .LBrace => res = Stmt.fromBlock(try self.block(self.cur_block, true, true), self.alloc),
      else => {
        std.debug.warn("Got in with tag {}\n\n", self.cur.tag);
        res = Stmt.fromExpr(try self.expr(), self.alloc);
      }
    }
    return res;
  }

  // Used for let/var/captures/arguments/etc. That's why there are so many parameters
  fn bindStmt(self: *Parser, 
    comptime need_mutability: bool, // Do we need to specify let/var?
    comptime need_type: bool, // Do we need to specify a type?
    comptime can_default: bool // Can we provide a default value?
  ) ParseError!*Stmt {
    var mut = false;
    if(need_mutability) {
      const start = try self.matchOne([_]Tag{.Let, .Var});
      mut = start.tag == .Var;
    }
    
    var res = Bind.new(self.alloc, mut);

    res.Bind.loc = try self.matchGet(.Symbol);
    if (need_type or self.opt(.Colon)) {
      res.Bind.ty = try self.parseType();
    }
    
    if(can_default) {
      if(self.opt(.Assign)) {
        try self.match(.Assign);
        res.Bind.val = try self.expr();
      }
    }
    
    return res; 
  }

  fn ifStmt(self: *Parser) ParseError!*Stmt {
    return error.NotImplemented;
  }

  fn loopStmt(self: *Parser) ParseError!*Stmt {
    return error.NotImplemented;
  }

  fn expr(self: *Parser) ParseError!*Expr {
    return self.binExprLevel(0);
  }

  fn binExprLevel(self: *Parser, level: usize) ParseError!*Expr {
    if (level == Tag.binary_ops.len) return try self.unaryExpr();

    var res = try self.binExprLevel(level + 1);
    if(self.optOne(Tag.binary_ops[level])) {
      const lhs = res;
      res = Call.new(self.cur.lexeme, self.alloc);
      res.Call.pushArg(lhs);

      var prev_op = self.cur.tag;
      while(self.tryMatchOne(Tag.binary_ops[level])) |op| {
        const rhs = try self.binExprLevel(level + 1);

        if (op.tag != prev_op) {
          // Need to rotate the tree to preserve left->right
          // For example:
          // a/b*c should be (lisp style) (* (/ a b) c), but a direct left->right translation will give
          // (/ a (* b c)). To rectify this, when we change between ops of the same precedence, we make the
          // old tree become the new leftmost part of the new tree with the new op
          var newRes = Call.new(op.lexeme, self.alloc);
          newRes.Call.pushArg(res);
          newRes.Call.pushArg(rhs);
        } else {
          res.Call.pushArg(rhs);
        }
        prev_op = op.tag;
      }
    }
    return res;
  }

  // We don't allow a unary expression before a case expression
  // It would just look ugly:
  // let x = -case{true => 1, else => 0};
  fn val(self: *Parser) ParseError!*Expr {
    return switch(self.cur.tag) {
      .Case => return try self.caseExpr(),
      else => return try self.unaryExpr(),
    };
  }

  fn caseExpr(self: *Parser) ParseError!*Expr {
    return error.NotImplemented;
  }

  fn unaryExpr(self: *Parser) ParseError!*Expr {
    var res: *Expr = undefined;
    if (self.tryMatchOne(Tag.unary_ops)) |op| {
      res = Call.new(op.lexeme, self.alloc);
      res.Call.pushArg(try self.accessExpr());
    } else {
      res = try self.accessExpr();
    }

    return res;
  }

  fn accessExpr(self: *Parser) ParseError!*Expr {
    // Note: This implies that even a literal like 1 can have field access:
    // let x = 1.foobar;
    // or even function calls:
    // let x = 1(true);
    // This does, however, make parsing struct literals a little nicer
    var res = try self.baseVal();

    if (self.optOne(Tag.access_ops)) {
      while(self.tryMatchOne(Tag.access_ops)) |op| {
        const old_res = res;
        switch (op.tag) {
          .Dot => {
            res = Call.new(op.lexeme, self.alloc);
            res.Call.pushArg(old_res);
            res.Call.pushArg(try self.baseVal());
          },
          .LParen, .LBracket => {
            // Note: this implies that you can do things like array[0, 1]            
            // It also technically implies you could do array[]
            // I'm thinking that should be handled as a typechecker-level error (bad code style)
            const closer = if(op.tag == .LParen) Tag.RParen else Tag.RBracket;

            // TODO: Should we rename the () function as `call` instead?
            res = Call.new(if (op.tag == .LParen) "()" else "[]", self.alloc);
            res.Call.pushArg(old_res);
            while(!self.opt(closer)) {
              res.Call.pushArg(try self.expr());
              if(!self.opt(closer)) try self.match(.Comma);
            }
            try self.match(closer);
          },
          else => unreachable,
        }
      }
    }

    return res;
  }

  fn baseVal(self: *Parser) ParseError!*Expr {
    return switch (self.cur.tag) {
      .Symbol => Expr.fromSymbol(self.matchGet(.Symbol) catch unreachable, self.alloc),

      .IntLit, .BoolLit, 
      .FloatLit, .StringLit, 
      .CharLit  => Expr.fromLiteral(self.tryMatchOne([_]Tag{.IntLit, .BoolLit, .FloatLit, .StringLit, .CharLit}).?, self.alloc),
      .LParen => try self.tupleExpr(),
      else => return error.UnexpectedToken,
    };
  }

  fn tupleExpr(self: *Parser) ParseError!*Expr {
    return error.NotImplemented;
  }

  fn parseType(self: *Parser) ParseError!Type {
    var res = Type {
      .mods = ArrayList(TypeMod).init(self.alloc),
      .base = undefined
    };
    while(self.optOne([_]Tag{.Opt, .LBracket})) {
      if (self.tryMatch(.Opt)) |_| {
        res.pushOpt();
      } else {
        try self.match(.LBracket);
        if(self.opt(.RBracket)) {
          // [] MUST be a slice
          res.pushSlice();
        } else {
          const size = (try self.matchGet(.IntLit)).val.IntLit;
          res.pushArray(@intCast(usize, size));
          try self.match(.RBracket);
        }
      } 
    }
    if (self.tryMatch(.Symbol)) |sym| {
      res.base = BaseType{ .Symbol = sym };
    } else if (self.tryMatchOne([_]Tag{.PureFn, .Fn})) |fn_tok| {
      return error.NotImplemented;
    } else if (self.tryMatch(.Struct)) |_| {
      res.base = BaseType { .StructDef = {} }; // = try self.structDef() };
    } else if (self.tryMatch(.Enum)) |_| {
      res.base = BaseType { .EnumDef = {}}; //= try self.enumDef() };
    } else if (self.tryMatch(.Union)) |_| {
      res.base = BaseType { .UnionDef = {}}; // = try self.unionDef() };
    } else if (self.tryMatch(.Concept)) |_| {
      res.base = BaseType { .ConceptDef = {}}; // = try self.conceptDef() };
    } else if (self.tryMatch(.Void)) |_| {
      res.base = BaseType {.VoidType = {}};
    } else {
      return error.UnexpectedToken;
    }

    return res;
  } 
  
  fn opt(self: Parser, tag: Tag) bool {
    return self.cur.tag == tag;
  }
  fn optOne(self: *Parser, tags: []const Tag) bool {
    for (tags) |tag| {
      if(self.opt(tag))
        return true;
    } else {
      return false;
    }
  }
  
  fn matchAll(self: *Parser, tags: []const Tag) ParseError!void {
    for (tags) |tag| {
      _ = try self.match(tag);
    }
  }

  fn matchOne(self: *Parser, tags: []const Tag) ParseError!Token {
    for (tags) |tag| {
      if(self.opt(tag))
       return self.matchGet(tag);
    }
  
    return error.UnexpectedToken;
  }

  // If we can match it, do it and return true.
  fn tryMatch(self: *Parser, tag: Tag) ?Token {
    if(self.opt(tag)) {
      return self.matchGet(tag) catch unreachable; // We already know it's there
    } else {
      return null;
    }
  }
  fn tryMatchOne(self: *Parser, tags: []const Tag) ?Token {
    return self.matchOne(tags) catch return null;
  }

  fn match(self: *Parser, tag: Tag) ParseError!void {
    _ = try self.matchGet(tag);
  }

  fn matchGet(self: *Parser, tag: Tag) ParseError!Token {
    if(self.opt(tag)) {
      var res = self.cur;
      self.cur = self.lexer.scan();
      return res;
    } else {
      return error.UnexpectedToken;
    }
  }
};

fn adapter(msg: []const u8) void {
  std.debug.warn("{}", msg);
}

test "parser" {
  var input =
    \\ let i;
    \\ var j;
    \\ let x = 1;
    \\ let y = x.foo(b);
  ;

  var lexer = Lexer{
    .input = input,
    .line = 0,
    .file_id = 0,
  };

  var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);
 
  var body = try Parser.parse(lexer, &arena.allocator);
  PrettyPrinter.prettyPrint(body);
}
