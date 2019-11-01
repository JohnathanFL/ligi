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

  cur_block: *Block = RootBlock,
  const RootBlock: *Block = @intToPtr(*Block, 1);  

  pub fn parse(lexer: Lexer, alloc: *std.mem.Allocator) !Parser {
    var parser = Parser {
      .lexer = lexer,
      .cur = undefined,
      .alloc = alloc,
    };

    parser.cur = parser.lexer.scan();

    return try parser.block(false, false);
  }


  // Note: match_braces should imply may_name.
  fn block(self: *Parser, match_braces: bool, may_name: bool) !*Block {
    var res = Block.init(self.alloc);
    res.parent = self.cur_block;
    if(may_name) res.label = try self.label();
    
    self.cur_block = res; 
    
    if(match_braces) _ = try self.match(.LBrace);

    // If we don't need braces, we must be parsing a full file, so the end is an EOF
    while ((match_braces and !self.opt(.RBrace)) or (!match_braces and !self.opt(.EOF))) {
      switch(self.cur.tag) {
        .Let, .Var => {
          var stmt = try self.bindStmt(true, false, true);
          res.symbols.putNoClobber(stmt.bindStmt.binding, stmt.bindStmt) catch return error.BindingAlreadyInScope;
          res.stmts.append(stmt);
        },
        .If => res.stmts.append(try self.ifStmt()),
        .While, .For => res.stmts.append(try self.loopStmt()),
        else => {
          // This will be typechecked as being void
          res.stmts.append(try self.expr());
          self.match(';');
        }
      }
    }


    if(match_braces) try self.match(.RBrace);

    self.cur_block = res.parent;
    return res;
  }

  // Used for let/var/captures/arguments/etc. That's why there are so many parameters
  fn bindStmt(self: *Parser, 
    comptime need_mutability: bool, 
    comptime need_type: bool, 
    comptime can_default: bool) !*Stmt {
    
    const line = self.lexer.line;
    const col = self.lexer.col;
    const file_id = self.lexer.file_id;
    const mutable = if(need_mutability) try self.matchOne(.Let, .Var).tag == .Var else false;
    
    var res = try self.alloc.create(Stmt);
    res = Stmt {.bindStmt = undefined};
    
    res.bindStmt = BindStmt {
      .line = line,
      .col = col,
      .file_id = file_id,
      .is_var = mutable,
      .binding = try self.match(.Symbol).lexeme,
      .typeof = blk: {
        if(need_type and self.opt(.Colon)) {
          _ = try self.match(.Colon);
          break :blk try self.parseType();
        } else if (need_type) {
          return error.MissingTypeAfterColon;
        } else {
          break :blk &void_type;
        }
      },
      .initial = blk: {
        if(can_default and self.opt(.Assign)) {
          _ = try self.match(.Assign);
          break :blk try self.expr();
        } else if (!can_default and self.opt(.Assign)) {
          return error.CannotUseDefaultValue;
        } else if (can_default and !self.opt(.Assign)) {
          return error.DefaultValueRequired;
        } else {
          break :blk null;
        }
      },
    };
  }
  
  fn ifStmt(self: *Parser) !*Stmt {
    var res = IfStmt.init(self.alloc);
    
    _ = try self.match(.If);
    res.ifStmt.conds.append(try self.ifCondBlock());
    while(self.tryMatch(.ElIf)) res.ifStmt.conds.append(try self.ifCondBlock());
    
    if(self.tryMatch(.Else)) res.ifStmt.default = try self.block(true, false);
    if(self.tryMatch(.Finally)) res.ifStmt.finally = try self.block(true, false);
    return res;
  }

  fn ifCondBlock(self: *Parser) !IfCondBlock {
    var cond = try self.expr();
    var capture_token: ?*BindStmt = null;
    if(self.tryMatch(.Colon)) {
      _ = try self.match(.BitOr);
      capture_token = try self.bindStmt(false, false, false);
      _ = try self.match(.BitOr);
    }
    var if_so = try self.block(true, false);
  
    return IfCondBlock {
      .cond = cond,
      .capture_token = capture_token,
      .if_so = if_so
    };
  }

  fn loopStmt(self: *Parser) !*Stmt {
    var res = self.alloc.create(Stmt);
    var first_tag = try self.matchOne(.For, .While).tag;
    var expr = try self.expr();

    var counter: ?*BindStmt = null;
    var capture: ?*BindStmt = null;
    if (self.tryMatch(.Colon)) {
      _ = try self.match(.BitOr);
      counter = try self.bindStmt(false, false, false);
      if(self.tryMatch(.Comma)) capture = try self.bindStmt(false, false, false);
      _ = try self.match(.BitOr);
    }

    var after: ?*Expr = null;
    if (self.tryMatch(.Colon)) {
      if (first_tag == .For) return error.ContinuationInvalidOnFor;
      
      after = try self.expr();
    }

    var each = try self.block();
    
    var finally: ?*Block = null;
    if (self.tryMatch(.Finally)) finally = try self.block();

    res.* = Stmt {
      .loopStmt = LoopStmt {
        .variant = if (first_tag == .For) .For else .While ,
        .eval = expr,
        .counter = counter,
        .capture = capture,
        .after = after,
        .each = each,
        .finally = finally,
      }
    }; 
  }

  fn expr(self: *Parser) !*Expr {
    // Parse a BinExpr, starting with level 0 (assign ops)
    return try self.binExpr(0);
  }
  
  
  fn binExpr(self: *Parser, precedence: u32) !*Expr {
    if (precedence == Tag.binary_ops.len) return try self.unaryExpr();

    const ops = Tag.opsOfPrecedence[precedence];

    var res: *Expr = undefined;

    var first = self.binExpr(precedence + 1);
    if (self.optOne(ops)) { // We're more than just a single expr of lower prec
      var curOp = self.optOne(ops);
      res = try self.alloc.create(Expr);
      res.* = Expr {
        .call = Call {
          .func = curOp.lexeme,
          .args = ArrayList(*Expr).init(self.alloc),
        }
      };
      res.call.args.append(first);
      while(self.optOne(ops)) { // While there's still another expr of lower prec
        var thisOp = self.matchOne(ops);
        if (thisOp.tag != curOp.tag) { // i.e in 2/2*2, we go / -> *
          // Rotate the entire tree to the leftmost subnode of a new Expr for thisOp
          var oldRes = res;
          res = self.alloc.create(Expr);
          res.* = Expr {
            .call = Call {
              .func = thisOp.lexeme,
              .args = ArrayList(*Expr).init(self.alloc),
            }
          };

          res.call.args.append(oldRes);
          curOp = thisOp;
        }
        res.call.args.append(self.binExpr(precedence + 1));
      }
    } else {
      // If there's no operator, then there's no point in adding more indirection
      res = first;
    }
    
    return res;
  }

  fn unaryExpr(self: *Parser) !*Expr {
    var res: *Expr = undefined;
    if(self.optOne(Tag.unary_ops)) {
      var op = try self.matchOne(Tag.unary_ops);
      
      res = try self.alloc.create(Expr);
      res.* = Expr {
        .call = Call {
          .func = op.lexeme,
          .args = ArrayList(*Expr).init(self.alloc),
        },
      };

      res.call.args.append(try self.val());
    } else {
      res = try self.val();
    }
    return res;
  }

  fn val(self: *Parser) !*Expr {
    // Val is nothing but a redirect for the current token
    return switch (self.cur.tag) {
      .Case => self.caseExpr(),
      .Symbol, .IntLit, .BoolLit, .CharLit, 
      .StringLit, .LParen => try self.accessExpr(), // symbol.symbol()[] and such. Includes tuples
      else => {
        return error.UnexpectedToken;
      },
    };
  }
  
  fn caseExpr(self: *Parser) !*Expr {
    try self.matchAll([_]Tag{.Case, .LBrace});
    
    var res = try self.alloc.create(Expr);
    res.* = Expr {
      .caseExpr = CaseExpr {
        .arms = ArrayList(Case).init(self.alloc),
        .default = undefined
      }
    };
    
    // While we have a normal case arm
    while (self.cur.tag != .Else) {
      var cond = try self.expr();
      _ = try self.match(.Implies);
      res.caseExpr.arms.append(Case {
        .cond = cond,
        .val = try self.expr(),
      }); 
      _ = try self.match(.Comma);
    }
   
     
    try self.matchAll([_]Tag{.Else, .Implies});
    res.caseExpr.default = try self.expr();
    _ = try self.match(.RBrace); 
  }

  fn accessExpr(self: *Parser) !*Expr {
    var baseSymbol = self.baseVal();
    
    var res: *Expr = undefined;
    
    var keep_going = true;
    while(keep_going) {
      switch(self.cur.tag) {
        .LParen  => {
          _ = try self.match(.LParen);
          
          

          _ = try self.match(.RParen);
        },
        .LBracket => {

        },
        .Dot => {

        },
        else => keep_going = false,
      }
    }
  } 

  fn baseVal(self: *Parser) !*Expr {
    return switch (self.cur.tag) {
      .Symbol => try self.symbolExpr(), // symbol + any number of fields/indices/calls
      .LParen => try self.tuple(), // If it's a single-item tuple, it's implicitly convertable to a single value
      .IntLit, .BoolLit, .CharLit, .StringLit, => try self.literalExpr(),
      else => return error.UnexpectedToken,
    };  
  }

  fn symbolExpr(self: *Parser) !*Expr {
    var res = try self.alloc.create(Expr);

    var sym = try self.match(.Symbol);
    res.* = Expr {
      .val = Value {
        .symbol = sym
      }
    };

    if (self.optOne([_]Tag{.Dot, .LParen, .LBracket})) {
      while(self.optOne([_]Tag{.Dot, .LParen, .LBracket})) {

      }
    }
    return res;
  }

  fn label(self: *Parser) ![]const u8 {
    return (try self.match(.Label)).lexeme;
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
  
  fn matchAll(self: *Parser, tags: []const Tag) !void {
    for (tags) |tag| {
      _ = try self.match(tag);
    }
  }

  fn matchOne(self: *Parser, tags: []const Tag) !Token {
    for (tags) |tag| {
      if(self.opt(tag))
        return self.match(tag);
    }
  
    return error.NoMatchOne;
  }

  // If we can match it, do it and return true.
  fn tryMatch(self: *Parser, tag: Tag) bool {
    if(self.opt(tag)) {
      _ = self.match(tag) catch unreachable; // We already know it's there
      return true;
    } else {
      return false;
    }
  }

  fn match(self: *Parser, tag: Tag) !Token {
    if(self.opt(tag)) {
      var res = self.cur;
      self.cur = self.lexer.scan();
      return res;
    } else {
      return error.NoMatch;
    }
  }
};

test "parser" {
  var input =
    \\ let i = 0;
    \\ i = 1 + 4 * 5;
    \\ while (true and i > 0) : (--i) {
    \\   print("Hello world!");
    \\   print('\n');
    \\ }
  ;

  var lexer = Lexer{
    .input = input,
    .line = 0,
    .fileID = 0,
  };

  var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);

  var body = try Parser.parse(lexer, &arena.allocator);
}
