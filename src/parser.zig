const std = @import("std");

const LexerMod = @import("lexer.zig");
const Lexer = LexerMod.Lexer;
const Token = LexerMod.Token;
const Tag = LexerMod.Tag;
const LexVal = LexerMod.LexVal;

pub usingnamespace @import("node.zig");

pub const Parser = struct {
  lexer: Lexer,
  cur: Token = null,

  alloc: *std.mem.Allocator,
  
  cur_block: ArrayList(*Block),

  fn curBlock(self: Parser) *Block {
    return self.cur_block.at(self.cur_block.len - 1);
  }

  fn popBlock(self: *Parser) *Block {
    return self.cur_block.pop();
  }
  
  fn pushBlock(self: *Parser, block: *Block) void {
    self.cur_block.append(block);
  }

  fn parse() *Block {
    return self.block(false);
  }

  fn block(self: *Parser, match_braces: bool, may_name: bool) *Block {
    var res = Block.init(self.alloc);
    
    if(may_name) res.label = self.label();

    self.pushBlock(res);        
    
    if(match_braces) match(.LBrace);

    // If we don't need braces, we must be parsing a full file, so the end is an EOF
    while ((match_braces && !self.opt(.RBrace)) || (!match_braces && !self.opt(.EOF))) {
      switch(self.cur.tag) {
        .Let, .Var => {
          var stmt = self.bindStmt(true, false, true);
          res.symbols.putNoClobber(stmt.bindStmt.binding, stmt.bindStmt) catch {
            printf("Binding {} already exists in scope!", std.bindStmt.binding);
            std.process.exit(1);
          };
          res.stmts.append(stmt);
        },
        .If => res.stmts.append(self.ifStmt()),
        .While, .For => res.stmts.append(self.loopStmt()),
        else => res.stmts.append(self.exprStmt)),
      }
    }


    if(match_braces) match(.RBrace);

    _ = self.popBlock();
    return res;
  }

  // Used for let/var/captures/arguments/etc. That's why there are so many parameters
  fn bindStmt(self: *Parser, 
    comptime need_mutability: bool, 
    comptime need_type: bool, 
    comptime can_default: bool) *Stmt {
    
    const line = self.lexer.line;
    const col = self.lexer.col;
    const file_id = self.lexer.file_id;
    const mutable = if(need_mutability) self.matchOne(.Let, .Var).tag == .Var else false;
    
    var res = self.alloc.create(Stmt);
    res = Stmt {.bindStmt = self.alloc.create(BindStmt)};
    
    res.bindStmt = BindStmt {
      .line = line,
      .col = col,
      .file_id = file_id,
      .is_var = mutable,
      .binding = self.match(.Symbol).lexeme,
      .typeof = blk: {
        if(need_type && self.opt(.Colon)) {
          _ = self.match(.Colon);
          break :blk self.parseType();
        } else if (need_type) {
          printf("We needed a type there!");
          std.process.exit(1);
        } else {
          break :blk &void_type;
        }
      },
      .initial = blk: {
        if(can_default && self.opt(.Assign)) {
          _ = self.match(.Assign);
          break :blk self.expr();
        } else if (!can_default && self.opt(.Assign)) {
          printf("Can't use a default value here!\n");
          std.process.exit(1);
        } else if (can_default && !self.opt(.Assign)) {
          printf("All bindings must have a default value!\n");
          std.process.exit(1);
        } else {
          break :blk null;
        }
      },
    };
  }
  
  fn ifStmt(self: *Parser) *Stmt {
    var res = IfStmt.init(self.alloc);
    
    _ = self.match(.If);
    res.ifStmt.conds.append(self.ifCondBlock());
    while(self.tryMatch(.ElIf)) res.ifStmt.conds.append(self.ifCondBlock());
    
    if(self.tryMatch(.Else)) res.ifStmt.default = self.block(true, false);
    if(self.tryMatch(.Finally)) res.ifStmt.finally = self.block(true, false);
    return res;
  }

  fn ifCondBlock(self: *Parser) IfCondBlock {
    var cond = self.expr();
    var capture_token: ?*BindStmt = null;
    if(self.tryMatch(.Colon)) {
      _ = self.match(.BitOr);
      capture_token = self.bindStmt(false, false, false);
      _ = self.match(.BitOr);
    }
    var if_so = self.block(true, false);
  
    return IfCondBlock {
      .cond = cond,
      .capture_token = capture_token,
      .if_so = if_so
    };
  }

  fn loopStmt(self: *Parser) *Stmt {
    var res = self.alloc.create(Stmt);
    var first_tag = self.matchOne(.For, .While).tag;
    var expr = self.expr();

    var counter: ?*BindStmt = null;
    var capture: ?*BindStmt = null;
    if (self.tryMatch(.Colon)) {
      _ = self.match(.BitOr);
      counter = self.bindStmt(false, false, false);
      if(self.tryMatch(.Comma)) capture = self.bindStmt(false, false, false);
      _ = self.match(.BitOr);
    }

    var after: ?*Expr = null;
    if (self.tryMatch(.Colon)) {
      if (first_tag == .For) {
        printf("Continuation expr isn't supported on for!\n");
        std.process.exit(1);
      }
      
      after = self.expr();
    }

    var each = self.block();
    
    var finally: ?*Block = null;
    if (self.tryMatch(.Finally)) finally = self.block();

    res.* = Stmt {
      .loopStmt = LoopStmt {
        .variant = if (first_tag == .For) { .For } else { .While },
        .eval = expr,
        .counter = counter,
        .capture = capture,
        .after = after,
        .each = each,
        .finally = finally,
      }
    }; 
  }

  fn expr(self: *Parser) *Expr {
    // Parse a BinExpr, starting with level 0 (assign ops)
    return self.binExpr(0);
  }
  
  
  fn binExpr(self: *Parser, precedence: u32) *Expr {
    if (precedence == Tag.greatest_binary_precedence + 1) return self.unExpr();

    const ops = Tag.opsOfPrecedence(precedence);

    var res: *Expr = undefined;

    var first = self.binExpr(precedence + 1);
    if (self.optOne(ops)) { // We're more than just a single expr of lower prec
      var curOp = self.optOne(ops);
      res = self.alloc.create(Expr);
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

  fn unExpr(self: *Parser) *Expr {
    var res: *Expr = undefined;
    if(self.optOne(Tag.unary_ops) {
      var op = self.matchOne(Tag.unary_ops);
      
      res = self.alloc.create(Expr);
      res.* = Expr {
        .call = Call {
         .func = op.lexeme,
          .args = ArrayList(*Expr).init(self.alloc),
      };

      res.call.args.append(self.val());
    } else {
      res = self.val();
    }
    return res;
  }

  fn val(self: *Parser) *Expr {
    // Val is nothing but a redirect for the current token
    return switch (self.cur.tag) {
      .Case => self.caseExpr(),
      .Symbol, .IntLit, .BoolLit, .CharLit, 
      .StringLit, .LParen => self.accessExpr(), // symbol.symbol()[] and such
      else => {
        printf("Unexpected token: {}", self.cur.tag);
        std.process.exit(1);
      },
    };
  }
  
  fn caseExpr(self: *Parser) *Expr {
    self.matchAll([_]Tag{.Case, .LBrace});
    
    var res = self.alloc.create(Expr);
    res.* = Expr {
      .caseExpr = CaseExpr {
        .arms = ArrayList(Case).init(self.alloc),
        .default = undefined
      }
    };
    
    // While we have a normal case arm
    while (self.cur.tag != .Else) {
      var cond = self.expr();
      _ = self.match(.Implies);
      res.caseExpr.arms.append(Case {
        .cond = cond,
        .val = self.expr(),
      }; 
      _ = self.match(.Comma);
    }
   
     
    self.matchAll([_]Tag{.Else, .Implies});
    res.caseExpr.default = self.expr();
    _ = self.match(.RBrace); 
  }

  fn accessExpr(self: *Parser) *Expr {
    var baseSymbol = self.baseVal();
    
    var res: *Expr = undefined;
    
    if (self.optOne([_]Tag{.Dot,      
  } 

  fn baseVal(self: *Parser) *Expr {
    return switch (self.cur.tag) {
      .Symbol => self.symbolExpr(),
      .LParen => self.tuple(),
      .IntLit, .BoolLit, .CharLit, .StringLit, => self.literalExpr(),
    };  
  }

  fn opt(self: Parser, tag: Tag) bool {
    return cur.tag == tag;
  }
  fn optOne(self: *Parser, tags: []const Tag) bool {
    for (tags) |tag| {
      if(self.opt(tag))
        return true;
    } else {
      return false;
    }
  
  fn matchAll(self: *Parser, tags: []const Tag) void {
    for (tags) |tag| {
      _ = self.match(tag);
    }
  }

  fn matchOne(self: *Parser, tags: []const Tag) Token {
    for (tags) |tag| {
      if(self.opt(tag))
        return self.match(tag);
    }
    std.debug.warn("EXPECTED ONE OF {}\n", tags);
    std.process.exit(1);
  }

  // If we can match it, do it and return true.
  fn tryMatch(self: *Parser, tag: Tag) bool {
    if(self.opt(tag)) {
      self.match(tag);
      return true;
    } else {
      return false;
    }
  }

  fn match(self: *Parser, tag: Tag) Token {
    if(self.opt(tag)) {
      var res = self.cur.?;
      self.cur = self.lexer.scan();
      return res;
    } else {
      std.debug.warn("EXPECTED A {}\n", tag);
      std.process.exit(1);
    }
  }
};

//test "parser" {
//  var input =
//    \\ let i = 0;
//    \\ i = 1 + 4 * 5;
//    \\ while (true and i > 0) : (--i) {
//    \\   print("Hello world!");
//    \\   print('\n');
//    \\ }
//  ;
//
//  var lexer = Lexer{
//    .input = input,
//    .line = 0,
//    .fileID = 0,
//  };
//
//  var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);
//  var parser = Parser {
//    .alloc = &arena.allocator,
//    .lexer = lexer
//  };
//
//  var body = try parser.parse();
//}
