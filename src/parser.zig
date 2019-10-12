const std = @import("std");

const LexerMod = @import("lexer.zig");
const Lexer = LexerMod.Lexer;
const Token = LexerMod.Token;
const Tag = LexerMod.Tag;
const LexVal = LexerMod.LexVal;

pub usingnamespace @import("node.zig");

pub const Parser = struct {
  lexer: Lexer,
  cur: ?Token = null,

  alloc: *std.mem.Allocator,


  pub fn parse(self: *Parser) !StmtBody {
    self.cur = self.lexer.scan();
    return self.stmtBody();
  }

  fn stmtBody(self: *Parser) !StmtBody {
    var res = StmtBody.init(self.alloc);
    
    while(self.cur) |cur| {
      try switch(cur.tag) {
        .If => res.append(Stmt {.IfStmt = try self.ifStmt()}),
        .While => res.append(Stmt {.WhileStmt = try self.whileStmt()}),
        .For => res.append(Stmt {.ForStmt = try self.forStmt()}),
        else => res.append(Stmt {.Expr = try self.call()}),
      };
      
    }

    return res;
  }

  fn ifStmt(self: *Parser) !IfStmt {
    return error.NotImplemented;
  }

  fn whileStmt(self: *Parser) !WhileStmt {
    return error.NotImplemented;
  }

  fn forStmt(self: *Parser) !ForStmt {
    return error.NotImplemented;
  }

  fn call(self: *Parser) !Call {
    return error.NotImplemented;
  }


  fn opt(self: Parser, tag: Tag) bool {
    if(self.cur) |cur| {
      return cur.tag == tag;
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
      std.debug.warn("EXPECTED A {}", tag);
      std.process.exit(1);
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
  var parser = Parser {
    .alloc = &arena.allocator,
    .lexer = lexer
  };

  var body = try parser.parse();
}