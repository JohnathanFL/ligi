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


  pub fn parse(self: *Parser) !List {
    self.cur = self.lexer.scan();
    return self.stmtList();
  }

  fn stmtList(self: *Parser) !List {
    var res = List.init(self.alloc);

    while(self.cur) |cur| {
      if(cur.tag == .RBrace) break;
      // If we're in here, self.cur != null
      switch(self.cur.?.tag) {
        .Let, .Var => try res.items.append(Value{.List = try self.letVarExpr()}),
        else => {
          try res.items.append(try self.expr());
        }
      }
      
    }

    return res;
  }

  // Merged into one since they're the exact same other than tags
  fn letVarExpr(self: *Parser) !List {
    var res = List.init(self.alloc);
    res.op = Op.fromTag(self.matchOne([_]Tag{.Var, .Let}).tag);

    try res.items.append(Value{.Token = self.match(.Symbol)});

    if(self.opt(.Colon)) { // We've got ourselves a type
      _ = self.match(.Colon);
      try res.items.append(Value{.Token = self.match(.Symbol)});
    } else {
        try res.items.append(Value.Null);
    }

    
    _ = self.match(.Assign);

    try res.items.append(try self.expr());
    
    _ = self. match(.Semicolon);

    return res;
  }

  fn expr(self: *Parser) !Value {
    return error.NotImplemented;
  }

  fn opt(self: Parser, tag: Tag) bool {
    if(self.cur) |cur| {
      return cur.tag == tag;
    } else {
      return false;
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
