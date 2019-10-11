const std = @import("std");

const LexerMod = @import("lexer.zig");
const Lexer = LexerMod.Lexer;
const Token = LexerMod.Token;
const Tag = LexerMod.Tag;
const LexVal = LexerMod.LexVal;

pub usingnamespace @import("node.zig");

pub const Parser = struct {
  lexer: Lexer,
  cur: ?Token,

  alloc: *std.mem.Allocator,



  pub fn parse(alloc: *std.mem.Allocator, lexer: Lexer) StmtBody {
    var parser = Parser {
      .lexer = lexer,
      .alloc = alloc,
      .cur = undefined,
    };
    parser.cur = parser.lexer.scan();

    return parser.stmtBody();
  }
};

test "parser" {
  var input =
    \\ int i = 0;
    \\ i = 1 + 4 * 5;
    \\ while (true and i > 0) : (--i) {
    \\   print("Hello world!");
    \\   print('\n');
    \\ }
  ;

  var lexer = Lexer{
    .input = input,
    .line = 0,
  };

  var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);
  var body = Parser.parse(&arena.alloc, lexer);
}