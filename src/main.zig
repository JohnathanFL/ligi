const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

test "module tests" {
  _ = @import("lexer.zig");
  _ = @import("parser.zig");
}



pub fn main() anyerror!void {
  // var a = .{1, 2, 3};
}