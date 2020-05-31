const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");

const par = @import("parser.zig");


test "lexer tests" {
    _ = @import("lexer.zig");
}

pub fn main() !void {
  var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
  defer arena.deinit();
  var block = try par.parse(@embedFile("../example.li"), &arena.allocator);
  std.debug.warn("{}\n", .{block});
}
