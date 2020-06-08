const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const Pretty = @import("Pretty.zig");
const eval = @import("evaluation.zig");

test "lexer tests" {
    _ = @import("lexer.zig");
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var block = try Parser.parse(@embedFile("../example.li"), &arena.allocator);
    Pretty.print(block, 0);
    var c: eval.Context = undefined;
}
