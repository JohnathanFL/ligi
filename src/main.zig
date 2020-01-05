const std = @import("std");

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
    _ = @import("typechecker.zig");
    _ = @import("evaluator.zig");
    _ = @import("compiler.zig");
}

pub fn main() anyerror!void {
    std.debug.warn("All your base are belong to us.\n");
}
