const std = @import("std");
const Alloc = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const mem = std.mem;
const printf = std.debug.warn;

const isSpace = std.ascii.isSpace;
const isAlNum = std.ascii.isAlNum;

const assert = std.debug.assert;

const StrCache = @import("StrCache.zig");
const StrID = StrCache.StrID;
const Lexer = @import("Lexer.zig");

const boehm = @import("boehm.zig");

const ast = @import("ast.zig");

test "_" {
    _ = @import("StrCache.zig");
    _ = @import("Lexer.zig");
    _ = @import("Parser.zig");
}

pub fn main() anyerror!void {
    boehm.init();
    defer boehm.deinit();

    try ast.initCommons();
}
