const std = @import("std");
const Alloc = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const mem = std.mem;

const isSpace = std.ascii.isSpace;
const isAlNum = std.ascii.isAlNum;

const assert = std.debug.assert;

const StrCache = @import("StrCache.zig");
const StrID = StrCache.StrID;
const Lexer = @import("Lexer.zig");

test "_" {
    _ = @import("StrCache.zig");
    _ = @import("Lexer.zig");
}

pub fn main() anyerror!void {}
