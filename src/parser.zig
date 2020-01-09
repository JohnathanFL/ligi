const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const SegmentedList = std.SegmentedList;

const tokens = @import("tokens.zig");
const Tag = tokens.Tag;
const Token = tokens.Token;


/// Each parser can make new subparsers for new files
pub const Parser = struct {
    /// Owned and deallocated by something outside any parser
    /// @embedded and @included files are placed here.
    files: *ArrayList([]const u8),


    /// Parse always assumes the file it should parse is the last in its arg
    pub fn parse(existingFiles: *ArrayList([]const u8)) *Block {
        unreachable;
    }
};


test "parser" {
    var arena = ArenaAllocator.init(std.heap.direct_allocator);
    var files = ArrayList([]const u8).init(&arena.allocator);

    
}
