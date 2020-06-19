const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const Pretty = @import("Pretty.zig");
const eval = @import("evaluation.zig");

test "lexer tests" {
    _ = @import("lexing.zig");
}

fn sentToSlice(s: [*:0]const u8) []const u8 {
    var cur = s[0];
    var length: usize = 0;
    while (cur != 0) {
        length += 1;
        cur = s[length];
    }
    return s[0..length];
}

pub fn main() !void {

    // For now at least, I'm only going to target Linux/POSIX for the compiler.
    // Honestly, with WSL, I'm not sure I even see a need to ever target Windows for a compiler
    const args = std.os.argv;
    for (args[1..]) |arg_sent| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const arg = sentToSlice(arg_sent);
        std.debug.warn("\nFile: {}\n", .{arg});

        var file = try std.fs.cwd().openFile(arg, .{});

        var file_buf = try arena.allocator.alloc(u8, (try file.stat()).size);
        if ((try file.readAll(file_buf)) != file_buf.len) @panic("Stat and readAll differed?");

        var block = try Parser.parse(file_buf, &arena.allocator);
        Pretty.print(block, 0);
        var c: eval.Context = undefined;
    }
}
