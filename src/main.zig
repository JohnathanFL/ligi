const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const Pretty = @import("Pretty.zig");
const eval = @import("evaluation.zig");

test "lexer tests" {
    _ = @import("lexing.zig");
}

/// Sentineled to slice
fn sentToSlice(s: [*:0]const u8) []const u8 {
    var cur = s[0];
    var length: usize = 0;
    while (cur != 0) {
        length += 1;
        cur = s[length];
    }
    return s[0..length];
}

const MAX_INT = std.math.maxInt(usize);

pub fn main() !void {

    // For now at least, I'm only going to target Linux/POSIX for the compiler.
    // Honestly, with WSL, I'm not sure I even see a need to ever target Windows for the compiler
    // To me, it's enough to target Windows for the final binary.
    const args = std.os.argv;
    var input = std.ArrayList(u8).init(std.heap.page_allocator);
    defer input.deinit();

    var stdin = std.io.getStdIn().inStream();

    std.debug.warn("Argc: {}\n", .{args.len});

    if (args.len > 1) { // Compile files
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        for (args[1..]) |arg_sent| {
            const arg = sentToSlice(arg_sent);

            if (std.mem.eql(u8, arg, "-")) {
                // Read from stdin
                std.debug.warn("\nStdIn\n", .{});
                try stdin.readAllArrayList(&input, MAX_INT);
            } else {
                std.debug.warn("\nFile: {}\n", .{arg});
                var file = try std.fs.cwd().openFile(arg, .{});
                try file.inStream().readAllArrayList(&input, MAX_INT);
            }
            var block = try Parser.parse(input.items, &arena.allocator);
            Pretty.print(block, 0);
            var c: eval.Context = undefined;
        }
    } else { // REPL
        std.debug.warn("REPL\n", .{});
        while (true) {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            std.debug.warn("\n>>> ", .{});
            try stdin.readUntilDelimiterArrayList(&input, '\n', MAX_INT);
            var block = try Parser.parse(input.items, &arena.allocator);
            Pretty.print(block, 0);
            var c: eval.Context = undefined;
        }
    }
}
