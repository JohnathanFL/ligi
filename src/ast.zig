const std = @import("std");
const clamp = std.math.clamp;
const Alloc = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const List = std.ArrayList;
const Dictionary = std.AutoHashMap;
const mem = std.mem;
const printf = std.debug.warn;

const assert = std.debug.assert;

const StrCache = @import("StrCache.zig");
const Str = StrCache.Str;
pub const StrID = StrCache.StrID;
pub const TypeID = StrCache.StrID;
pub const FnID = StrID;

// All allocations in the AST are expected to be either garbage collected or intended to
// outlive the AST object it's in.
// To that end, we'll use a global Boehm alloc for this.
pub const alloc = @import("boehm.zig").allocator;
pub const commons = @import("common_ast.zig");
pub const initCommons = commons.initCommons;
pub const ids = commons.ids_ar[0..];

pub const CompoundKind = enum { Tup, Array, Block };

pub const EvalFn = fn (*Atom, *Context) void;

pub const Atom = union(enum) {
    word: StrID,
    /// list[0] always exists and is the command
    tag: StrID,
    /// list[0] is always the command (though it may be another list to eval first)
    /// This should be the only thing in Atom which owns memory. Thus, with the exception of
    /// the case of .list, Atoms can be compared with std.meta.eql.
    list: List(Atom),

    // ========================================================================
    // Natives/immediates                                                     |
    // ========================================================================

    /// An error with the parser/evaluator/typechecker to propopogate back up
    err: StrID,

    int: i64,
    real: f64,
    str: StrID,

    func: FnID,
    ty: TypeID,

    proc: EvalFn,

    fn indentMe(indent: usize) void {
        var i: usize = 0;
        while (i <= indent) {
            printf("  ", .{});
            i += 1;
        }
    }

    pub fn print(self: Atom, cache: *StrCache, indent: usize) void {
        switch (self) {
            .word => |w| printf("{s}", .{cache.resolve(w)}),
            .tag => |t| printf("#{s}", .{cache.resolve(t)}),
            .list => |l| {
                const is_block = l.items[0] == .word and l.items[0].word == commons.resolveCommon(.ibBlock);
                printf("( ", .{});
                for (l.items) |item| {
                    if (is_block) {
                        printf("\n", .{});
                        indentMe(indent);
                    }
                    item.print(cache, indent + 1);
                    printf(" ", .{});
                }
                printf(")", .{});
            },
            .err => |e| printf("@@err`{s}`@@", .{cache.resolve(e)}),
            .int => |i| printf("{}", .{i}),
            .real => |r| printf("{}", .{r}),
            .str => |s| printf("{}{}{}", .{ '"', s, '"' }),
            .func => |id| printf("@@func`{}`@@", .{id}),
            .ty => |id| printf("@@ty`{}`@@`", .{id}),
            .proc => |p| printf("@@proc`{}`@@`", .{p}),
        }
    }

    pub fn copy(self: Atom) !Atom {
        switch (self) {
            .list => |old| {
                var new = List(Atom).init(old.allocator);
                try new.appendSlice(old.items);
                return Atom{ .list = new };
            },
            else => return self,
        }
    }

    pub fn eql(lhs: Atom, rhs: Atom) bool {
        if (@tagName(lhs) != @tagName(rhs)) return false;
        switch (self) {
            .list => |l| {
                if (l.items.len != rhs.list.items.len) return false;
                for (l) |item, i| if (!item.eql(rhs.list.items[i])) return false;
                return true;
            },
            else => return std.mem.eql(Atom, lhs, rhs),
        }
    }
};

pub const Context = struct {
    parent: ?*Context,
    values: Dictionary(StrID, *Atom),
    /// List of atoms /to be evaluated/ when this context is destroyed.
    deinits: List(Atom),
};
