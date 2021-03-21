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
const StrID = StrCache.StrID;
const FnID = StrID;

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
    /// This should be the only thing in Atom which owns memory.
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
};

pub const Context = struct {
    parent: ?*Context,
    values: Dictionary(StrID, *Atom),
    /// List of atoms /to be evaluated/ when this context is destroyed.
    deinits: List(Atom),
};
