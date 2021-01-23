const std = @import("std");
const clamp = std.math.clamp;
const Alloc = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const List = std.List;
const Dictionary = std.AutoHashMap;
const mem = std.mem;
const printf = std.debug.warn;

const assert = std.debug.assert;

const StrCache = @import("StrCache.zig");
const Str = StrCache.Str;
const StrID = StrCache.StrID;

// All allocations in the AST are expected to be either garbage collected or intended to
// outlive the AST object it's in.
// To that end, we'll use a global Boehm alloc for this.
pub const alloc = @import("boehm.zig").allocator;
pub const commons = @import("common_ast.zig");
pub const initCommons = commons.initCommons;
pub const ids = commons.ids_ar[0..];

pub const CompoundKind = enum { Tup, Array, Block };

pub const Atom = union(enum) {
    word: StrID,
    str: StrID,
    /// list[0] always exists and is the command
    tag: StrID,
    list: List(Atom),
    /// TODO: Holds native types like strings, ints, floats, etc
    native: union(enum) {},
};

pub const Context = struct {
    parent: ?*Context,
    values: Dictionary(StrID, *Atom),
    /// List of atoms /to be evaluated/ when this context is destroyed.
    deinits: List(Atom),
};
