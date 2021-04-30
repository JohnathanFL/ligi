const StrCache = @This();

const std = @import("std");
const List = std.ArrayList;
const printf = std.debug.warn;
const Alloc = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const mem = std.mem;

const assert = std.debug.assert;

pub const ID = usize;
pub const StrID = ID;

pub const Str = []const u8;

// ======================================================================================
// Fields
// ======================================================================================
alloc: *Alloc,
// All Strs referenced in id_to_str and str_to_id are owned by alloc
id_to_str: List(Str),
str_to_id: std.StringHashMap(StrID),
// Will always start from 0 and count up 1 at a time. No backsies.
next: StrID = 0,

// ======================================================================================
// Funcs
// ======================================================================================

pub fn dump(self: StrCache) void {
    for (self.id_to_str.items) |item, i| {
        printf("{}: {s}\n", .{ i, item });
    }
}

/// alloc is expected to be some garbage collected allocator.
pub fn init(alloc: *Alloc) StrCache {
    return .{
        .alloc = alloc,
        .id_to_str = List(Str).init(alloc),
        .str_to_id = std.StringHashMap(StrID).init(alloc),
    };
}

pub fn resolve(self: *const StrCache, id: StrID) Str {
    return self.id_to_str.items[id];
}
pub fn intern(self: *StrCache, str: Str) !StrID {
    if (self.str_to_id.get(str)) |id| {
        return id;
    } else {
        const s = try self.alloc.dupe(u8, str);
        return self.insert(s);
    }
}
pub fn insert(self: *StrCache, str: Str) !StrID {
    const id = self.nextID();
    try self.str_to_id.putNoClobber(str, id);
    try self.id_to_str.append(str);
    return id;
}

/// Concatenate the strings referenced by the ids to form a new token
pub fn concat(self: *StrCache, ids: []const StrID) !StrID {
    return try self.join("", ids);
}

/// Join the strings referenced by the ids together to form a new interned string,
/// placing joiner in between each instance.
pub fn join(self: *StrCache, joiner: Str, ids: []const StrID) !StrID {
    // The length of the eventual string
    var n = joiner.len * ids.len;
    for (ids) |id| n += self.resolve(id).len;

    const new = try self.alloc.alloc(u8, n);
    var i = 0;
    for (ids) |id| {
        const s = self.resolve(id);
        mem.copy(u8, new[i], s);
        i += s.len;
    }

    return self.insert(new);
}

fn nextID(self: *StrCache) StrID {
    const id = self.next;
    self.next += 1;
    return id;
}

// ======================================================================================
// C ABI wrappers - intended for calling from Ligi
// ======================================================================================
const C = @import("C.zig");

pub fn str_intern(self: *StrCache, str: C.Slice) callconv(.C) StrID {
    const slice = str.ptr[0..str.len];
    return self.intern(slice) catch unreachable;
}

pub fn str_resolve(self: *const StrCache, id: StrID) callconv(.C) C.Slice {
    const res = self.resolve(id);
    return .{ .ptr = res, .len = res.len };
}

// ======================================================================================
// Tests
// ======================================================================================

test "Basic Caching" {
    var cache_arena = ArenaAllocator.init(std.heap.page_allocator);
    defer cache_arena.deinit();
    var str_arena = ArenaAllocator.init(std.heap.page_allocator);
    var cache = StrCache.init(&cache_arena.allocator);

    const original_strs = [_]Str{
        "Hello, world!",
        "blah",
        "2",
        "",
        " ",
    };

    var strs: [original_strs.len]Str = undefined;
    for (original_strs) |str, i| strs[i] = try str_arena.allocator.dupe(u8, str);

    for (strs) |str, i| {
        // Intern all our strings
        var id = try cache.intern(str);
        // The ids must start from 0 and count up by one each time
        assert(i == id);
        // Make sure it actually interned properly
        assert(mem.eql(u8, str, cache.resolve(id)));
    }

    // Make sure it properly persisted even after other strings were interned
    for (strs) |str, i|
        assert(mem.eql(u8, str, cache.resolve(i)));

    // Make sure the cache owns its own memory
    str_arena.deinit();
    for (original_strs) |str, i|
        assert(mem.eql(u8, str, cache.resolve(i)));
}

test "Same is same" {
    var cache_arena = ArenaAllocator.init(std.heap.page_allocator);
    defer cache_arena.deinit();
    var str_arena = ArenaAllocator.init(std.heap.page_allocator);
    var cache = StrCache.init(&cache_arena.allocator);

    var id1 = try cache.intern("blah");
    var id2 = try cache.intern("blah");
    assert(id1 == id2);
}
