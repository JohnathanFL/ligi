const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;

pub const c = @cImport({
    @cInclude("gc.h");
});

fn boehmAlloc(
    self: *Allocator,
    len: usize,
    ptr_align: u29,
    len_align: u29,
    ret_addr: usize,
) Allocator.Error![]u8 {
    var n = len;
    // if (n % len_align != 0) n += len_align - (n % len_align);
    return @ptrCast([*]u8, c.GC_MALLOC(len).?)[0..len];
}

fn boehmRealloc(
    self: *Allocator,
    buf: []u8,
    buf_align: u29,
    new_len: usize,
    len_align: u29,
    ret_addr: usize,
) Allocator.Error!usize {
    // if (new_len == buf.len) return 0;
    // var n = new_len;
    // // if (n % len_align != 0) n += len_align - (n % len_align);
    // if (c.GC_realloc(buf.ptr, n) == null) return error.OutOfMemory;
    // return 0;
    if (new_len == 0) {
        c.GC_free(buf.ptr);
        return 0;
    }
    if (new_len <= buf.len) return mem.alignAllocLen(buf.len, new_len, len_align);

    const full_len = c.GC_size(buf.ptr);
    if (new_len <= full_len) {
        return mem.alignAllocLen(full_len, new_len, len_align);
    }
    return error.OutOfMemory;
}

var boehm_alloc_val = std.mem.Allocator{
    .allocFn = boehmAlloc,
    .resizeFn = boehmRealloc,
};

pub const allocator = &boehm_alloc_val;

/// TODO: Add opts
pub fn init() void {
    c.GC_init();
}

pub fn deinit() void {
    c.GC_deinit();
}

pub fn collect() void {
    c.GC_collect();
}
