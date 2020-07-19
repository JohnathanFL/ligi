// This is for representing Ligi types

const std = @import("std");
const ast = @import("ast.zig");
const str = []const u8;

pub const TypeID = usize;

// Max primitive TypeID
pub const MAX_PRIM = 4096;

pub const TypeType = enum {
    Undef, // No known type yet
    Void,
    Int,
    Sink,
    Bool,
    Float,
    AnyEnum,
    AnyFunc,
    Func,
    Ast,

    Ref,
    Ptr,

    Tuple,

    Struct,
    Enum,
};

pub const FloatType = enum { F32, F64 };

/// This is the structure that will be directly exposed as @typeinfo
/// Must be trivially copyable and immutable
/// Statics are stored in another list (indexed by TypeID), as anything can have them
pub const TypeInfo = union(TypeType) {
    Undef,
    Sink,
    Void,
    Bool,
    AnyEnum: void,
    Int: struct { signed: bool, bits: usize },
    USize,
    ISize: void,
    Float: FloatType,
    Func: FuncType,
    Tuple: []const TypeID,
    Struct: struct {
        fields: []const struct { public: bool, name: str, ty: TypeID, def: *ast.Expr },
        states: []const struct { name: str, cond: *ast.Expr },
        pack: bool,
        external: bool,
    },
    Enum: struct {
        tags: []const struct {
            name: str,
            ty: TypeID,
            tag: TypeID,
            range: *ast.Expr, // what determines what it is
        },
        pub fn getTag(self: @This(), name: str) ?usize {
            for (self.tags) |tag, i| if (std.mem.eql(u8, tag, name)) return i;
            return null;
        }
        pub fn hasTag(self: @This(), name: str) bool {
            return self.getTag(name) != null;
        }
    },

    pub const FuncType = struct {
        args: []const TypeID,
        ret: TypeID,
        pure: bool,
        inlined: bool,

        pub fn hash(self: @This()) u32 {
            var adler = std.hash.Adler32.init();
            adler.update(std.mem.sliceAsBytes(self.args));
            adler.update(std.mem.asBytes(&self.ret));
            adler.update(std.mem.asBytes(&self.pure));
            adler.update(std.mem.asBytes(&self.inlined));
            return adler.final();
        }
        pub fn eql(a: @This(), b: @This()) bool {
            return std.mem.eql(TypeID, a.args, b.args) and (a.ret == b.ret) and (a.pure == b.pure) and (a.inlined == b.inlined);
        }
    };
};

pub const TypeSet = []const TypeID;

fn typesetEql(set1: TypeSet, set2: TypeSet) bool {
    if (set1.len != set2.len) return false;
    for (set1) |val, i| {
        if (val != set2[i]) return false;
    }
    return true;
}

fn typesetHash(set: TypeSet) u32 {
    return std.hash.CityHash32.hash(std.mem.sliceAsBytes(set[0..]));
}

const TypeSetMap = std.HashMap([]const TypeID, TypeID, typesetHash, typesetEql);
const FuncTypeMap = std.HashMap(
    TypeInfo.FuncType,
    TypeID,
    TypeInfo.FuncType.hash,
    TypeInfo.FuncType.eql,
);

pub const TypeDB = struct {
    pub const Self = @This();
    alloc: *std.mem.Allocator,
    // mapped to (id - MAX_PRIM)
    types: std.ArrayList(TypeInfo),
    names: std.AutoHashMap(TypeID, str),
    // The key is the same value that's in the TypeInfo
    tuples: TypeSetMap,
    // The arg slice in the TypeInfo is a subslice of [0..-1] of this key
    funcs: FuncTypeMap,
    next_id: TypeID = MAX_PRIM,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .alloc = alloc,
            .types = std.ArrayList(TypeInfo).init(alloc),
            .tuples = TypeSetMap.init(alloc),
            .funcs = FuncTypeMap.init(alloc),
            .names = std.AutoHashMap(TypeID, str).init(alloc),
        };
    }

    pub fn tupID(self: *Self, ty: TypeSet) TypeID {
        if (self.tuples.getValue(ty)) |id| return id;

        const id = self.next_id;
        self.next_id += 1;
        const set = self.alloc.dupe(TypeID, ty);
        self.tuples.put(ty, id);
        return id;
    }

    // Either gets the existing TypeID or copies the typeset and makes a new
    // TypeID if it doesn't exist.
    pub fn funcID(self: *Self, ty: TypeInfo.FuncType) TypeID {
        if (self.funcs.getValue(ty)) |id| return id;

        const id = self.next_id;
        self.next_id += 1;
        const set = self.alloc.dupe(TypeID, set) catch unreachable;
        self.funcs.put(set, id);
        return id;
    }

    // This is to be considered the sole source of truth. Nothing outside this function
    // should rely on the exact values of the IDs (at least not hardcoded)
    pub fn getTypeInfo(self: @This(), id: TypeID) TypeInfo {
        const num_ints = 1024;
        const u1_id = 1;
        const i1_id = 1025;
        const usize_id = 2049;
        const isize_id = 2050;

        const sink_id = 2051;
        const bool_id = 2052;
        const f32_id = 2053;
        const f64_id = 2054;
        const anyenum_id = 2055;
        const anyfunc_id = 2056;
        const ast_id = 2057;

        return switch (id) {
            0 => .{ .Void = .{} },
            u1_id...(u1_id + num_ints) => .{ .Int = .{ .signed = false, .bits = id - u1_id + 1 } },
            i1_id...(i1_id + num_ints) => .{ .Int = .{ .signed = true, .bits = id - i1_id + 1 } },
            usize_id => .{ .Int = .{ .signed = false, .bits = null } },
            isize_id => .{ .Int = .{ .signed = false, .bits = null } },
            sink_id => .{ .Sink = .{} },
            bool_id => .{ .Bool = .{} },
            f32_id => .{ .Float = .F32 },
            f64_id => .{ .Float = .F64 },
            anyenum_id => .{ .AnyEnum = .{} },
            anyfunc_id => .{ .AnyFunc = .{} },
            ast_id => .{ .Ast = .{} },

            else => info: {
                if (id < MAX_PRIM) @panic("UNKNOWN PRIMITIVE TYPE");
                break :info self.types.items[id - MAX_PRIM];
            },
        };
    }
};
