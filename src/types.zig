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
    Property,

    Ref,
    Ptr,
    Type,

    Tuple,

    Ext,
    Struct,
    Enum,
    Array,
    Slice,
};

pub const FloatType = enum { F32, F64 };

pub const Static = union(enum) {
    Func: FuncID,
    Var: union(enum) {
        Var: TypeID,
        Property: Property,
    },
};

pub const Property = struct {
    get: ?FuncID,
    set: ?FuncID,
    /// The below are only tentative for now, but you get the idea. Properties
    /// will be for more than just get/set. e.g you could have array.rev to index/iter in reverse
    iter: ?FuncID,
    iter_mut: ?FuncID,
};

/// This is the structure that will be directly exposed as @typeinfo
/// Must be trivially copyable and immutable
/// Statics are stored in another list (indexed by TypeID), as anything can have them. See the Static type
pub const TypeInfo = union(TypeType) {
    Undef,
    Sink,
    Void,
    Bool,
    Type,
    AnyEnum,
    AnyFunc,
    Ast: void,
    Ptr,
    Ref: TypeID,
    Slice: TypeID,
    Int: struct { signed: bool, bits: usize },
    Float: FloatType,
    Func: FuncType,
    Tuple: []const TypeID,
    Array: struct { size: usize, ty: TypeID },
    /// For example, the builtin str type is `slice const u8 + struct {...}`
    /// There, the inner is `slice const u8`, and the
    /// Any type arithmetic where one side is a primitive (including tuples) is an Ext.
    /// Any type arithmetic where one side adds no fields is an Ext.
    /// Exts may also not add new fields, only new statics.
    /// Exts may coerce into their base.
    Ext: TypeInfo,
    Struct: struct {
        /// Was this a primitive type before?
        /// If from!=null, then
        from: ?TypeID,
        fields: []const struct {
            public: bool,
            name: str,
            ty: TypeID,
            // Evaluated each time @This is instantiated without specifying this field.
            def: *ast.Expr,
        },
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
const NUM_INTS = 1024;
const U1_ID = 1;
const I1_ID = 1025;
pub fn int_id(bits: usize) TypeID {
    return I1_ID + bits - 1;
}
pub fn uint_id(bits: usize) TypeID {
    return U1_ID + bits - 1;
}
// const USIZE_ID = 2049;
// const ISIZE_ID = 2050;

pub const SINK_ID = 2051;
pub const BOOL_ID = 2052;
pub const F32_ID = 2053;
pub const F64_ID = 2054;
pub const ANYENUM_ID = 2055;
pub const ANYFUNC_ID = 2056;
pub const AST_ID = 2057;
pub const VOID_ID = 2058;
pub const TYPE_ID = 2059;

pub const TypeDB = struct {
    pub const Self = @This();
    pub const TypeMap = std.AutoHashMap(TypeID, TypeID);
    pub const SizeToTypeMap = std.AutoHashMap(usize, TypeID);
    pub const StaticMap = std.AutoHashMap(TypeID, std.StringHashMap(Static));
    alloc: *std.mem.Allocator,
    // mapped to (id - MAX_PRIM)
    types: std.ArrayList(TypeInfo),
    // Note this does not let us look up a type by name.
    // That must be done in context.
    names: std.AutoHashMap(TypeID, str),
    // inner -> count -> array(count, inner)
    arrays: std.AutoHashMap(TypeID, SizeToTypeMap),
    // Maps slice, const, comptime, etc to their type maps
    // arrays handled separately because they're op(size,type), not op(type)
    op_types: std.AutoHashMap(ast.Op, TypeMap),
    // The key is the same value that's in the TypeInfo
    tuples: TypeSetMap,
    // The arg slice in the TypeInfo is a subslice of [0..-1] of this key
    funcs: FuncTypeMap,
    // id -> name -> static{var,prop,func}
    statics: StaticMap,

    next_id: TypeID = MAX_PRIM,

    pub fn init(alloc: *std.mem.Allocator) Self {
        return .{
            .alloc = alloc,
            .types = std.ArrayList(TypeInfo).init(alloc),
            .names = std.AutoHashMap(TypeID, str).init(alloc),
            .arrays = std.AutoHashMap(TypeID, SizeToTypeMap).init(alloc),
            .tuples = TypeSetMap.init(alloc),
            .funcs = FuncTypeMap.init(alloc),
            .op_types = std.AutoHashMap(ast.Op, TypeMap).init(alloc),
            .statics = StaticMap.init(alloc),
        };
    }

    pub fn tupID(self: *Self, ty: TypeSet) TypeID {
        if (self.tuples.getValue(ty)) |id| return id;

        const id = self.nextID();
        const set = self.alloc.dupe(TypeID, ty);
        self.tuples.put(ty, id);
        return id;
    }

    fn nextID(self: *Self) TypeID {
        const id = self.next_id;
        self.next_id += 1;
        return id;
    }

    // Either gets the existing TypeID or copies the typeset and makes a new
    // TypeID if it doesn't exist.
    pub fn funcID(self: *Self, ty: TypeInfo.FuncType) TypeID {
        if (self.funcs.getValue(ty)) |id| return id;

        const id = self.nextID();
        const set = self.alloc.dupe(TypeID, set) catch unreachable;
        self.funcs.put(set, id);
        return id;
    }

    pub fn opID(self: *Self, op: ast.Op, inner: TypeID) TypeID {
        // Note that HashMap does not alloc memory until you use it, so it's safe to
        // pass a "new" HashMap each time we call this.
        const map = self.op_types.getOrPutValue(op, TypeMap.init(self.alloc)) catch unreachable;
        const slot = map.value.getOrPut(inner) catch unreachable;
        if (!slot.found_existing) slot.kv.value = self.nextID();
        return slot.kv.value;
    }

    pub fn arrayID(self: *Self, inner: TypeID, size: usize) TypeID {
        if (self.arrays.getValue(inner)) |ars| {
            if (ars.getValue(size)) |id| return id;

            const id = self.nextID();
            ars.putNoClobber(size, id) catch unreachable;
            return id;
        } else {
            const id = self.nextID();
            var map = SizeToTypeMap.init(self.alloc);
            map.putNoClobber(size, id) catch unreachable;
            self.arrays.putNoClobber(inner, map) catch unreachable;
        }
    }

    pub fn sliceID(self: *Self, inner: TypeID) TypeID {
        return self.opID(.Slice, inner);
    }
    // TODO: This currently allows "const const const foo" to be distinct from "const foo",
    // even though they are logically equivalent
    pub fn constID(self: *Self, inner: TypeID) TypeID {
        return self.opID(.Const, inner);
    }
    // TODO: See TODO on constID
    pub fn comptimeID(self: *Self, inner: TypeID) TypeID {
        return self.opID(.Comptime, inner);
    }

    // This is to be considered the sole source of truth. Nothing outside this function
    // should rely on the exact values of the IDs (at least not hardcoded)
    // Exception: #Undef can be assumed to be 0
    pub fn getTypeInfo(self: @This(), id: TypeID) TypeInfo {
        return switch (id) {
            0 => .{ .Undef = .{} },
            U1_ID...(U1_ID + NUM_INTS) => .{ .Int = .{ .signed = false, .bits = id - U1_ID + 1 } },
            I1_ID...(I1_ID + NUM_INTS) => .{ .Int = .{ .signed = true, .bits = id - I1_ID + 1 } },
            // USIZE_ID => .{ .Int = .{ .signed = false, .bits = null } },
            // ISIZE_ID => .{ .Int = .{ .signed = false, .bits = null } },
            SINK_ID => .{ .Sink = .{} },
            BOOL_ID => .{ .Bool = .{} },
            F32_ID => .{ .Float = .F32 },
            F64_ID => .{ .Float = .F64 },
            ANYENUM_ID => .{ .AnyEnum = .{} },
            ANYFUNC_ID => .{ .AnyFunc = .{} },
            AST_ID => .{ .Ast = .{} },
            VOID_ID => .{ .Void = .{} },
            TYPE_ID => .{ .Type = .{} },

            else => info: {
                if (id < MAX_PRIM) @panic("UNKNOWN PRIMITIVE TYPE");
                break :info self.types.items[id - MAX_PRIM];
            },
        };
    }
};
