const std = @import("std");
const StrHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;
const Hash = std.Hash;
const ArrayList = std.ArrayList;
const SegList = std.SegmentedList;

const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Expr = ast.Expr;

const Name = []const u8;

// Conventions:
//   Locations can have their values replaced. Thus...
//   Values are always pass by value.
//

// A "Context" is a scope which contains bindings of names -> Locations
pub const Context = struct {
    alloc: *Allocator,
    parent: ?*Context,
    // Checked before checking .binds
    // Used for hooking into things like u8/u9/u10/etc
    // If the function returns null, it goes on to the next hook
    // If it returns a Binding, then that's used as the binding for that name
    pub const BindHook = *fn (*Context, Name) ?*Binding;
    bind_hooks: ArrayList(BindHook),
    bind_names: StrHashMap(*Binding),
    pub_binds: StrHashMap(*Binding),
    binds: SegList(Binding, 16),
    // All get executed when leaving a context
    deferred: ArrayList(*Expr),

    pub fn init(alloc: *Allocator, parent: ?*Context) Context {
        return .{
            .alloc = alloc,
            .parent = parent,
            .bind_hooks = ArrayList(BindHook).init(alloc),
            .bind_names = StrHashMap(*Binding).init(alloc),
            .binds = SegList(Binding, 16).init(alloc),
            .deferred = ArrayList(*Expr).init(alloc),
        };
    }

    pub fn deinit(this: *@This()) !void {
        for (this.deferred.items) |expr| {
            const res = try this.eval(expr);
            if (res != .Void) {
                std.debug.warn("Error: Got a non-void in defer.");
                return error.NonVoidDefer;
            }
        }
        this.deferred.deinit();
    }

    pub fn eval(this: *@This(), expr: *Expr) !Val {
        return .{ .Void = .{} };
    }
};

pub const Binding = struct {
    shadowable: bool,
    ty: TypeId,
    val: *Val,
};

// A "Val" is something
pub const Val = union(enum) {
    Void: void,
    Ast: Expr,

    // Since the `type` type to be passed around by the user is a typeid,
    // this maps directly to the @typeOf function/method/property
    pub fn typeOf(this: Val) TypeId {
        switch (this) {
            .Void => TypeDB.VOID_ID,
            .Bool => TypeDB.BOOL_ID,
            .Sink => TypeDB.SINK_ID,
            .Ast => TypeDB.AST_ID,
            _ => @panic(""),
        }
    }
};

pub const TypeId = usize;
// Global singleton for TypeId <-> TypeInfo
// Existing types are immutable. May only add new types
pub const TypeDB = struct {
    var alloc: *Allocator = undefined;
    var list: ArrayList(TypeInfo) = undefined;

    // Hash of arg-types + ret-type to the typeid of the function
    var funcs: AutoHashMap(Hash, TypeId) = undefined;
    // Hash of all inner types to the typeid of the overall tuple
    var tuples: AutoHashMap(Hash, TypeId) = undefined;

    pub fn init(all: *Allocator) void {
        alloc = all;
        list = ArrayList(Type).init(alloc);
    }

    // 4096 picked soley because it's big and a pretty number.
    pub const MAX_HARDCODED = 4096;
    pub const VOID_ID = 0;
    pub const F16_ID = 2051;
    pub const F32_ID = 2052;
    pub const F64_ID = 2053;
    pub const F128_ID = 2054;
    pub const BOOL_ID = 2055;
    pub const SINK_ID = 2056;
    pub const AST_ID = 2057;
    // Note: This table is to be considered *the* source of truth for
    // typeids. It is not kept in sync with anything else.
    // Other things are kept in sync with it.
    pub fn get(id: TypeId) TypeInfo {
        switch (id) {
            0 => return .{ .Void = .{} },

            // Hardcoded ones
            // Ints
            1...1024 => return .{ .Int = .{ .signed = false, .bits = id } },
            1025...2048 => return .{ .Int = .{ .signed = true, .bits = id - 1024 } },
            2049 => return .{ .Int = .{ .signed = false, .bits = null } },
            2050 => return .{ .Int = .{ .signed = true, .bits = null } },
            // Floats
            2051 => return .{ .Float = .F16 },
            2052 => return .{ .Float = .F32 },
            2053 => return .{ .Float = .F64 },
            2054 => return .{ .Float = .F128 },
            // Others
            2055 => return .{ .Bool = .{} },

            2056 => return .{ .Sink = .{} },

            2057 => return .{ .Ast = .{} },

            2058 => return .{ .AnyEnum = .{} },

            // Dynamic
            else => {
                if (id < MAX_HARDCODED) {
                    @panic("INVALID HARDCODED ID");
                } else return list.at(id - MAX_HARDCODED);
            },
        }
    }
};

/// Immutable info structure
/// This will be the structure that's exposed to the user when they do .@typeInfo, so also
/// no complex datatypes (HashMap, etc).
/// Any slices are expected to be allocated by TypeDB.alloc
pub const TypeInfo = union(enum) {
    // Can't read or write
    Void: void,
    // If !bits, then it's a *size
    Int: struct { signed: bool, bits: ?usize },
    Float: enum { F16, F32, F64, F128 },
    Bool: void,

    // Can write. All comparison reads are true.
    // Should never be stored anywhere long-term. (See eventual proposal for patterns)
    // Just used for stuff like ` if x == #Tag(val1, _)`
    Sink: void,

    // A node of the actual AST
    // TODO: Should this even be a builtin type? At least once we're self-hosting...
    Ast: void,

    AnyEnum: void,

    // Types that depend on other types

    Func: struct { args: []const TypeId, ret: TypeId },
    // Each TypeId is a Func that the overload contains
    Overload: []const TypeId,

    Opt: TypeId,
    Ptr: TypeId,
    Ref: TypeId,
    Const: TypeId,
    Comptime: TypeId,
    Slice: TypeId,
    Array: struct {
        // TODO: Specify this
        // indexer: TypeId,
        size: usize,
        inner: TypeId,
    },
    // Only the primitive tuple. For a compound-tuple, see #User
    Tuple: []const TypeId,

    // Either struct or enum, since they're both `user` types
    // Enum "fields" are just discriminants. I'd use `field` for that in the language
    // too, if I thought it'd be readable.
    // This also includes compound-tuples
    User: struct {
        kind: enum { Struct, Enum, Distinct },
        /// If this User-type came from another type (like usize + struct{...}),
        /// then it can also be converted to/from that type. Mark as such.
        from: ?TypeId,
        /// Using a [] instead of StrHashMap because order matters
        /// Also, there should be few enough fields/etc that the search cost should be trivial
        /// If it's an enum, this should be kept in asc. sorted order
        /// Compound-tuples just use [.name = (0, 1, 2, 3...)] and always have name.len > 1
        fields: []const struct { name: Name, ty: TypeId, init: *Expr },
        /// Used to store statics
        /// May not have deferred (so far)
        ctx: *Context,
    },
};
