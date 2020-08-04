// Performs an in-place evaluation on the AST and transforms nodes appropriately
// Its purpose is to make the tree suitable for direct conversion to C
// (or LLVM, when we get that far)
// To keep things simple for now, I'll be ignoring returning error types.
// I'll just @panic on errors. More important to get a proof of concept going.

const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");
const Self = @This();
const TypeID = types.TypeID;

const str = []const u8;
const Label = str;

pub const Val = struct {
    // This val is guaranteed to be an actual value, like:
    // Type, EnumLit, Tuple, Struct, Array, Str, FuncID
    val: *ast.Expr,
    ty: TypeID,
};

pub const Res = union(enum) {
    Break: struct {
        from: ?Label,
        with: ?Val,
    },
    Return: ?Val,
    Continue: ?Label,
    Val: Val,
};

pub const NOP_AST = ast.Expr{ .NOP = .{} };
pub const VOID = Res{
    .Val = .{
        .val = &NOP_AST,
        .ty = types.TypeDB.VOID_ID,
    },
};

alloc: *std.mem.Allocator,
types: types.TypeDB,
// The current type we expect, or 0 for "anything goes"
expected: TypeID,
// types' ID for a `slice const u8`
str_id: TypeID,

pub fn init(alloc: *std.mem.Allocator) Self {
    var res = Self{
        .alloc = alloc,
        .types = types.TypeDB.init(alloc),
        .expected = types.VOID_ID,
        .str_id = undefined,
    };
    res.str_id = res.types.sliceID(res.types.constID(types.uint_id(8)));
    return res;
}

// Evaluates the node and updates internal state while returning the result
pub fn eval(self: *Self, node: *ast.Expr) Res {
    switch (node.*) {
        .Call => |call| return self.evalCall(call),
        .Block => |block| return self.evalBlock(block),
        .Nop => return VOID,

        .Continue => |label| return .{ .Continue = label },
        .Break => |brak| return .{
            .from = brak.from,
            .with = if (brak.with) self.eval(brak.with) else null,
        },
        .Return => |val| return .{ .Return = self.eval(val) },

        .Expansion => todo("Expansions"),
        .Word => todo("Need to lookup the word in the stack!"),
        .Array => todo("Array type handling"),
        .EnumLit => |lit| return self.evalEnumLit(lit),

        .Assert => |assert| {
            const res = self.eval(assert);
            if (res != .Val) @panic("An assert must yield a value!");
            if (res.Val != .Bool) @panic("An assert must yield a boolean!");
        },

        // The pure value types
        .Tuple => |tup| return self.evalTuple(tup),
        .Struct => |strct| return self.evalStruct(strct),
        .Str => return asResVal(node, self.types.sliceID()),
        .Array => |ar| return self.evalArray(ar),

        else => todo("Unhandled AST node type"),
    }
}

// Keeps a cache of TypeID nodes, that way we don't create new ones all the time.
pub fn typeIDVal(self: *Self, id: TypeID) *ast.Expr {}
pub fn evalAsTypeID(self: *Self, node: *ast.Expr) types.TypeID {
    return self.evalAsType(node).Type;
}

// Assumes we cannot break/return/etc, as you cannot when
// evaluating a `:type` expression
pub fn evalAsType(self: *Self, node: *ast.Expr) Val {
    const old_expected = self.expected;
    defer self.expected = old_expected;
    self.expected = self.types.TYPE_ID;
    const res = self.eval(node);
    if (res != .Val) @panic("Expected a type!");
    return res.Val;
}

pub fn evalArray(self: *Self, ar: *ast.Array) Res {
    todo("Array lits");
}
pub fn evalStruct(self: *Self, ar: *ast.Struct) Res {
    todo("Struct lits");
}
pub fn evalFunction(self: *Self, ar: *ast.Func) Res {
    todo("Function lits");
}

fn todo(comptime msg: str) noreturn {
    @panic("TODO: " ++ msg);
}

fn asResVal(node: *ast.Expr, ty: TypeID) Res {
    return .{ .Val = .{ .val = node, .ty = ty } };
}

// We only need to support absolute primitives.
// Everything else, including anyenum<->enum and the like, can be handled in
// userspace (or at least by writing builtin code)
