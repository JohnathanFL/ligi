// Performs an in-place evaluation on the AST and transforms nodes appropriately
// Its purpose is to make the tree suitable for direct conversion to C
// (or LLVM, when we get that far)
// To keep things simple for now, I'll be ignoring returning error types.
// I'll just @panic on errors. More important to get a proof of concept going.

const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");
const Self = @This();

pub const Val = struct {
    val: *ast.Expr,
    ty: TypeID,
};

alloc: *std.mem.Allocator,
types: types.TypeDB,

pub fn init(alloc: *std.mem.Allocator) Self {
    return .{
        .alloc = alloc,
        .types = types.TypeDB.init(alloc),
    };
}

pub fn eval(self: *Self, node: *ast.Expr) Val {}

// In order of most to least lax
const ConversionKind = enum { Cmp, Implicit, Explicit };
fn canConvert(
    self: Self,
    kind: ConversionKind,
    a: Val,
    b: Val,
) bool {
    // Anything goes between those of like minds
    if (a.ty == b.ty) return true;

    const t1 = self.types.getTypeInfo(a);
    const t2 = self.types.getTypeInfo(b);

    // Going into this, we already know the two are NOT the same type
    switch (t1) {
        // Since we don't allow storing sinks, we can only "convert" if it's a cmp
        .Sink => return kind == .Cmp,
        // Can only do void<->void
        .Void => return false,
        // No C-styled "==1?" styled stuff. Do that with a function.
        .Bool => return false,
        .AnyEnum => {},
    }
}
