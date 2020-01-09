const std = @import("std");
const SegmentedList = std.SegmentedList;
const ArrayList = std.ArrayList;

const tokens = @import("tokens.zig");

pub const Block = struct {
    ty: *Expr,
    interpret: enum {
        /// {}
        Direct,
        /// #{} or block{}
        Unchecked,
        /// structdef{}
        StructDef,
        /// enumdef{}
        EnumDef,
    },
    children: ArrayList(*Stmt),
};

/// Binds are the only thing that can't potentially return something
pub const Stmt = union(enum) {
    Expr: *Expr,
    Bind: *Bind,
};

pub const Expr = union(enum) {
    Call: *Call,
    If: *If,
    Loop: *Loop,
    Block: *Block,

    Compund: *CompoundLiteral,
    EnumLit: Token,
    
    Literal: Token,
    Symbol: Token,
};


pub const Bind = struct {
    interpret: enum {
      Let,
      Var,
      CVar,
      Field,
      Property,
      /// TODO
      Alias,
    },
    /// len > 1 if it's a tuple unbind.
    /// let (i) === Bind { i }
    /// let i, j, k === Bind {i}, Bind{j}, Bind{k}
    /// let (i, j, k) === Bind{i, j, k}
    /// let ((i, j), k) === Bind{Bind{i, j}, k}
        location: SegmentedList(union(enum) {
        Single: Token,
        Tuple: *Bind,
    }),
    ty: ?*Expr,
    init: ?*Expr,
    public: bool,
};

pub const Call = struct {
    /// Needs to be re-resolved when needed
    func: Token,
};

pub const Range = union(enum) {
    Signed: struct {
        min: isize,
        max: isize
    },
    Unsigned: struct {
        min: usize,
        max: usize,
    }
};

pub const Type = union(enum) {
    /// Resolved from a Symbol
    /// cvars and comptime-known lets can be referenced from any scope, so this works.
    Bind: *Bind,
    // Cannot exist at this level, as a Type only exists post Evaluator, which resolves this.
    //Symbol: Token,

    /// Signed integer containing however many bits
    Int: usize,
    /// Unsigned integer containing however many bits
    UInt: usize,
    /// Floating number containing however many bits
    Float: usize,
    Char: void,
    Bool: void,
    Slice: *Type,
    Array: struct {
        /// 0 iff needs to be deduced.
        size: usize,
        members: *Type,
    },
    /// Resolves from a tuple of types
    Tuple: ArrayList(*Type),
    /// A signed/unsigned number between min..=max
    /// Always a usize/isize
    Range: Range,
    StructDef: struct {
        /// Do fields have no padding between them?
        pack: bool,
        /// Does layout perfectly match how C would do it?
        /// TODO: Make extern a binary or unary operator so we can do
        /// `"C" extern structdef...`
        external: bool,
        /// Stores lets/vars *and* properties, as properties are just functions.
        statics: ArrayList(*Bind),
        /// TODO: I should really make a new internal structure for fields
        fields: ArrayList(*Bind),
    },
    EnumDef: struct {
        // packed/external not needed here as unions will take on their constituent properties
        // packed/extern operators will merely ensure all enumerations follow those properties.
        /// What stores the tag itself?
        tagType: *Type,
        /// Same as struct defs, stores lets/vars/properties
        statics: ArrayList(*Bind),
        /// TODO: Like structs, I should make a new internal structure for enum tags
        enums: ArrayList(*Bind),
    },
};

pub const Loop = struct {
    ty: *Expr,
    interpret: union(enum) {
        Infinite: void,
        For: *Expr,
        While: *Expr,
    },
    capture: struct {
        counter: ?*Bind,
        /// Guarenteed to be null if interpret == .Infinite
        value: ?*Bind,
    },
    do: *Block,
};
