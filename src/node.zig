const std = @import("std");
const ArrayList = std.ArrayList;
const SegmentedList = std.SegmentedList;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;

const Tokens = @import("token.zig");
const Token = Tokens.Token;
const Tag = Tokens.Tag;
const FilePos = Tokens.FilePos;

pub const Action = enum { Read, Write };

pub const Block = struct {
    pos: FilePos,
    pub const Interpret = enum {
      /// This block must be evaluated to create a new type
      TypeDef,
      /// Don't interpret or even type check at all.
      /// Signaled with 'block {' at the moment
      Macro,
      /// Just evaluate everything at runtime like normal
      Normal, 
    };
    parent: ?*Block,
    interpret: Interpret,
    children: ArrayList(*Expr),
    label: ?Token,
    external: struct {
        reads: ArrayList(*Bind),
        writes: ArrayList(*Bind),
    },

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .parent = null,
            .pos = undefined,
            .interpret = .Block,
            .stmts = ArrayList(*Stmt).init(alloc),
            .label = null,
            .external = .{
                .reads = ArrayList(*Bind).init(alloc),
                .writes = ArrayList(*Bind).init(alloc),
            }
        };
    }
};

pub const Expr = union(enum) {
    ifExpr: *If,    
    block: *Block,
    call: *Call,
    literal: Token,
    struct_lit: *StructLiteral,
    array_lit: *ArrayLiteral,
    symbol: Token,
    undef: void,
    fnDef: *Fn,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .block = undefined,
        };
    }

    pub fn getType(self: Expr) ?*Type {
        switch(self) {
            .ifExpr => |i| return i.ty,
            .block => |b| return b.ty,
            .call => |c| return c.ty,
            .literal => |l| {
              if (l.tag == .IntLit)
                
        }
    }
};

pub const If = struct {
    pub const Arm = struct {
        cond: *Expr,
        then: *Block,
        capture: ?*Bind,
    };

    pos: FilePos,
    ty: *Type, // The type this expr returns
    arms: ArrayList(Arm),
    default: ?*Block,
    finally: ?*Block,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        const dummy: If = undefined;
        return .{
            .pos = undefined,
            .arms = @typeOf(dummy.arms).init(alloc),
            .default = null,
            .finally = null,
        };
    }
};

pub const Loop = struct {
    pos: FilePos,
    ty: *Type, // Returned either in the main loop or from the finally
    interpret: enum {
        Infinite,
        For,
        While,
    },

    /// Must be present if interpret != Infinite
    expr: ?*Expr,
    capture: struct {
        index: ?*Bind,
        variable: ?*Bind,
    },

    /// Expr to allow assignment while disallowing more if/etc structures.
    cont: ?*Expr,
    body: *Block,
    finally: ?*Block,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pos = undefined,
            .interpret = .Infinite,
            .expr = null,
            .capture = .{
                .index = null,
                .variable = null,
            },
            .cont = null,
            .body = undefined,
            .finally = null,
        };
    }
};

pub const Bind = struct {
    pub const Interpret = enum {
        /// Constant
        Let,
        /// Mutable
        Var,
        /// Instance-level mutable
        Field,
        /// Possible form of an enum
        Enum,
        /// get/set shortcut
        Property,
        /// Function output. Mutable.
        Out,
        pub fn fromTag(tag: Tag) @This() {
            return switch (tag) {
                .Let => .Let,
                .Var => .Var,
                .Field => .Field,
                .Enum => .Enum,
                .Property => .Property,
                else => @panic("fromTag with non specifier!"),
            };
        }
    };
    pos: FilePos,
    interpret: Interpret,
    loc: Token,
    ty: ?*Type,

    /// If interpret === Let,Var, then default != null
    default: ?*Expr,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pos = undefined,
            .interpret = .Let,
            .loc = undefined,
            .ty = null,
            .default = null,
        };
    }
};

pub const Break = struct {
    pos: FilePos,
    label: ?Token,
    val: ?*Expr,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pos = undefined,
            .label = null,
            .val = null,
        };
    }
};
//=================================================================================================
// Expr types
//=================================================================================================

/// All operators are also compiled down to a call.
pub const Call = struct {
    pos: FilePos,

    /// Are we eligible for overloads?
    operator: bool,

    /// What are we calling?
    func: Token,

    /// Since the majority of these are likely to be binary/unary expressions or function calls with
    /// few args, prealloc 2 just in case.
    args: ArrayList(*Expr),

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pos = undefined,
            .operator = false,
            .func = undefined,
            .args = ArrayList(*Expr).init(alloc),
        };
    }
};

pub const StructLiteral = struct {
    pos: FilePos,
    vals: StringHashMap(*Expr),
    ty: ?*Type,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pos = undefined,
            .vals = StringHashMap(*Expr).init(alloc),
            .ty = null,
        };
    }
};

pub const ArrayLiteral = struct {
    pos: FilePos,
    vals: ArrayList(*Expr),
    ty: ?*Type,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pos = undefined,
            .vals = ArrayList(*Expr).init(alloc),
            .ty = null,
        };
    }
};

pub const IfExpr = struct {
    pub const Arm = struct {
        cond: *Expr,
        capture: ?*Bind,
        val: *Expr,
    };
    pos: FilePos,
    arms: ArrayList(Arm),
    default: *Expr,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pos = undefined,
            .arms = ArrayList(Arm).init(alloc),
            .default = undefined,
        };
    }
};

pub const Fn = struct {
    pos: FilePos,
    pure: bool,
    inlined: bool,
    
    args: ArrayList(*Bind),

    /// If null, then it returns void.
    ret: ?*Bind,
    body: *Block,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pos = undefined,
            .pure = false,
            .args = ArrayList(*Bind).init(alloc),
            .ret = null,
            .body = undefined,
        };
    }
};

//=================================================================================================
// Type types
//=================================================================================================

pub const Type = union(enum) {
    /// Only pre-evaluator stage
    expr: *Expr,
    /// Should only be present in Type.Void
    voidType: void,
    consumed: void,
    int: struct {
        signed: bool,
        bits: usize
    },
    boolean: void,
    pointer: struct {
      // Reserving the right to add more attributes here later
      inner: *Type,
    },
    optional: struct {
      inner: *Type,
    },
    structDef: struct {
        isPacked: bool,
        isExtern: bool,
        static: ArrayList(*Bind),
        fields: ArrayList(*Bind),
    },
    enumDef: struct {
        isExtern: bool,
        static: ArrayList(*Bind),
        enums: ArrayList(*Bind),
        /// Only needed if discriminators.len > 0
        tagType: *Type,
    },
    // A function declaration without an accompanying block
    fnType: struct {
        isExtern: bool,
        /// Whether it *must* be pure.
        isPure: bool,
        // No point in the type being inlined. Each declaration decide that
        //isInline: bool,
        args: ArrayList(*Bind),
        returnVal: ?*Bind,
    },

    pub const Void: *Type = &.{ .voidType = .{} };
    pub const Consumed: *Type = &.{  
    
    // Concepts will just use structs. The 'concept' keyword will just be a sugar
    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .base = .{
                .symbol = Token{
                    .lexeme = "@@NOTYPE@@",
                    .tag = .Symbol,
                    .pos = .{
                        .file = 0,
                        .line = 0,
                        .col = 0,
                    },
                },
            },
        };
    }
};
