const std = @import("std");
const ArrayList = std.ArrayList;
const SegmentedList = std.SegmentedList;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;

const Tokens = @import("token.zig");
const Token = Tokens.Token;
const Tag = Tokens.Tag;
const FilePos = Tokens.FilePos;



pub const Block = struct {
    pos: FilePos,
    pub const Interpret = enum {
        Struct,
        Enum,

        /// Zag's answer to generic interfaces
        Concept,

        /// Don't interpret it at all
        Stream,

        /// Completely normal mode: Just run each statement inside at runtime.
        Block,
    };
    parent: ?*Block,
    interpret: Interpret,
    stmts: ArrayList(*Stmt),
    label: ?Token,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .parent = null,
            .pos = undefined,
            .interpret = .Block,
            .stmts = ArrayList(*Stmt).init(alloc),
            .label = null,
        };
    }
};

pub const Stmt = union(enum) {
    ifStmt: *If,
    loop: *Loop,
    expr: *Expr,
    bind: *Bind,
    block: *Block,
    breakStmt: *Break,

    /// We really don't need a new structure type for a single *Expr.
    /// If it's null, we return void.
    returnStmt: ?*Expr,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .bind = undefined,
        };
    }
};

pub const Expr = union(enum) {
    block: *Block,
    call: *Call,
    literal: Token,
    struct_lit: *StructLiteral,
    array_lit: *ArrayLiteral,
    symbol: Token,
    ifExpr: *IfExpr,
    undef: void,
    fnDef: *Fn,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .block = undefined,
        };
    }
};

//=================================================================================================
// Stmt types
//=================================================================================================

pub const If = struct {
    pub const Arm = struct {
        cond: *Expr,
        then: *Block,
        capture: ?*Bind,
    };

    pos: FilePos,
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
        Let,
        Var,
        Field,
        Enum,
        pub fn fromTag(tag: Tag) @This() {
            return switch (tag) {
                .Let => .Let,
                .Var => .Var,
                .Field => .Field,
                .Enum => .Enum,
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

pub const BaseType = union(enum) {
    fnDecl: struct {
        pos: FilePos,
        pure: bool,
        args: ArrayList(*Type),
        ret: *Type,
    },
    symbol: Token,
};

pub const Type = union(enum) {
    base: BaseType,
    mod: struct {
        pos: FilePos,
        mod: union(enum) {
            optional: void,
            ptr: void,
            constant: void,
            slice: void,
            array: *Expr,

            /// Force any expression assigned to this to be resolved at comptime.
            compiletime: void,
        },
        next: *Type,
    },

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
