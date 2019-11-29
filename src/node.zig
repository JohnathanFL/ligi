const std = @import("std");
const ArrayList = std.ArrayList;
const SegmentedList = std.SegmentedList;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;

const Tokens = @import("token.zig");
const Token = Tokens.Token;
const Tag = Tokens.Tag;

pub const Block = struct {
    interpret: enum {
        Struct,
        Enum,

        /// Zag's answer to generic interfaces
        Concept,

        /// Don't interpret it at all
        Stream,

        /// Completely normal mode: Just run each statement inside at runtime.
        Block,
    },
    stmts: ArrayList(*Stmt),
    label: ?Token,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .interpret = .Block,
            .stmts = ArrayList(*Stmt).init(alloc),
            .label = null,
        };
    }
};

pub const Break = struct {
    label: ?Token,
    val: ?*Expr,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .label = null,
            .val = null,
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
    returnStmt: *Expr,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .bind = undefined,
        };
    }
};

pub const StructLiteral = struct {
    vals: StringHashMap(*Expr),
    ty: ?*Type,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .vals = StringHashMap(*Expr).init(alloc),
            .ty = null,
        };
    }
};

pub const ArrayLiteral = struct {
    vals: ArrayList(*Expr),
    ty: ?*Type,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .vals = ArrayList(*Expr).init(alloc),
            .ty = null,
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

pub const Fn = struct {
    pure: bool,
    args: ArrayList(*Bind),

    /// If null, then it returns void.
    ret: ?*Bind,
    body: *Block,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .pure = false,
            .args = ArrayList(*Bind).init(alloc),
            .ret = null,
            .body = undefined,
        };
    }
};

pub const If = struct {
    pub const Arm = struct {
        cond: *Expr,
        then: *Block,
        capture: ?*Bind,
    };
    arms: ArrayList(Arm),
    default: ?*Block,
    finally: ?*Block,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        const dummy: If = undefined;
        return .{
            .arms = @typeOf(dummy.arms).init(alloc),
            .default = null,
            .finally = null,
        };
    }
};

pub const Loop = struct {
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
            return switch(tag) {
                .Let => .Let,
                .Var => .Var,
                .Field => .Field,
                .Enum => .Enum,
                else => @panic("fromTag with non specifier!")
            };
        }
    };
    interpret: Interpret,
    loc: Token,
    ty: ?*Type,

    /// If interpret === Let,Var, then default != null
    default: ?*Expr,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .interpret = .Let,
            .loc = undefined,
            .ty = null,
            .default = null,
        };
    }
};

/// All operators are also compiled down to a call.
pub const Call = struct {
    /// Are we eligible for overloads?
    operator: bool,

    /// What are we calling?
    func: Token,

    /// Since the majority of these are likely to be binary/unary expressions or function calls with
    /// few args, prealloc 2 just in case.
    args: ArrayList(*Expr),

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .operator = false,
            .func = undefined,
            .args = ArrayList(*Expr).init(alloc),
        };
    }
};

pub const IfExpr = struct {
    pub const Arm = struct {
        cond: *Expr,
        capture: ?*Bind,
        val: *Expr,
    };
    arms: ArrayList(Arm),
    default: *Expr,

    pub fn init(alloc: *std.mem.Allocator) @This() {
        return .{
            .arms = ArrayList(Arm).init(alloc),
            .default = undefined,
        };
    }
};

pub const BaseType = union(enum) {
    fnDecl: struct {
        pure: bool,
        args: ArrayList(*Type),
        ret: *Type,
    },
    symbol: Token,
};

pub const Type = union(enum) {
    base: BaseType,
    mod: struct {
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
                    .line = 0,
                    .col = 0,
                    .file = 0,
                },
            },
        };
    }
};
