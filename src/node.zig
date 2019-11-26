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
};

pub const Stmt = union(enum) {
    ifStmt: *If,
    loop: *Loop,
    expr: *Expr,
    bind: *Bind,
};

pub const Expr = union(enum) {
    block: *Block,
    call: *Call,
    literal: Token,
    symbol: Token,
    case: *Case,
};

pub const If = struct {
    arms: ArrayList(struct {
        cond: *Expr,
        then: *Block,
    }),
    default: ?*Block,
    finally: ?*Block,
};

pub const Loop = struct {
    interpret: enum {
        Infinite,
        For,
        While,
    },
    capture: struct {
        index: ?Token,
        variable: ?struct {
            ref: bool,
            tok: Token,
        },
    },
    body: *Block,
    finally: ?*Block,
};

pub const Bind = struct {
    interpret: enum {
        Let,
        Var,
        Field,
        Enum,
    },
    loc: Token,
    ty: ?*Type,

    /// If interpret === Let,Var, then default != null
    default: ?*Expr,
};

/// All operators are also compiled down to a call.
pub const Call = struct {
    /// Are we eligible for overloads?
    operator: bool,
    /// What are we calling?
    func: Token,
    /// Since the majority of these are likely to be binary/unary expressions or function calls with
    /// few args, prealloc 2 just in case.
    args: SegmentedList(2, *Expr),

};

/// TODO: Case could really use a better name
pub const Case = struct {
    arms: ArrayList(struct {
        cond: *Expr,
        val: *Expr,
    }),
    default: *Expr,

    pub const Default = Case {.arms = undefined, .default = undefined};
};
