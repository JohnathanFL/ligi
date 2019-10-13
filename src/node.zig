const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Tokens = @import("token.zig");
const Token = Tokens.Token;
const Tag = Tokens.Tag;

pub const Op = union(enum) {
    // voids are builtins

    // Args: condition, captureToken, execIfTrue, execIfFalse
    // For example:
    // if(optVal) |val| {
    // val = 1;
    // }
    // Becomes: (using Unlabeled {} as a List)
    // {op: If, items: {optVal, Token{"val"}, {`=`, Token{"val"}, Token{1}}, Null}}
    If: void,

    // Args: condition, captureToken, execContinue, execDuring, execAfter
    // For example: (Assuming i exists)
    // while(i < 5) : (++i) {
    //   print("Hi");
    // } else {
    //   print("Finished");
    // }
    // Becomes:
    // {op: While, items: {
    // {op: `<`, items: {i, 5}},
    //  Null,
    //  {op: `++`, items: Token{i}},
    //  {op: Func(print), items: {Token{"Hi"}}},
    //  {op: Func(print), items: {Token{"Finished"}}
    //  }
    // }
    While: void,

    // Args: list, captureToken, execDuring, execAfter
    For: void,

    // TODO: Do we need this one at all? The Value.Token ought to do it already.
    // Take the literal value of the list (i.e a tuple or single value).
    // Used to execute the full program.
    Val: void,

    // Make an immutable variable in current scope.
    // Used for variables AND types and such.
    // Think Zig's 'const'.
    // Args: bindToken, type, val
    Let: void,
    // Make a mutable variable in current scope
    // Args: bindToken, type, val
    Var: void,
    // Name of the function to execute.
    // Functions that literally have the '`' characters surrounding them
    // are operators. (e.g `+`). This way they can be overloaded
    Func: []const u8,

    pub fn fromTag(tag: Tag) Op {
        switch(tag) {
            .Let => return .Let,
            .Var => return .Let,
            .While => return .While,
            .For => return .For,
            .If => return .If,
            else => {
                std.debug.warn("ERROR: Attemped to convert {} to an op!\n", tag);
                @panic("Invalid conversion.\n");
            }
        }
    }
};

pub const List = struct {
    op: Op,
    items: ArrayList(Value),

    pub fn init(alloc: *Allocator) List {
        return List{
            .op = .Var,
            .items = ArrayList(Value).init(alloc),
        };
    }

    pub fn from(alloc: *Allocator, op: Op, items: []const Value) List {
        var res = init(alloc);
        res.op = op;
        res.items.appendSlice(items);
        return res;
    }
};

pub const Value = union(enum) {
    // If the list is to be taken as is (like tuple (1, 2, 3)),
    // then list.op == Op.Val
    List: List,
    Token: Token,
    Null: void,
};
