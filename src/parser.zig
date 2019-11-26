const std = @import("std");
const ArrayList = std.ArrayList;
const SegmentedList = std.SegmentedList;

const LexerMod = @import("lexer.zig");
const Lexer = LexerMod.Lexer;
const Token = LexerMod.Token;
const Tag = LexerMod.Tag;
const LexVal = LexerMod.LexVal;

pub usingnamespace @import("node.zig");

pub const Parser = struct {
    lexer: Lexer,
    cur: Token,
    alloc: *std.mem.Allocator,

    cur_block: ?*Block = undefined,


    /// Allows us to avoid constantly calling an allocator.
    storages: struct {
        // Since they're all SegmentedLists, we can safely store pointers to their contents.

        /// Arbitrary. Once we're production-scale, I'd say 1024+ ought to be better.
        const prealloc: usize = 128;
        blocks: SegmentedList(prealloc, Block),
        stmts: SegmentedList(prealloc, Stmt),
        exprs: SegmentedList(prealloc, Expr),

        ifs: SegmentedList(prealloc, If),
        loops: SegmentedList(prealloc, Loop),
        binds: SegmentedList(prealloc, Bind),

        calls: SegmentedList(prealloc, Call),
        cases: SegmentedList(prealloc, Case),

        pub fn new(comptime T: type) *T {
            switch(T) {
                Block => {

                },
                Stmt => {

                },
                Expr => {

                },
                If => {

                },
                Loop => {

                },
                Bind => {

                },
                Call => {

                },
                Case => {

                },
            }
        }
    },


    const ParseError = error{
        UnexpectedToken,
        BindingAlreadyInScope,
        ContinuationInvalidOnFor,

        CantDefaultValue,
        MustDefaultValue,

        NotImplemented,
    };

    pub fn init(lexer: Lexer, alloc: *std.mem.Allocator) Parser {
        var our_lexer = lexer;
        return Parser {
            .cur = our_lexer.scan(),
            .lexer = our_lexer,
            .alloc = alloc,
            .storages = .{
                .blocks = SegmentedList(prealloc, Block).init(alloc),
                .stmts = SegmentedList(prealloc, Stmt).init(alloc),
                .exprs = SegmentedList(prealloc, Expr).init(alloc),

                .ifs = SegmentedList(prealloc, If).init(alloc),
                .loops = SegmentedList(prealloc, Loop).init(alloc),
                .binds = SegmentedList(prealloc, Bind).init(alloc),

                .calls = SegmentedList(prealloc, Call).init(alloc),
                .cases = SegmentedList(prealloc, Case).init(alloc),
            },
        };
    }

    pub fn parseBlock(self: *Parser, needs_braces: bool) *Block {
        var block = Block.new(self.alloc);
      
        if (needs_braces) self.match(.LBrace);

        switch (self.cur.tag) {
            .LBrace => 
        }

        if (needs_braces) self.match(.RBrace);
    }

    fn match(self: *Parser, tag: Tag) !void {
        if (self.tryMatch(tag) == null) return error.Expected;
    }

    fn tryMatch(self: *Parser, tag: Tag) ?Token {
        if (self.cur.tag == tag) return self.advance();
        return null;
    }

    fn matchOne(self: *Parser, tags: []const Tag) !void {
        if (self.tryMatchOne(tags) == null) return error.Expected;
    }

    fn tryMatchOne(self: *Parser, tags: []const Tag) ?Token {
        for (tags) : |tag| {
            if (self.cur.tag == tag) return self.advance();
        }

        return null;
    }

    fn opt(self: *Parser, tag: Tag) bool {
        return self.cur.tag == tag;
    }

    fn optOne(self: *Parser, tags: []const Tag) bool {
        for (tags) : |tag| {
            if(self.cur.tag == tag) return true;
        }
        return false;
    }

    fn advance(self: *Parser) Token {
        const old = self.cur;
        self.cur = self.lexer.scan();
        return old;
    }
};

fn adapter(msg: []const u8) void {
    std.debug.warn("{}", msg);
}

test "parser" {
    var input =
        \\ let i;
        \\ var j;
        \\ let x = 1;
        \\ let y = x.foo(b);
    ;

    var lexer = Lexer{
        .input = input,
        .line = 0,
        .file_id = 0,
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);

    var body = try Parser.parse(lexer, &arena.allocator);
    PrettyPrinter.prettyPrint(body);
}
