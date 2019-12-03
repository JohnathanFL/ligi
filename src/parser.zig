const std = @import("std");
const ArrayList = std.ArrayList;
const SegmentedList = std.SegmentedList;

const printf = std.debug.warn;

const LexerMod = @import("lexer.zig");
const Lexer = LexerMod.Lexer;
const Token = LexerMod.Token;
const Tag = LexerMod.Tag;
const LexVal = LexerMod.LexVal;

pub usingnamespace @import("node.zig");

pub const Parser = struct {
    lexer: *Lexer,
    cur: Token,
    alloc: *std.mem.Allocator,

    cur_block: ?*Block = undefined,

    /// Allows us to avoid constantly calling an allocator.
    storages: struct {
        // Since they're all SegmentedLists, we can safely store pointers to their contents.
        /// Arbitrary. Once we're production-scale, I'd say 1024+ ought to be better.
        /// TODO: Change all pointers in the AST into simple usize indices.
        /// Then we can just store the index and serialize the AST as is for incremental comp.
        const prealloc: usize = 128;
        alloc: *std.mem.Allocator,
        blocks: SegmentedList(Block, prealloc),
        stmts: SegmentedList(Stmt, prealloc),
        exprs: SegmentedList(Expr, prealloc),

        ifs: SegmentedList(If, prealloc),
        loops: SegmentedList(Loop, prealloc),
        binds: SegmentedList(Bind, prealloc),
        breaks: SegmentedList(Break, prealloc),

        calls: SegmentedList(Call, prealloc),
        if_exprs: SegmentedList(IfExpr, prealloc),
        struct_lits: SegmentedList(StructLiteral, prealloc),
        array_lits: SegmentedList(ArrayLiteral, prealloc),
        fns: SegmentedList(Fn, prealloc),

        types: SegmentedList(Type, prealloc),

        fn newInternal(self: *@This(), comptime T: type, comptime field: []const u8) *T {
            const storage = &@field(self, field);
            const pos = storage.len;
            storage.push(T.init(self.alloc)) catch unreachable;
            return storage.uncheckedAt(pos);
        }

        pub fn new(self: *@This(), comptime T: type) *T {
            return switch (T) {
                Block => self.newInternal(Block, "blocks"),
                Stmt => self.newInternal(Stmt, "stmts"),
                Expr => self.newInternal(Expr, "exprs"),
                If => self.newInternal(If, "ifs"),
                Loop => self.newInternal(Loop, "loops"),
                Bind => self.newInternal(Bind, "binds"),
                Break => self.newInternal(Break, "breaks"),
                Call => self.newInternal(Call, "calls"),
                IfExpr => self.newInternal(IfExpr, "if_exprs"),
                StructLiteral => self.newInternal(StructLiteral, "struct_lits"),
                ArrayLiteral => self.newInternal(ArrayLiteral, "array_lits"),
                Fn => self.newInternal(Fn, "fns"),
                Type => self.newInternal(Type, "types"),
                else => @compileError("Unsupported type for new"),
            };
        }
    },

    const ParseError = error{
        OutOfMemory,
        UnexpectedToken,
        BindingAlreadyInScope,
        ContinuationInvalidOnFor,

        CantDefaultValue,
        MustDefaultValue,

        NotImplemented,
        Expected,
        MultipleStructLitAssignment,
        MissingSpecifier,

        UnexpectedEOF,
    };

    pub fn init(lexer: *Lexer, alloc: *std.mem.Allocator) Parser {
        const prealloc = 128;
        return Parser{
            .cur = lexer.scan(),
            .lexer = lexer,
            .alloc = alloc,
            .cur_block = null,
            .storages = .{
                .alloc = alloc,
                .blocks = SegmentedList(Block, prealloc).init(alloc),
                .stmts = SegmentedList(Stmt, prealloc).init(alloc),
                .exprs = SegmentedList(Expr, prealloc).init(alloc),

                .ifs = SegmentedList(If, prealloc).init(alloc),
                .loops = SegmentedList(Loop, prealloc).init(alloc),
                .binds = SegmentedList(Bind, prealloc).init(alloc),
                .breaks = SegmentedList(Break, prealloc).init(alloc),

                .calls = SegmentedList(Call, prealloc).init(alloc),
                .if_exprs = SegmentedList(IfExpr, prealloc).init(alloc),
                .struct_lits = SegmentedList(StructLiteral, prealloc).init(alloc),
                .array_lits = SegmentedList(ArrayLiteral, prealloc).init(alloc),
                .fns = SegmentedList(Fn, prealloc).init(alloc),

                .types = SegmentedList(Type, prealloc).init(alloc),
            },
        };
    }

    pub fn parse(self: *Parser) ParseError!*Block {
        var res = try self.parseBlock(.Naked);
        res.interpret = .Struct;

        return res;
    }

    /// Can we add 'struct', 'enum', etc before a block?
    const BlockStyle = enum {
        /// Mutually exclusive with MayLabel.
        NeedReinterpret,

        /// May not reinterpret.
        Braced,

        /// For files. No label, reinterpret, or braces.
        Naked,
    };
    fn parseBlock(self: *Parser, comptime style: BlockStyle) ParseError!*Block {
        var block = self.storages.new(Block);
        block.parent = self.cur_block;
        self.cur_block = block;

        if (style != .Naked) {
            if (style == .NeedReinterpret) {
                if (self.tryMatch(.Struct)) |_| {
                    block.interpret = .Struct;
                } else if (self.tryMatch(.Enum)) |_| {
                    block.interpret = .Enum;
                } else if (self.tryMatch(.Block)) |_| {
                    block.interpret = .Stream;
                } else if (self.tryMatch(.Concept)) |_| {
                    block.interpret = .Concept;
                } else {
                    printf("\nExpected a block prefix at line {}", self.cur.pos.line);
                }
            }
            // Note: This implies that all blocks, even structs/etc, may be labeled (in AST)
            // It will be a typechecker error to break/label struct/enum/concept.
            // We don't need to worry about Stream blocks as much since they're never typechecked.
            if (self.tryMatch(.Label)) |label| {
                block.label = label;
            }
            const b = try self.matchGet(.LBrace);
            block.pos = b.pos;
        } else {
            block.pos = .{
                .file = self.lexer.file_id,
                .line = self.lexer.line,
                .col = self.lexer.col,
            };
        }

        const end_tag = if (style != .Naked) Tag.RBrace else Tag.EOF;
        while (!self.opt(end_tag)) {
            try block.stmts.append(try self.parseStmt());
        }

        if (style != .Naked) try self.match(.RBrace);

        self.cur_block = block.parent;
        return block;
    }

    fn parseStmt(self: *Parser) ParseError!*Stmt {
        var stmt = self.storages.new(Stmt);

        switch (self.cur.tag) {
            .Label, .LBrace => stmt.* = .{ .block = try self.parseBlock(.Braced) },
            .If => stmt.* = .{ .ifStmt = try self.parseIf() },
            .While, .For, .Loop => stmt.* = .{ .loop = try self.parseLoop() },
            .Let, .Var, .Enum, .Field => {
                stmt.* = .{ .bind = try self.parseBind(.AlwaysSpecifier, .MayType, .MayDefault) };
                try self.match(.Semicolon);
            },
            .Break => {
                self.match(.Break) catch unreachable;
                stmt.* = .{ .breakStmt = self.storages.new(Break) };

                if (self.tryMatch(.Label)) |label| {
                    stmt.breakStmt.label = label;
                }

                if (!self.opt(.Semicolon)) {
                    stmt.breakStmt.val = try self.parseExpr();
                }

                try self.match(.Semicolon);
            },
            .Return => {
                self.match(.Return) catch unreachable;
                stmt.* = .{ .returnStmt = try self.parseExpr() };
                try self.match(.Semicolon);
            },
            .EOF => return error.UnexpectedEOF,

            // The only valid way at this point should be an expression
            else => {
                stmt.* = .{ .expr = try self.parseExpr() };
                try self.match(.Semicolon);
            },
        }

        return stmt;
    }

    fn parseIf(self: *Parser) ParseError!*If {
        var ifStmt = self.storages.new(If);

        const opener = try self.matchGet(.If);
        ifStmt.pos = opener.pos;

        // We always have at least one arm
        {
            var arm = If.Arm{
                .cond = try self.parseExpr(),
                .then = undefined,
                .capture = null,
            };
            if (self.tryMatch(.Colon)) |_| {
                try self.match(.BitOr);
                arm.capture = try self.parseBind(.MaySpecifier, .NeverType, .NeverDefault);
                try self.match(.BitOr);
            }

            arm.then = try self.parseBlock(.Braced);
            try ifStmt.arms.append(arm);
        }
        while (self.tryMatch(.ElIf)) |_| {
            var arm = If.Arm{
                .cond = try self.parseExpr(),
                .then = undefined,
                .capture = null,
            };
            if (self.tryMatch(.Colon)) |_| {
                try self.match(.BitOr);
                arm.capture = try self.parseBind(.MaySpecifier, .NeverType, .NeverDefault);
                try self.match(.BitOr);
            }

            arm.then = try self.parseBlock(.Braced);
            try ifStmt.arms.append(arm);
        }

        if (self.tryMatch(.Else)) |_| {
            ifStmt.default = try self.parseBlock(.Braced);
        }

        if (self.tryMatch(.Finally)) |_| {
            ifStmt.finally = try self.parseBlock(.Braced);
        }

        return ifStmt;
    }

    fn parseLoop(self: *Parser) ParseError!*Loop {
        var loop = self.storages.new(Loop);

        const opener = try self.matchGetOne([_]Tag{ .Loop, .While, .For });
        loop.pos = opener.pos;
        switch (opener.tag) {
            .Loop => loop.interpret = .Infinite,
            .While => loop.interpret = .While,
            .For => loop.interpret = .For,
            else => unreachable,
        }

        if (loop.interpret != .Infinite) loop.expr = try self.parseExpr();

        if (self.tryMatch(.Colon)) |_| {
            // We have a capture to parse
            try self.match(.BitOr);
            // The user may use the var specifier to specify that they want the binding to be mutable.
            loop.capture.index = try self.parseBind(.MaySpecifier, .NeverType, .NeverDefault);
            if (self.tryMatch(.Comma)) |_| {
                loop.capture.variable = try self.parseBind(.MaySpecifier, .NeverType, .NeverDefault);
            }

            try self.match(.BitOr);

            if (self.tryMatch(.Colon)) |_| {
                loop.cont = try self.parseExpr();
            }
        }

        loop.body = try self.parseBlock(.Braced);

        if (self.tryMatch(.Finally)) |_| {
            loop.finally = try self.parseBlock(.Braced);
        }

        return loop;
    }

    const NeedSpec = enum {
        /// Directly bound in a block
        AlwaysSpecifier,
        NeverSpecifier,

        /// A capture.
        /// Technically, only var is allowed here, but we can let the typechecker handle that.
        MaySpecifier,
    };
    const NeedTy = enum {
        ///
        AlwaysType,
        NeverType,
        MayType,
    };
    const NeedDefault = enum {
        NeverDefault,
        MayDefault,
    };
    fn parseBind(self: *Parser, comptime needs_specifier: NeedSpec, comptime parse_ty: NeedTy, comptime may_default: NeedDefault) ParseError!*Bind {
        const specifiers = [_]Tag{
            .Let, .Var, .Enum, .Field,
        };

        var bind = self.storages.new(Bind);

        //printf("Next: {}", self.cur.tag);
        if (needs_specifier != .NeverSpecifier) {
            if (self.tryMatchOne(specifiers)) |spec| {
                bind.interpret = Bind.Interpret.fromTag(spec.tag);
            } else if (needs_specifier == .AlwaysSpecifier) {
                return error.MissingSpecifier;
            }
        }

        bind.loc = try self.matchGetOne([_]Tag{ .Symbol, .NoLoc });

        if (parse_ty == .AlwaysType or (parse_ty == .MayType and self.opt(.Colon))) {
            try self.match(.Colon);
            bind.ty = try self.parseType();
        }

        if (may_default == .MayDefault and self.opt(.Assign)) {
            try self.match(.Assign);
            bind.default = try self.parseExpr();
        }

        return bind;
    }

    fn parseExpr(self: *Parser) ParseError!*Expr {
        // Note: This means that you can't write 0 + if cond:...
        // You must write 0 + (if cond: ...).
        // This is to remove ambiguity in the case of if cond: val else val2 + 0
        if (self.opt(.If)) {
            return try self.parseIfExpr();
        } else {
            return try self.parseBinExprLevel(0);
        }
    }

    fn parseIfExpr(self: *Parser) ParseError!*Expr {
        try self.match(.If);
        var res = self.storages.new(Expr);
        res.* = .{ .ifExpr = self.storages.new(IfExpr) };

        {
            var arm: IfExpr.Arm = undefined;
            // Purposefully chosen so you can't directly nest like ' if if val: val1 else val2: val3 else: val4
            arm.cond = try self.parseBinExprLevel(0);
            try self.match(.Colon);
            if (self.tryMatch(.BitOr)) |_| {
                arm.capture = try self.parseBind(.MaySpecifier, .NeverType, .NeverDefault);
                try self.match(.BitOr);
            } else {
                arm.capture = null;
            }

            arm.val = try self.parseBinExprLevel(0);
            try res.ifExpr.arms.append(arm);
        }

        while (self.tryMatch(.ElIf)) |_| {
            var arm: IfExpr.Arm = undefined;
            // Purposefully chosen so you can't directly nest like ' if if val: val1 else val2: val3 else: val4
            arm.cond = try self.parseBinExprLevel(0);
            try self.match(.Colon);
            if (self.tryMatch(.BitOr)) |_| {
                arm.capture = try self.parseBind(.MaySpecifier, .NeverType, .NeverDefault);
                try self.match(.BitOr);
            } else {
                arm.capture = null;
            }

            arm.val = try self.parseBinExprLevel(0);
            try res.ifExpr.arms.append(arm);
        }

        try self.match(.Else);
        // TODO: Should the else here need a colon?
        // There's no ambiguity to solve here, but there's also something to say for consistency.
        //try self.match(.Colon);
        res.ifExpr.default = try self.parseBinExprLevel(0);

        return res;
    }

    /// Any binary operator. Recurses into itself for the next level of precedence.
    fn parseBinExprLevel(self: *Parser, comptime level: usize) ParseError!*Expr {
        if (level > Tag.binary_ops.len) @compileError("Can't call parseBinExprLevel with higher precedence than exists!");
        var res = if (level + 1 < Tag.binary_ops.len)
            try self.parseBinExprLevel(level + 1)
        else
            try self.parseUnaryExpr();

        if (self.optOne(Tag.binary_ops[level])) {
            var lastOp = self.cur.tag;
            while (self.tryMatchOne(Tag.binary_ops[level])) |op| {
                // Preserve left->right associativity by rotating
                // the tree left when we hit another op.
                // For example, without this, we'd parse:
                // a / b * c -> a / (b*c)
                // a/b/c -> a / (b / c)
                // etc...
                res = self.newCall(op, [_]*Expr{res});
                res.call.operator = true;
                try res.call.args.append(if (level + 1 < Tag.binary_ops.len) try self.parseBinExprLevel(level + 1) else try self.parseUnaryExpr());
                lastOp = op.tag;
            }
        }
        return res;
    }

    fn newCall(self: *Parser, op: Token, args: []const *Expr) *Expr {
        var new = self.storages.new(Call);
        new.func = op;
        for (args) |arg| {
            new.args.append(arg) catch unreachable;
        }

        var res = self.storages.new(Expr);
        res.* = .{ .call = new };
        return res;
    }

    inline fn exprFromSymbol(self: *Parser, sym: Token) *Expr {
        var res = self.storages.new(Expr);
        res.* = .{ .symbol = sym };
        return res;
    }

    inline fn exprFromLiteral(self: *Parser, lit: Token) *Expr {
        var res = self.storages.new(Expr);
        res.* = .{ .literal = lit };
        return res;
    }

    /// Any _single_ unary (e.g -, --, ++, etc) operator
    /// Multiple operators aren't allowed, as they'd get messy fast:
    /// a = - -b;
    /// a = - --b;
    /// a = - ---b;
    fn parseUnaryExpr(self: *Parser) ParseError!*Expr {
        var res: *Expr = undefined;
        if (self.tryMatchOne(Tag.unary_ops)) |op| {
            res = self.storages.new(Expr);
            res.* = Expr{ .call = self.storages.new(Call) };
            res.call.func = op;
            res.call.operator = true;
            try res.call.args.append(try self.parseAccessExpr());
        } else {
            res = try self.parseAccessExpr();
        }

        return res;
    }

    /// any number of Field accesses, []s, and ()s.
    fn parseAccessExpr(self: *Parser) ParseError!*Expr {
        var res = try self.parseBaseVal();

        while (self.tryMatchOne(Tag.access_ops)) |op| {
            switch (op.tag) {
                .Dot => {
                    const right = try self.matchGet(.Symbol);
                    res = self.newCall(op, [_]*Expr{ res, self.exprFromSymbol(right) });
                },

                // Note: This means that it is syntactically correct for [] to have 0 args.
                // I do, however, plan for this to be a typechecker error.
                .LBracket, .LParen => {
                    const closer: Tag = if (op.tag == .LBracket) .RBracket else .RParen;
                    // Remember, [] and () may _both_ have multiple arguments.
                    res = self.newCall(Token{
                        .lexeme = if (op.tag == .LBracket) "[]" else "()",
                        .val = undefined,
                        .tag = .Symbol,
                        .pos = op.pos,
                    }, [_]*Expr{});

                    while (!self.opt(closer)) {
                        try res.call.args.append(try self.parseExpr());
                        // If we aren't at the end, we need to delimit.
                        if (!self.opt(closer)) try self.match(.Comma);
                    }

                    try self.match(closer);
                },
                else => unreachable,
            }
        }

        return res;
    }

    /// The absolute base values:
    ///     int literals
    ///     float literals
    ///     bool literals
    ///     str literals
    ///     char literals
    ///     null literal
    ///     symbols
    ///     (expr)s
    ///     `label {block}s
    ///     compound literals
    ///     undef
    ///     fn or purefn
    fn parseBaseVal(self: *Parser) ParseError!*Expr {
        var res: *Expr = undefined;
        if (self.tryMatch(.Symbol)) |sym| {
            res = self.exprFromSymbol(sym);
        } else if (self.opt(.LBracket)) {
            // A '[' without a preceeding symbol always means the beginning of a compound literal
            res = try self.parseCompoundLiteral();
        } else if (self.tryMatch(.LParen)) |_| {
            // Parenthesized expression. The recursion continues.
            res = try self.parseExpr();
            try self.match(.RParen);
        } else if (self.tryMatchOne([_]Tag{ .IntLit, .BoolLit, .StringLit, .NullLit, .CharLit })) |lit| {
            // No special processing. We'll resolve that when we actually need it.
            res = self.exprFromLiteral(lit);
        } else if (self.optOne([_]Tag{ .Struct, .Enum, .Concept, .Block })) {
            // Since type definitions are just another value to be manipulated
            res = self.storages.new(Expr);
            res.* = .{ .block = try self.parseBlock(.NeedReinterpret) };
        } else if (self.tryMatch(.Undef)) |_| {
            res = self.storages.new(Expr);
            res.* = .{ .undef = .{} };
        } else if (self.opt(.Label)) {
            res = self.storages.new(Expr);
            res.* = .{ .block = try self.parseBlock(.Braced) };
        } else if (self.optOne([_]Tag{ .Fn, .PureFn })) {
            res = self.storages.new(Expr);
            res.* = .{ .fnDef = try self.parseFn() };
        } else {
            printf("\nExpected one of: symbol, literal, compound literal, or type block but found {}", self.cur.tag);
            return error.Expected;
        }

        return res;
    }

    fn parseFn(self: *Parser) ParseError!*Fn {
        var res = self.storages.new(Fn);
        if (self.tryMatch(.PureFn)) |_| {
            res.pure = true;
        } else {
            try self.match(.Fn);
        }

        try self.match(.LParen);

        while (!self.opt(.RParen)) {
            // Relaxed to .MayType. This way we can allow for shorter lambdas:
            //      varThingy.lambda = fn(a: usize, b: Blah) res: usize { res = 5 };
            //      vs
            //      varThingy.lambda = fn(a, b) res { res = 5};
            // No specified type would then be a typechecker error.
            try res.args.append(try self.parseBind(.NeverSpecifier, .MayType, .NeverDefault));

            if (!self.opt(.RParen)) try self.match(.Comma);
        }

        try self.match(.RParen);

        if (self.tryMatch(.Void)) |_| {
            res.ret = null;
        } else {
            // TODO: Would .MayDefault be good? May let people eliminate lines of code.
            res.ret = try self.parseBind(.NeverSpecifier, .MayType, .NeverDefault);
        }
        // Labeling a function block will be a type error.
        res.body = try self.parseBlock(.Braced);
        return res;
    }

    fn parseCompoundLiteral(self: *Parser) ParseError!*Expr {
        try self.match(.LBracket);

        var ty: ?*Type = null;
        if (self.tryMatch(.Colon)) |_| {
            ty = try self.parseType();
            try self.match(.Colon);
        }

        var res = self.storages.new(Expr);
        if (self.opt(.Dot)) {
            // Struct literal
            res.* = .{ .struct_lit = self.storages.new(StructLiteral) };
            res.struct_lit.ty = ty;

            while (!self.opt(.RBracket)) {
                try self.match(.Dot);
                const name = (try self.matchGet(.Symbol)).lexeme;
                try self.match(.Assign);
                _ = res.struct_lit.vals.putNoClobber(name, try self.parseExpr()) catch return error.MultipleStructLitAssignment;

                if (!self.opt(.RBracket)) try self.match(.Comma);
            }
        } else {
            // Array literal
            res.* = .{ .array_lit = self.storages.new(ArrayLiteral) };
            res.array_lit.ty = ty;

            try res.array_lit.vals.append(try self.parseExpr());
            while (self.tryMatch(.Comma)) |_| {
                try res.array_lit.vals.append(try self.parseExpr());
            }
        }

        try self.match(.RBracket);

        return res;
    }

    fn parseType(self: *Parser) ParseError!*Type {
        var res = self.storages.new(Type);
        if (self.tryMatch(.Symbol)) |sym| {
            res.* = .{ .base = .{ .symbol = sym } };
        } else if (self.tryMatchOne([_]Tag{ .PureFn, .Fn })) |opener| {
            res.* = .{
                .base = .{
                    .fnDecl = .{
                        .pos = opener.pos,
                        .pure = false,
                        .args = ArrayList(*Type).init(self.alloc),
                        .ret = undefined,
                    },
                },
            };
            res.base.fnDecl.pure = if (opener.tag == .PureFn) true else false;

            try self.match(.LParen);
            while (!self.opt(.RParen)) {
                try res.base.fnDecl.args.append(try self.parseType());

                if (!self.opt(.RParen)) try self.match(.Comma);
            }
            try self.match(.RParen);

            res.base.fnDecl.ret = try self.parseType();
        } else if (self.tryMatch(.Optional)) |o| {
            res.* = .{ .mod = .{ .pos = o.pos, .mod = .optional, .next = try self.parseType() } };
        } else if (self.tryMatch(.Mul)) |p| {
            res.* = .{ .mod = .{ .pos = p.pos, .mod = .ptr, .next = try self.parseType() } };
        } else if (self.tryMatch(.Const)) |c| {
            res.* = .{ .mod = .{ .pos = c.pos, .mod = .constant, .next = try self.parseType() } };
        } else if (self.tryMatch(.Comptime)) |c| {
            res.* = .{ .mod = .{ .pos = c.pos, .mod = .compiletime, .next = try self.parseType() } };
        } else if (self.tryMatch(.LBracket)) |b| {
            // Slice or array?
            if (self.tryMatch(.RBracket)) |_| {
                // Slice.
                res.* = .{ .mod = .{ .pos = b.pos, .mod = .slice, .next = try self.parseType() } };
            } else {
                // Array.
                // Typechecking will ensure this is constant.
                const size = try self.parseExpr();
                try self.match(.RBracket);
                res.* = .{ .mod = .{ .pos = b.pos, .mod = .{ .array = size }, .next = try self.parseType() } };
            }
        } else {
            return error.Expected;
        }

        return res;
    }

    fn match(self: *Parser, tag: Tag) !void {
        _ = try self.matchGet(tag);
    }

    fn matchGet(self: *Parser, tag: Tag) !Token {
        return self.tryMatch(tag) orelse {
            printf("\nLine {}: Expected a {}", self.lexer.line, tag);
            return error.Expected;
        };
    }

    fn tryMatch(self: *Parser, tag: Tag) ?Token {
        if (self.cur.tag == tag) {
            //printf("\n\tMatched {} ({})", self.cur.tag, self.cur.lexeme);
            return self.advance();
        }
        return null;
    }

    fn matchOne(self: *Parser, tags: []const Tag) !void {
        if (self.tryMatchOne(tags) == null) {
            printf("\nLine {}: Expected one of {} but found {}", self.lexer.line, tags, self.cur.tag);
            return error.Expected;
        }
    }

    fn matchGetOne(self: *Parser, tags: []const Tag) !Token {
        return self.tryMatchOne(tags) orelse {
            printf("\nLine {}: Expected one of {} but found {}", self.lexer.line, tags, self.cur.tag);
            return error.Expected;
        };
    }

    fn tryMatchOne(self: *Parser, tags: []const Tag) ?Token {
        for (tags) |tag| {
            if (self.tryMatch(tag)) |tok| return tok;
        }

        return null;
    }

    fn opt(self: *Parser, tag: Tag) bool {
        return self.cur.tag == tag;
    }

    fn optOne(self: *Parser, tags: []const Tag) bool {
        for (tags) |tag| {
            if (self.cur.tag == tag) return true;
        }
        return false;
    }

    fn advance(self: *Parser) Token {
        const old = self.cur;
        self.cur = self.lexer.scan();
        return old;
    }
};

test "grammar.zag" {
    // The tutorial shall be used as the official "if it's here, it needs to work" test.
    var input = @embedFile("grammar.zag");
    var lexer = Lexer{
        .input = input,
        .line = 1,
        .file_id = 0,
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);
    defer arena.deinit();
    var parser = Parser.init(&lexer, &arena.allocator);
    var body = parser.parse() catch {
        printf("\nFailed to parse grammar.zag on line {}", lexer.line);
        //printf("\nRemaining: \n{}", lexer.input);
        @panic("");
    };

    //printf("\n{p}\n", body);

    //var printer = PrettyPrinter {.counter = 1};
    //printf("\n\ndigraph G{{");
    //printer.visitBlock(body);
    //printf("}}\n\n");
}
