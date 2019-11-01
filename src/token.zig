pub const Tag = enum(u8) {
    Label, // `xxxx
    Case,
    Implies, // => 
    Add, // +
    AddAssign, // +=
    And,
    AShr, // >>>
    Assert, // ===
    Assign, // =
    BitAnd, // &
    BitAndAssign, // &=
    BitNot, // ~
    BitOr, // |
    BitOrAssign, // |=
    CaseOf, // caseof 
    Colon,
    Comma,
    Dec, // -- // Equivalent to i--
    DecNow, // --- // Equivalent to --i
    Div, // /
    DivAssign, // /=
    Dot, // .
    ElIf, // elif
    Else,
    Enum,
    EOF, // When the lexer reaches the end of the stream
    Equal, // ==
    Finally, // finally
    Fn, // fn
    For,
    Greater, // >
    GreaterEq, // >=
    If,
    Inc, // ++ // Equivalent to i++
    IncNow, // +++ // Equivalent to ++i
    LBrace,
    LBracket,
    Less, // <
    LessEq, // <=
    Let, // Immutable
    LParen,
    Mod, // %
    Mul, // *
    MulAssign, // *=
    Not, // !
    NotEqual, // !=
    Null,
    Opt, // ?
    Or,
    PureFn, // purefn
    RBrace,
    RBracket,
    RParen,
    Semicolon,
    Shl, // <<
    ShlAssign, // <<=
    Shr, // >>
    ShrAssign, // >>=
    Struct,
    Sub, // -
    SubAssign, // -=
    Symbol,
    Union,
    Var, // Mutable
    While,
    Xor, // ^      
    // All lits have the highest bit set
    IntLit = 0b10000000,
    BoolLit,
    FloatLit,
    CharLit,
    StringLit,
    NullLit, // null


    pub const unary_ops = [_]Tag {
      .Not, .BitNot, .Sub, .Inc, .Dec, .IncNow, .DecNow
    };

    const greatest_binary_precedence = 9;
    // Precedence here essentially means the depth it would be in a tree with all ops
    // Note this is only for binary operators
    pub fn precedence(self: Tag) u32 {
      return switch (self) {
        .Assign, .AddAssign, .SubAssign, .MulAssign, 
        .DivAssign, .ShlAssign, .ShrAssign, .BitOrAssign, 
        .BitAndAssign => 0,
        
        .Or => 1,
        .And => 2,
        
        .Equal, .NotEqual, .Less, .Greater, 
        .LessEq, .GreaterEq, .Assert => 3,

        .BitOr => 4,
        .BitXor => 5,
        .bitAnd => 6,
        .Shl, .Shr, .AShr => 7,
        .Add, .Sub => 8,
        .Mul, .Div, .Mod => 9,
        
        else => 2663,
      };
    }

    // Sorted by precedence 0-...
    pub const binary_ops = [_][]const Tag{

        [_]Tag {
          .Assign, .AddAssign, .SubAssign, .MulAssign, 
          .DivAssign, .ShlAssign, .ShrAssign, .BitOrAssign, .BitAndAssign,
        },
        
        [_]Tag {.Or},
        [_]Tag {.And},
        
        [_]Tag {.Equal, .NotEqual, .Less, .Greater, 
        .LessEq, .GreaterEq, .Assert},

        [_]Tag {.BitOr},
        [_]Tag {.Xor},
        [_]Tag {.BitAnd},
        [_]Tag {.Shl, .Shr, .AShr},
        [_]Tag {.Add, .Sub},
        [_]Tag {.Mul, .Div, .Mod},
    };
    
    pub fn matching(self: Tag) ?Tag {
      return switch (self) {
        .LBrace => .RBrace,
        .LParen => .RParen,
        .LBracket => .RBracket,
        else => null,
      };
    }
  
    pub fn isLit(self: Tag) bool {
        return @enumToInt(self) & 0b10000000 != 0;
    }
};

pub const LexVal = union(enum) {
    pub IntLit: i128,
    pub CharLit: u8,
    pub BoolLit: bool,
    pub DoesntMatter: void, // StringLit: just use the lexeme ya dolt
    // NullLit: whadd'ya think?
};

pub const Token = struct {
    pub lexeme: []const u8,
    pub val: LexVal = .DoesntMatter,
    pub tag: Tag,
    pub file: usize,
    pub line: usize,
};
