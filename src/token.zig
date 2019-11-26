pub const Tag = enum(u8) {
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
    BitXor, // ^
    Case,
    CaseOf, // caseof 
    Colon,
    Comma,
    Concept, 
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
    Implies, // => 
    Inc, // ++ // Equivalent to i++
    IncNow, // +++ // Equivalent to ++i
    Label, // `xxxx
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
    Optional, // ?
    Or,
    Ptr, // *
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
    Void,
    While,
    Xor, // xor
    Block,
    Concept,
    Field,

    
    // All lits have the highest bit set
    IntLit = 0b10000000,
    BoolLit,
    FloatLit,
    CharLit,
    StringLit,
    NullLit, // null

    pub const access_ops = [_]Tag {
      .Dot, .LBracket, .LParen
    };

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
        .Xor => 5,
        .BitAnd => 6,
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

/// These are the only 3 (currently) that could benefit from pre-parsing their values.
/// chars/strs need escape characters interpreted, and bools only have 2 to begin with.
pub const LexVal = union {
    charVal: u8,
    boolVal: bool,
    strVal: []const u8,
};

pub const Token = struct {
    lexeme: []const u8,
    /// Only used if tag == CharLit, BoolLit, StringLit
    val: LexVal = undefined,
    tag: Tag,
    file: usize,
    line: usize,
    col: usize,
};
