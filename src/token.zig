pub const Tag = enum(u8) {
    Case,
    Implies, // => 
    Add, // +
    AddAssign, // +=
    And,
    Assert, // ===
    Assign, // =
    BitAnd, // &
    BitNot, // ~
    BitOr,
    CaseOf, // caseof 
    Colon,
    Comma,
    Dec, // -- // Equivalent to i--
    DecNow, // --- // Equivalent to --i
    Div, // /
    DivAssign, // /=
    Dot, // .
    ElIf, // elif    
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
    Shr, // >>
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

    // Sorted be Binary-Unary, then by precedence
    const all_ops = [_]Tag {
      .Assign, // =
      .AddAssign, // +=
      .DivAssign, // /=
      .MulAssign, // *=
      .SubAssign, // -=

      .Or,

      .And,

      .Assert, // ===
      .Equal, // ==
      .NotEqual, // !=
      .Greater, // >
      .GreaterEq, // >=
      .Less, // <
      .LessEq, // <=

      
      .BitOr, // |
      .Xor, // ^
      .BitAnd, // &

  
      .Shl, // <<
      .Shr, // >>
      .AShr, // >>>

      .Add, // +
      .Sub, // -

      .Mul,
      .Div,
      .Mod,
           
      // UNARY OPERATORS
      .Sub, 
      .Not, // !
      .Inc, // ++ // Equivalent to i++
      .IncNow, // +++ // Equivalent to ++i    
      .Dec, // -- // Equivalent to i--
      .DecNow, // --- // Equivalent to --i    
    };
    const unary_ops = [_]Tag {
      .Not, .BinNot, .Sub, .Inc, .Dec, .IncNow, .DecNow
    };

    const greatest_binary_precedence = 9;
    // Precedence here essentially means the depth it would be in a tree with all ops
    // Note this is only for binary operators
    pub fn precedence(self: Tag) u32 {
      return switch (self) {
        .Assign, .AddAssign, .SubAssign, .MulAssign, 
        .DivAssign, .ShlAssign, .ShrAssign, .OrAssign, 
        .AndAssign => 0,
        
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

    pub fn opsOfPrecedence(comptime prec: u32) []const Tag {
      comptime if(prec > 9) @compileError("No precedence higher than 9!");      

      return switch (prec) {
        0 => [_]Tag {
          .Assign, .AddAssign, .SubAssign, .MulAssign, 
          .DivAssign, .ShlAssign, .ShrAssign, .OrAssign, 
          .AndAssign 
        },
        
        1 => [_]Tag {.Or},
        2 => [_]Tag {.And},
        
        3 => [_]Tag {.Equal, .NotEqual, .Less, .Greater, 
        .LessEq, .GreaterEq, .Assert},

        4 => [_]Tag {.BitOr},
        5 => [_]Tag {.BitXor},
        6 => [_]Tag {.bitAnd},
        7 => [_]Tag {.Shl, .Shr, .AShr},
        8 => [_]Tag {.Add, .Sub},
        9 => [_]Tag {.Mul, .Div, .Mod},
        
        else => [_]Tag{},
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
