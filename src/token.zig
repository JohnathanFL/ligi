pub const Tag = enum(u8) {
    Assign = 0, // =
    Or,
    And,
    Pipe, // |
    Xor, // ^
    BitAnd, // &
    Equal, // ==
    NotEqual, // !=
    Less, // <
    Greater, // >
    LessEq, // <=
    GreaterEq, // >=
    Shl, // <<
    Shr, // >>
    Add, // +
    AddAssign, // +=
    Sub, // -
    SubAssign, // -=
    Mul, // *
    MulAssign, // *=
    Div, // /
    DivAssign, // /=
    BitNot, // ~
    Not, // !
    Inc, // ++ // Equivalent to i++
    Dec, // -- // Equivalent to i--
    IncNow, // +++ // Equivalent to ++i
    DecNow, // --- // Equivalent to --i
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Dot, // .
    DotOpt, // .?
    Symbol,
    If,
    While,
    For,
    Colon,
    Semicolon,
    Let, // Immutable
    Var, // Mutable

    // All lits have the highest bit set
    IntLit = 0b10000000,
    BoolLit,
    FloatLit,
    CharLit,
    StringLit,
    NullLit, // null

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
