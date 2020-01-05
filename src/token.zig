pub const char = u32;
const Tag = union(enum) {
    // Punctuation
    LBrace: void,
    RBrace: void,
    LBracket: void,
    RBracket: void,
    LParen: void,
    RParen: void,
    Store: void, // ->
    Colon: void,
    Comma: void,

    ///// OPERATORS

    // Assertions
    // These are parsed at the same level as assignments
    Assert: void, // ===
    NotAssert: void, // !==
    
    // Assignment Operators
    Assign: void,
    AddAssign: void,
    SubAssign: void,
    MulAssign: void,
    DivAssign: void,
    ModAssign: void,
    ShlAssign: void,
    ShrAssign: void,


    // Standard expression ops 
    Add: void,
    Sub: void,
    Mul: void,
    Div: void,
    Mod: void,

    // Logical
    Not: void,
    And: void,
    Or: void,
    Xor: void,

    // Relational
    Eq: void,
    NotEq: void,
    LessEq: void,
    GreaterEq: void,

    // Range
    In: void,
    OpenRange: void,
    ClosedRange: void,

    // Bit twiddlers
    BitNot: void,
    BitAnd: void,
    BitOr: void,
    BitXor: void,

    // Code-mods
    Comptime: void,
    Pure: void,
    Inline: void,
    Optional: void, // ?
    // Pointer should also be here, but it's the same as Mul


    // Binds
    Let: void,
    Var: void,
    CVar: void,
    Field: void,
    Enum: void,
    Property: void,
    Alias: void, // New one: Creates a simple alias. The AST equivalent of a pointer

    // Typedefs
    StructDef: void,
    EnumDef: void,

    // Control Flow
    If: void,
    ElIf: void,
    Else: void,
    For: void,
    While: void,
    Loop: void,
    Finally: void,
    Switch: void, // Not specced yet, but planned.
    Fn: void,
    Break: void,
    Return: void,
    
    Label: []const u8,
    Symbol: []const u8,
    Sink: void, // '_'
    
    // Literals
    StringLit: []const u8, // This string is allocated apart from the file
    CharLit: char,
    BoolLit: bool, // Reserves both 'true' and 'false'
    IntLit: usize, // If it can't fit in a usize, it shouldn't be a literal.
    FloatLit: f64, // See above
};

const FilePos = struct {
  file_id: usize,
  line: usize,
  col: usize,
};
const Token = struct {
  pos: FilePos,
  lexeme: []const u8,
  tag: Tag,
};
