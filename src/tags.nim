type Tag* {.pure.} = enum
  # Float literals are now parsed as field accesses into an int
  # (i.e every int has an infinite number of fields, each named for an int)
  # FloatLit = "FLOATLIT"
  Access = "."
  Add = "+"
  AddAssign = "+="
  Alias = "alias"
  And = "and"
  Array = "array"
  AShr = ">>>"
  Assert = "assert"
  Assign = "="
  BitAnd = "&"
  BitAndAssign = "&="
  BitNot = "~"
  BitNotAssign = "~="
  BitOr = "|"
  BitOrAssign = "|="
  BitXor = "^"
  BitXorAssign = "^="
  Break = "break"
  CharLit = "CHARLIT"
  ClosedRange = "..="
  Comptime = "comptime"
  Const = "const"
  CVar = "cvar"
  Div = "/"
  DivAssign = "/="
  Until = "until"
  Enum = "enum"
  Equal = "=="
  Field = "field"
  For = "for"
  Greater = ">"
  GreaterEq = ">="
  If = "if"
  In = "in"
  Inline = "inline"
  IntLit = "INTLIT"
  Less = "<"
  LessEq = "<="
  Let = "let"
  Loop = "loop"
  Mod = "%"
  Mul = "*"
  MulAssign = "*="
  NotEqual = "!="
  NotIn = "notin"
  Not = "not"
  NullLit = "null"
  OpenRange = ".."
  Optional = "?"    # These shall be actual operators
  Or = "or"
  Overload = "overload"
  Property = "property"
  Pure = "pure"
  Return = "return"
  Shl = "<<"
  ShlAssign = "<<="
  Shr = ">>"
  ShrAssign = ">>="
  Sink = "_"
  Slice = "slice"
  Spaceship = "<=>" # Spaceship only tentative
  StringLit = "STRLIT"
  Struct = "struct"
  Sub = "-"
  SubAssign = "-="
  Symbol = "SYM"
  Test = "test" # TODO
  Undef = "undef"
  Use = "use"
  Var = "var"
  Void = "void" # Can't be a symbol since we need to be able to do 'fn -> void' without a bind
  While = "while"
  Xor = "xor"

  
  # Punctuation
  # These don't map (directly) to any actual commands, and only serve to disambiguate the syntax
  StoreIn = "->"
  RParen = ")"
  Pound = "#"
  LParen = "("
  Label = "LABEL"
  Fn = "fn"
  Finally = "finally"
  EOF = "EOF"
  Comma = ","
  ElIf = "elif"
  Else = "else"
  LBrace = "{"
  LBracket = "["
  RBrace = "}"
  RBracket = "]"
  Semicolon = ";"
  Separator = ":"
  


  INVALID_TAG = "INVALID"
