#pragma once

#include <iostream>
#include <set>
#include <sstream>

enum class Tag {
  // Punctuation
  LBrace = 0,
  RBrace,
  LBracket,
  RBracket,
  LParen,
  RParen,
  Store,  // ->
  Colon,
  Semicolon,
  Comma,
  Dot,

  ///// OPERATORS

  // Assertions
  // These are parsed at the same level as assignments
  Assert,  // 'assert'

  // Assignment Operators
  Assign,
  AddAssign,
  SubAssign,
  MulAssign,
  DivAssign,
  ModAssign,
  ShlAssign,
  ShrAssign,

  // Standard expression ops
  Add,
  Sub,
  Mul,
  Div,
  Mod,

  // Logical
  Not,
  And,
  Or,
  Xor,

  // Relational
  Eq,
  NotEq,
  Less,
  Greater,
  LessEq,
  GreaterEq,

  // Range
  In,
  OpenRange,
  ClosedRange,

  // Bit twiddlers
  BitNot,
  BitAnd,
  BitOr,
  BitXor,

  // Code-mods
  Array,  // These 2 since [] is ambiguous with a compound literal.
  Slice,
  Comptime,
  Pure,
  Inline,
  Optional,  // ?
  // Pointer should also be here, but it's the same as Mul

  // Binds
  Let,
  Var,
  CVar,
  Field,
  Enum,
  Property,
  Alias,  // New one a simple alias. The AST equivalent of a pointer

  // Typedefs
  StructDef,
  EnumDef,

  // Control Flow
  If,
  ElIf,
  Else,
  For,
  While,
  Loop,
  Finally,
  Switch,  // Not specced yet, but planned.
  Fn,
  Break,
  Return,

  Label,
  Symbol,
  Sink,  // '_'

  // Literals
  StringLit,  // This string is allocated apart from the file (TODO)
  CharLit,
  BoolLit,   // Reserves both 'true' and 'false'
  IntLit,    // If it can't fit in a usize, it shouldn't be a literal.
  FloatLit,  // See above
};

const std::string TagToStr[] = {
  "{",   // LBrace,
  "}",   // RBrace,
  "[",   // LBracket,
  "]",   // RBracket,
  "(",   // LParen,
  ")",   // RParen,
  "->",  // Store
  ":",   // Colon,
  ";",   // Semicolon,
  ",",   // Comma,
  ".",   // Dot,

  ///// OPERATORS

  // Assertions
  // These are parsed at the same level as assignments
  "assert",  // Assert

  // Assignment Operators
  "=",    // Assign,
  "+=",   // AddAssign,
  "-=",   // SubAssign,
  "*=",   // MulAssign,
  "/=",   // DivAssign,
  "%=",   // ModAssign,
  "<<=",  // ShlAssign,
  ">>=",  // ShrAssign,

  // Standard expression ops
  "+",  // Add,
  "-",  // Sub,
  "*",  // Mul,
  "/",  // Div,
  "%",  // Mod,

  // Logical
  "not",  // Not,
  "and",  // And,
  "or",   // Or,
  "xor",  // Xor,

  // Relational
  "==",  // Eq,
  "!=",  // NotEq,
  "<",   // Less,
  ">",   // Greater,
  "<=",  // LessEq,
  ">=",  // GreaterEq,

  // Range
  "in",   // In,
  "..",   // OpenRange,
  "..=",  // ClosedRange,

  // Bit twiddlers
  "~",  // BitNot,
  "&",  // BitAnd,
  "|",  // BitOr,
  "^",  // BitXor,

  // Code-mods
  "array",     // Array, // These 2 since [] is ambiguous with a compound literal.
  "slice",     // Slice,
  "comptime",  // Comptime,
  "pure",      // Pure,
  "inline",    // Inline,
  "?",         // Optional, // ?
  // Pointer should also be , //here, but it's the same as Mul

  // Binds
  "let",       // Let,
  "var",       // Var,
  "cvar",      // CVar,
  "field",     // Field,
  "enum",      // Enum,
  "property",  // Property,
  "alias",     // Alias, // New one a simple alias. The AST equivalent of a pointer

  // Typedefs
  "structdef",  // StructDef,
  "enumdef",    // EnumDef,

  // Control Flow
  "if",       // If,
  "elif",     // ElIf,
  "else",     // Else,
  "for",      // For,
  "while",    // While,
  "loop",     // Loop,
  "finally",  // Finally,
  "",         // Switch, // Not specced yet, but planned.
  "fn",       // Fn,
  "break",    // Break,
  "return",   // Return,

  "@@LABEL@@",   // Label,
  "@@SYMBOL@@",  // Symbol,
  "_",           // Sink, // '_'

  // Literals
  "@@STRLIT@@",    // StringLit, // This string is allocated apart from the file (TODO)
  "@@CHARLIT@@",   // CharLit,
  "@@BOOLLIT@@",   // BoolLit, // Reserves both 'true' and 'false'
  "@@INTLIT@@",    // IntLit, // If it can't fit in a usize, it shouldn't be a literal.
  "@@FLOATLIT@@",  // FloatLit, // See above
};

constexpr const std::string& ToString(const Tag& tag) { return TagToStr[(unsigned)tag]; }

static const std::set<Tag> ALL_OPERATORS = {
  Tag::Assign,    Tag::AddAssign, Tag::SubAssign, Tag::MulAssign, Tag::DivAssign, Tag::ModAssign, Tag::ShlAssign,
  Tag::ShrAssign, Tag::Add,       Tag::Sub,       Tag::Mul,       Tag::Div,       Tag::Eq,        Tag::NotEq,
  Tag::Less,      Tag::Greater,   Tag::LessEq,    Tag::GreaterEq, Tag::Mod,       Tag::OpenRange, Tag::ClosedRange,
  Tag::BitNot,    Tag::BitAnd,    Tag::BitOr,     Tag::BitXor,    Tag::Optional,  Tag::LBrace,    Tag::RBrace,
  Tag::LBracket,  Tag::RBracket,  Tag::LParen,    Tag::RParen,    Tag::Store,     Tag::Colon,     Tag::Semicolon,
  Tag::Comma,     Tag::Dot,
};
static const std::set<Tag> ALL_KEYWORDS = {
  Tag::Assert, Tag::Not,      Tag::And,      Tag::Or,     Tag::Xor,       Tag::In,      Tag::Array,
  Tag::Slice,  Tag::Comptime, Tag::Pure,     Tag::Inline, Tag::Let,       Tag::Var,     Tag::CVar,
  Tag::Field,  Tag::Enum,     Tag::Property, Tag::Alias,  Tag::StructDef, Tag::EnumDef, Tag::If,
  Tag::ElIf,   Tag::Else,     Tag::For,      Tag::While,  Tag::Loop,      Tag::Finally, Tag::Switch,
  Tag::Fn,     Tag::Break,    Tag::Return,   Tag::Label,  Tag::Sink,
};

struct Token {

  Token() {
    
  }

  ~Token() {
    switch(this->tag) {
      case Tag::Symbol:
      case Tag::StringLit:
      case Tag::CharLit:
        this->stringLit.~basic_string();
        break;
      default:
        break;
    }
  }
  
  Tag tag;
  union {
    std::string symbol;
    std::string stringLit, charLit;
    bool        boolLit;
    /// Either FloatLit or IntLit
    struct {
      long unsigned whole, decimal;
    } numLit;
  };
};

std::istream& operator>>(std::istream& in, Token& tok) { 
  auto doneSeeking = false;
  while(!doneSeeking) {
    doneSeeking = true;

    // Skip all whitespace
    while (std::isspace(in.peek())) in.get();

    // Skip comments
    if (in.peek() == '(') {
      in.get();
      if (in.peek() == ':' || in.peek() == '=') {
        // If it was a comment, we need to re-skip whitespace before trying to skip another comment.
        doneSeeking = false;
        while (in.peek() != '\n') in.get();
      } else {
        // If it wasn't, we've already consumed the '(', so we need to handle that specially.
        tok.tag = Tag::LParen;
        return in;
      }
    }
  }

  
  if (std::isdigit(in.peek())) {
    in >> tok.numLit.whole;
    if (in.peek() == '.') {
      tok.tag = Tag::FloatLit;
      in >> tok.numLit.decimal;
    } else {
      tok.tag            = Tag::IntLit;
      tok.numLit.decimal = 0;
    }
  } else if (std::isalpha(in.peek()) || in.peek() == '@' || in.peek() == '_') {
    std::stringstream lexeme;
    while (std::isalnum(in.peek()) || in.peek() == '@' || in.peek() == '_') lexeme << in.get();

    tok.tag    = Tag::Symbol;
    tok.symbol = lexeme.str();
    for (auto keyword : ALL_KEYWORDS) {
      if (ToString(keyword) == tok.symbol) tok.tag = keyword;
    }
  } else {
    unsigned i = 0;
    auto possibilities = ALL_OPERATORS;
    while(true) {
      auto iter = possibilities.begin();
      while (iter != possibilities.end()) {
        if(ToString(*iter)[i++] != in.peek()) {
          std::cout << "\n\tEliminated " << ToString(*iter);
          iter = possibilities.erase(*iter);
        }
      }
    }
  }


  return in;
}
