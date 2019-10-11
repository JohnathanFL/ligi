const std = @import("std");
const isAlpha = std.ascii.isAlpha;
const isAlNum = std.ascii.isAlNum;
const isDigit = std.ascii.isDigit;
const isSpace = std.ascii.isSpace;
const eql = std.mem.eql;

const testing = std.testing;
const expect = testing.expect;

const Tokens = @import("token.zig");
pub const Token = Tokens.Token;
pub const LexVal = Tokens.LexVal;
pub const Tag = Tokens.Tag;

pub const Lexer = struct {
  input: []const u8,
  line: usize,
  inline fn curChar(self: Lexer) u8 {
    return self.input[0];
  }

  inline fn advance(self: *Lexer) u8 {
    var cur = self.curChar();
    self.input = self.input[1..];
    return cur;
  }
  inline fn advanceBy(self: *Lexer, n: usize) []const u8 {
    var res = self.input[0..n];
    self.input = self.input[n..];
    return res;
  }

  inline fn tokenOfLen(self: *Lexer, n: usize, tag: Tag) Token {
    return Token{
      .tag = tag,
      .lexeme = self.advanceBy(n),
      .val = .DoesntMatter,
    };
  }

  fn atoi(str: []const u8) i128 {
    var res: i128 = 0;

    for (str) |c| {
      res *= 10; // shl by 1 in base 10
      res += @intCast(i128, c - '0');
    }

    return res;
  }

  fn nextEql(self: Lexer, str: []const u8) bool {
    if (str.len > self.input.len)
      return false;
    return eql(u8, str, self.input[0..str.len]);
  }

  pub fn scan(self: *Lexer) ?Token {
    var res: Token = undefined;

    while (isSpace(self.curChar())) : (_ = self.advance()) {
      if (self.curChar() == '\n')
        self.line += 1;
    }

    if (isAlpha(self.curChar())) {
      var i: usize = 0;
      while (isAlNum(self.input[i]) or self.input[i] == '_') : (i += 1) {}

      res = Token{
        .tag = .Symbol,
        .lexeme = self.advanceBy(i),
        .val = .DoesntMatter,
      };

      if (eql(u8, res.lexeme, "if")) { // Word operators
        res.tag = .If;
      } else if (eql(u8, res.lexeme, "while")) {
        res.tag = .While;
      } else if (eql(u8, res.lexeme, "for")) {
        res.tag = .For;
      } else if (eql(u8, res.lexeme, "or")) {
        res.tag = .Or;
      } else if (eql(u8, res.lexeme, "and")) {
        res.tag = .And;
      } else if (eql(u8, res.lexeme, "true")) {
        res.tag = .BoolLit;
        res.val = LexVal{ .BoolLit = true };
      } else if (eql(u8, res.lexeme, "false")) {
        res.tag = .BoolLit;
        res.val = LexVal{ .BoolLit = false };
      }
    } else if (isDigit(self.curChar())) {
      var i: usize = 0;

      // TODO: FloatLit
      while (isDigit(self.input[i])) : (i += 1) {}

      res = Token{
        .tag = .IntLit,
        .lexeme = self.advanceBy(i),
        .val = undefined,
      };

      res.val = LexVal{ .IntLit = atoi(res.lexeme) };
    } else { // true operators
      switch (self.curChar()) {
        '+' => {

          // NOTE: We must compare in descending length
          // Otherwise nextEql("++") is also true for input = "+++"
          if (self.nextEql("+++")) {
            res = self.tokenOfLen(3, .IncNow);
          } else if (self.nextEql("++")) {
            res = self.tokenOfLen(2, .Inc);
          } else if (self.nextEql("+=")) {
            res = self.tokenOfLen(2, .AddAssign);
          } else {
            res = self.tokenOfLen(1, .Add);
          }
        },
        '-' => {
          if (self.nextEql("---")) {
            res = self.tokenOfLen(3, .DecNow);
          } else if (self.nextEql("--")) {
            res = self.tokenOfLen(2, .Dec);
          } else if (self.nextEql("-=")) {
            res = self.tokenOfLen(2, .SubAssign);
          } else {
            res = self.tokenOfLen(1, .Sub);
          }
        },
        '*' => {
          if (self.nextEql("*=")) {
            res = self.tokenOfLen(2, .MulAssign);
          } else {
            res = self.tokenOfLen(1, .Mul);
          }
        },
        '/' => {
          if (self.nextEql("/=")) {
            res = self.tokenOfLen(2, .DivAssign);
          } else {
            res = self.tokenOfLen(1, .Div);
          }
        },
        '<' => {
          if (self.nextEql("<<")) {
            res = self.tokenOfLen(2, .Shl);
          } else if (self.nextEql("<=")) {
            res = self.tokenOfLen(2, .LessEq);
          } else {
            res = self.tokenOfLen(1, .Greater);
          }
        },
        '>' => {
          if (self.nextEql(">>")) {
            res = self.tokenOfLen(2, .Shr);
          } else if (self.nextEql(">=")) {
            res = self.tokenOfLen(2, .GreaterEq);
          } else {
            res = self.tokenOfLen(1, .Greater);
          }
        },
        '!' => {
          if (self.nextEql("!=")) {
            res = self.tokenOfLen(2, .NotEqual);
          } else {
            res = self.tokenOfLen(1, .Not);
          }
        },
        '=' => {
          if (self.nextEql("==")) {
            res = self.tokenOfLen(2, .Equal);
          } else {
            res = self.tokenOfLen(1, .Assign);
          }
        },
        '.' => {
          if (self.nextEql(".?")) {
            res = self.tokenOfLen(2, .DotOpt);
          } else {
            res = self.tokenOfLen(1, .Dot);
          }
        },

        ':' => res = self.tokenOfLen(1, .Colon),
        ';' => res = self.tokenOfLen(1, .SemiColon),
        '|' => res = self.tokenOfLen(1, .Pipe),
        '(' => res = self.tokenOfLen(1, .LParen),
        ')' => res = self.tokenOfLen(1, .RParen),
        '[' => res = self.tokenOfLen(1, .LBracket),
        ']' => res = self.tokenOfLen(1, .RBracket),
        '{' => res = self.tokenOfLen(1, .LBrace),
        '}' => res = self.tokenOfLen(1, .RBrace),

        // TODO: Escape codes
        '"' => {
          var i: usize = 1; // 1 to explicitly skip the existing "
          while (self.input[i] != '"') : (i += 1) {}
          i += 1; // To include the '"'

          res = self.tokenOfLen(i, .StringLit);
        },

        // TODO: This doesn't currently handle escaping properly (like '\'')
        '\'' => {
          var i: usize = 1; // 1 to explicitly skip the existing "
          while (self.input[i] != '\'') : (i += 1) {}
          i += 1; // To include the '"'

          res = self.tokenOfLen(i, .CharLit);
        },

        else => {
          std.debug.warn("Invalid character in lexer: {c}\n", self.curChar());
        },
      }
    }
    return res;
  }
};

test "lexer" {
  var input =
    \\ int i = 0;
    \\ i = 1 + 4 * 5;
    \\ while (true and i > 0) : (--i) {
    \\   print("Hello world!");
    \\   print('\n');
    \\ }
  ;

  var lexer = Lexer{
    .input = input,
    .line = 0,
  };

  var tags = [_]Tag{
    .Symbol,  .Symbol,  .Assign,  .IntLit,  .SemiColon,
    .Symbol,  .Assign,  .IntLit,  .Add,   .IntLit, .Mul,   .IntLit,  .SemiColon, 
    .While,   .LParen, .BoolLit, .And,     .Symbol,  .Greater, .IntLit, .RParen,  .Colon,   .LParen,  .Dec,   .IntLit, .RParen,  .LBrace,  
    .Symbol,  .LParen,  .StringLit, .RParen,  .SemiColon, 
    .Symbol,  .LParen,  .CharLit, .RParen,  .SemiColon,
    .RBrace,
  };

  //   std.debug.warn("\n");

  var prevLine = lexer.line;
  for (tags) |tag| {
    var cur = lexer.scan().?;
    if (lexer.line != prevLine) {
      //   std.debug.warn("\n");
      prevLine = lexer.line;
    }
    // std.debug.warn("{} ", cur.tag);
  }
}
