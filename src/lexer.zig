const std = @import("std");
const assert = std.debug.assert;

const ast = @import("ast.zig");

pub const Tag = enum {
  Word, Label, 
  // The .str for these are not guaranteed to be allocated in the main file's .input
  Str, Char,

  Tag, // #
  
  Assg, AddAssg, SubAssg, MulAssg, DivAssg,

  // BinOps (Sorted by prec by line)
  Eq, NotEq, Gt, Lt, GtEq, LtEq, Spaceship,
  Or, Xor,
  And,
  In, NotIn,
  OpenRange, ClosedRange,
  Add, Sub,
  Mul, Div, Mod,
  BitOr, BitAnd, BitXor,

  // UnaOps (No order)
  // Neg is Sub, Enum is in Binds, Ptr is Mul
  Struct, Ref, Slice, Array, Const, Comptime,
  BitNot, Not, Opt, Pure, Inline, Overload, Property,

  // Access ops
  Access, Pipe, // `.` and `::`

  // Binds
  // Note Enum is here, so won't be in the unary ops section
  Using, // Actually a modifier on a bind statement
  Let, Var, CVar, Field, Enum, Alias,

  // Keywords
  Assert, Break, Return, Defer,
  If, ElIf, Else, When, Is, While, Loop, For,
  Finally,
  Fn,

  // Punctuation
  Colon, Comma, StoreIn,
  LParen, RParen,
  LBracket, RBracket,
  LBrace, RBrace,

  EOF,

  
  pub const TagCtx = enum { Binary, Unary };
  pub fn toOp(t: Tag, comptime ctx: TagCtx) ast.Op {
    return switch(t) {
      .Assg => .Assg,
      .AddAssg => .AddAssg,
      .SubAssg => .SubAssg,
      .MulAssg => .MulAssg,
      .DivAssg => .DivAssg,
      .Eq => .Eq,
      .NotEq => .NotEq,
      .Gt => .Gt,
      .Lt => .Lt,
      .GtEq => .GtEq,
      .LtEq => .LtEq,
      .Spaceship => .Spaceship,
      .Or => .Or,
      .Xor => .Xor,
      .And => .And,
      .In => .In,
      .NotIn => .NotIn,
      .OpenRange => .OpenRange,
      .ClosedRange => .ClosedRange,
      .Add => .Add,
      .Sub => if(ctx == .Binary) .Sub else .Neg,
      .Mul => if(ctx == .Binary) .Mul else .Ptr,
      .Div => .Div,
      .Mod => .Mod,
      .BitOr => .BitOr,
      .BitAnd => .BitAnd,
      .BitXor => .BitXor,
      .Struct => .Struct,
      .Ref => .Ref,
      .Slice => .Slice,
      .Array => .Array,
      .Const => .Const,
      .Comptime => .Comptime,
      .BitNot => .BitNot,
      .Not => .Not,
      .Opt => .Opt,
      .Pure => .Pure,
      .Inline => .Inline,
      .Overload => .Overload,
      .Property => .Property,
      else => unreachable,
    };
  }
  pub fn toBindOp(t: Tag) ast.BindOp {
    return switch(t) {
      .Let => .Let, .Var => .Var, .CVar => .CVar, .Field => .Field, .Enum => .Enum,
      else => unreachable
    };
  }
  pub fn toBindLevel(t: Tag) ast.BindLevel {
    return switch(t) {
      .Mul => .Pub,
      .Add => .ReadOnly,
      .Sub => .Priv,
      else => unreachable
    };
  }
};

pub const FilePos = struct {
  file: usize,
  line: usize,
  col: usize,
};

pub const Token = struct {
  tag: Tag,
  pos: FilePos,
  str: [] const u8
};


pub const Lexer = struct {
  // Holds only string lits
  // Note that Lexer does NOT deinit this. That's your job.
  // It's designed like this so you can have many lexers for many files that share
  // an arena.
  alloc: *std.mem.Allocator,
  // Allocated and cleaned by caller.
  input: []const u8,
  pos: FilePos,

  pub fn init(input: []const u8, file: usize, alloc: *std.mem.Allocator) Lexer {
    return .{ .alloc = alloc, .input = input, .pos = .{ .file = file, .line = 1, .col = 1 } };
  }

  // Care must be taken to keep these in sorted order
  // For example, `..=` must come before `..`
  // TODO: Just make this thing get sorted by .len desc once at runtime.
  pub const TokPair = struct { str: []const u8, tag: Tag };
  fn tp(str: []const u8, tag: Tag) TokPair { return .{ .str = str, .tag = tag }; }
  pub const TOKS = [_]TokPair{
    // Keywords
    tp("using", .Using), tp("assert", .Assert), tp("break", .Break),
    tp("defer", .Defer),
    tp("if", .If), tp("elif", .ElIf), tp("else", .Else),
    tp("when", .When), tp("is", .Is), tp("finally", .Finally),
    tp("while", .While), tp("loop", .Loop), tp("for", .For),
    tp("finally", .Finally), tp("fn", .Fn),
    
    tp("let", .Let), tp("var", .Var), tp("cvar", .CVar),
    tp("field", .Field), tp("enum", .Enum),

    tp("struct", .Struct), tp("ref", .Ref), tp("slice", .Slice),
    tp("array", .Array), tp("const", .Const), tp("comptime", .Comptime),
    tp("notin", .NotIn), tp("not", .Not), tp("pure", .Pure),
    tp("inline", .Inline), tp("in", .In), tp("overload", .Overload), tp("property", .Property),
    tp("or", .Or), tp("xor", .Xor), tp("and", .And),
    tp("mod", .Mod), tp("alias", .Alias),
    

    // Non-words
    tp("->", .StoreIn), tp("#", .Tag),
    tp("~", .BitNot), tp("?", .Opt),
    tp("+=", .AddAssg), tp("+", .Add),
    tp("-=", .SubAssg), tp("-", .Sub),
    tp("*=", .MulAssg), tp("*", .Mul),
    tp("/=", .DivAssg), tp("/", .Div),
    tp("..=", .ClosedRange), tp("..", .OpenRange), tp(".", .Access),
    tp("::", .Pipe), tp(":", .Colon),
    tp("<=>", .Spaceship), tp("<=", .LtEq), tp("<", .Lt),
    tp(">=", .GtEq), tp(">", .Gt),
    tp("==", .Eq), tp("=", .Assg), tp("!=", .NotEq),
    tp("|", .BitOr), tp("&", .BitAnd), tp("^", .BitXor),

    tp("(", .LParen), tp(")", .RParen),
    tp("[", .LBracket), tp("]", .RBracket),
    tp("{", .LBrace), tp("}", .RBrace),
    tp(",", .Comma),
  };

  fn hasLen(self: *Lexer, len: usize) bool {
    return self.input.len >= len;
  }
  
  // Assumes hasLen(len)
  // Assumes no newlines in the munched section
  fn munch(self: *Lexer, len: usize) []const u8 {
    self.pos.col += len;
    const res = self.input[0..len];
    self.input = self.input[len..];
    return res;
  }

  fn nextIs(self: *Lexer, str: []const u8) bool {
    if(!self.hasLen(str.len)) return false;
    return std.mem.eql(u8, self.input[0..str.len], str);
  }

  fn match(self: *Lexer, str: []const u8) bool {
    if (self.nextIs(str)) {
      self.pos.col += str.len;
      self.input = self.input[str.len..];
      return true;
    }
    else return false;
  }

  fn isWordChar(c: u8) bool {
    return switch(c) {
      'a'...'z', 'A'...'Z', '0'...'9', '_', '@' => true,
      else => false
    };
  }

  // Assumes that the first char is a wordChar, no matter what
  fn matchWord(self: *Lexer) []const u8 {
    var len: usize = 1;
    while(self.hasLen(len + 1) and isWordChar(self.input[len])) { len += 1; }
    return self.munch(len);
  }


  fn skipWs(self: *Lexer) void {
    // Remember hasLen === hasAtLeastLen
    while(self.hasLen(1) and std.ascii.isSpace(self.input[0])) {
      if (self.input[0] == '\n') {
        self.pos.col = 1;
        self.pos.line += 1;
      } else {
        self.pos.col += 1;
      }
      self.input = self.input[1..];
    }
  }

  pub const Error = error {
    UnknownToken,
    UnTermStr,
    UnTermChar
  };

  fn eof(pos: FilePos) Token {
    return .{
      .tag = .EOF,
      .pos = pos,
      .str = "EOF"
    };
  }
  pub fn lex(self: *Lexer) Error!Token {
    self.skipWs();
    const pos = self.pos;
    
    if(!self.hasLen(1)) return eof(pos);
    
    // Maybe not the most performant, but we'll worry about that later....
    for (TOKS) |pair| {
      if (self.match(pair.str)) {
        return Token{
          .tag = pair.tag,
          .pos = pos,
          .str = pair.str,
        };
      }
    }
    // Wasn't a reserved token
    if(isWordChar(self.input[0])) {
      return Token {
        .tag = .Word,
        .pos = pos,
        .str = self.matchWord()
      };
    }

    if (self.input[0] == '`') {
      return Token {
        .tag = .Label,
        .pos = pos,
        .str = self.matchWord()
      };
    }


    // This and the `\\` sections will allocate with .alloc
    if (self.match("\"")) {
      var len: usize = 0;
      // TODO: Escapes. It'll require allocating memory for it.
      while(self.input.len - len > 0 and self.input[len] != '\n' and self.input[len] != '"') {
        len += 1;
      }
      const res = self.munch(len);
      if(!self.match("\"")) return error.UnTermStr;
      return Token {
        .tag = .Str,
        .pos = pos,
        .str = res,
      };
    }

    if (self.nextIs("\\\\")) {
      // Inner value is "leaked" as .str
      // Gets cleaned up from the 
      var res = std.ArrayList(u8).init(self.alloc);

      while(self.match("\\\\")) {
        if(res.items.len > 0) res.append('\n') catch unreachable;
        var len: usize = 0;
        while(self.input.len-len > 0 and self.input[len] != '\n') {
          res.append(self.input[len]) catch unreachable;
          len += 1;
        }
        _ = self.munch(len);
        _ = self.match("\n");
        self.skipWs();
      }
      return Token {
        .tag = .Str,
        .pos = pos,
        .str = res.toOwnedSlice()
      };
    }

    if (self.match("'")) {
      // TODO: Escapes
      const res = self.munch(1);
      if (!self.match("'")) return error.UnTermChar;
      
      return Token {
        .tag = .Char,
        .pos = pos,
        .str = res
      };
    }

    return error.UnknownToken;
  }
};


test "recognize reserved" {
  const expected = [_]Tag {
    .Assg, .AddAssg, .SubAssg, .MulAssg, .DivAssg,
    .Eq, .NotEq, .Gt, .Lt, .GtEq, .LtEq, .Spaceship,
    .Or, .Xor,
    .And,
    .In, .NotIn,
    .OpenRange, .ClosedRange,
    .Add, .Sub,
    .Mul, .Div, .Mod,
    .BitOr, .BitAnd, .BitXor,
    .Struct, .Ref, .Slice, .Array, .Const, .Comptime,
    .BitNot, .Not, .Opt, .Pure, .Inline, .Overload, .Property,
    .Access, .Pipe,
    .Using,
    .Let, .Var, .CVar, .Field, .Enum, .Alias,
    .If, .ElIf, .Else, .When, .Is, .While, .Loop, .For,
    .Finally,
    .Fn,
    .Colon, .Comma, .StoreIn,
    .LParen, .RParen,
    .LBracket, .RBracket,
    .LBrace, .RBrace,
  };
  const input = \\ = += -= *= /= == != > < >= <= <=> or xor and in notin .. ..=
                \\ + - * / mod | & ^ struct ref slice array const comptime ~ not ?
                \\ pure inline overload property . :: let var cvar field enum alias
                \\ if elif else when is while loop for finally fn : , -> ( ) [ ] { }
                ;
  var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
  var lexer = Lexer.init(input, 0, &arena.allocator);

  //std.debug.warn("\n", .{});
  for(expected) |tag| {
    const res = try lexer.lex();
    //std.debug.warn("{} === {}\n", .{res.Ok.tag, tag});
    assert(res.tag == tag);
  }

}
