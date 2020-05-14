use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::str::from_utf8;
#[rustfmt::skip]

#[derive(Debug, Clone, PartialEq)]
pub enum Tag<'a> {
  // Values
  Word(&'a str), Label(&'a str),
  // Needs to be a full string so we can hold multiple lines of MLStrings
  StrLit(String),
  Void,

  // Operators
  //// AssgOp
  Assg, AddAssg, SubAssg, MulAssg, DivAssg,
	 // | BitOrAssg | BitAndAssg | ShlAssg | ShrAssg
  //// BinOps
  Eq, NotEq,
  Spaceship, GreaterEq, LessEq, Less, Greater,
  Or, Xor,
  And,
  NotIn, In,
  ClosedRange, OpenRange, 
  Add, Sub,
  Mul, Div, Mod,
  BitOr, BitAnd, BitXor,
  //// UnaOps
  BitNot, Not,
  Const, Comptime,
  Array, Slice, Opt,
  Pure, Inline, Struct, Enum, Overload, Property,

  // `#`
  Tag,

  Let, Var, CVar, Field, Alias, Use,

  Descend, Pass,
  
  // Punctuation
  EOF,
  //// Open/Close
  LParen, RParen, LBrace, RBrace, LBracket, RBracket,
  //// Common
  StoreIn, Comma, Semicolon, Colon,
  //// Control Structures
  If, When, Is, ElIf, Else, Finally,
  Loop, While, Until, For,
  Defer, Return, Break, Assert, Test,
  Fn
}

// TODO: Properly implement Ord
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct FilePos(pub usize, pub usize, pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
  pub tag: Tag<'a>,
  pub pos: FilePos,
}

#[derive(Debug, Copy, Clone)]
pub struct Lexer<'a> {
  pub input: &'a [u8],
  pub pos: FilePos,
}


impl<'a> Lexer<'a> {
  pub fn peek(&mut self) -> u8 {
    if self.input.len() > 0 {
      return self.input[0];
    } else {
      return 0;
    }
  }
  pub fn munch(&mut self) -> u8 {
    let res = self.peek();
    if self.input.len() > 0 {
      self.input = &self.input[1..];
      if res == b'\n' {
        self.pos.1 += 1;
        self.pos.2 = 1;
      } else {
        self.pos.2 += 1;
      }
    }
    return res;
  }

  pub fn from(fid: usize, s: &'a str) -> Lexer<'a> {
    Lexer {
      input: s.as_bytes(),
      pos: FilePos(fid, 1, 1),
    }
  }

  pub fn is_wordthy(u: u8) -> bool {
    return u.is_ascii_alphanumeric() || u == b'_' || u == b'@';
  }

  pub fn skip_ws(&mut self) {
    while self.peek().is_ascii_whitespace() {
      self.munch();
    }
  }

  pub fn munch_word(&mut self) -> &'a str {
    let resStr = self.input;
    let mut len = 1 as usize;
    while Self::is_wordthy(self.peek()) {
      len += 1;
      self.munch();
    }
    return from_utf8(&resStr[0..len - 1]).expect("How'd we get here?");
  }

  pub fn lex(&mut self) -> Result<Token<'a>, String> {
    self.skip_ws();

    let mut res = Token {
      tag: Tag::Semicolon,
      pos: self.pos,
    };

    macro_rules! err {
      ($msg:expr) => {
        return Err(format!("{:?}: {}", res.pos, $msg));
      };
    }

    // TODO: Refactor this so it's inside the match
    if Self::is_wordthy(self.peek()) {
      let s = self.munch_word();
      res.tag = Tag::Word(s);
      // TODO: Extract this out with lazy_static
      let keywords: BTreeMap<&'static str, Tag<'a>> = BTreeMap::from_iter(vec![
        ("alias", Tag::Alias),
        ("and", Tag::And),
        ("array", Tag::Array),
        ("assert", Tag::Assert),
        ("break", Tag::Break),
        ("comptime", Tag::Comptime),
        ("const", Tag::Const),
        ("cvar", Tag::CVar),
        ("until", Tag::Until),
        ("elif", Tag::ElIf),
        ("else", Tag::Else),
        ("enum", Tag::Enum),
        ("field", Tag::Field),
        ("finally", Tag::Finally),
        ("fn", Tag::Fn),
        ("for", Tag::For),
        ("if", Tag::If),
        ("in", Tag::In),
        ("notin", Tag::NotIn),
        ("inline", Tag::Inline),
        ("let", Tag::Let),
        ("loop", Tag::Loop),
        ("not", Tag::Not),
        ("is", Tag::Is),
        ("property", Tag::Property),
        ("pure", Tag::Pure),
        ("return", Tag::Return),
        ("slice", Tag::Slice),
        ("struct", Tag::Struct),
        ("test", Tag::Test),
        ("use", Tag::Use),
        ("var", Tag::Var),
        ("overload", Tag::Overload),
        ("void", Tag::Void),
        ("while", Tag::While),
        ("xor", Tag::Xor),
        ("when", Tag::When),
      ]);

      if let Some(tag) = keywords.get(&s) {
        res.tag = tag.clone();
      }

      return Ok(res);
    }

    #[rustfmt::skip]
    res.tag = match self.munch() {
      0 => Tag::EOF,
      b'{' => Tag::LBrace,
      b'}' => Tag::RBrace,
      b'(' => Tag::LParen,
      b')' => Tag::RParen,
      b'[' => Tag::LBracket,
      b']' => Tag::RBracket,
      b':' => match self.peek() {
        b':' => {
          self.munch();
          Tag::Pass
        }
        _ => Tag::Colon,
      },
      b'%' => Tag::Mod,
      b'?' => Tag::Opt,
      b',' => Tag::Comma,
      b'-' => match self.peek() {
        b'-' => {
          // Comment
          while self.munch() != b'\n' {}
          return self.lex();
        }
        b'=' => {
          self.munch();
          Tag::SubAssg
        }
        _ => Tag::Sub,
      },
      // TODO: Extract these into a custom macro
      b'=' => match self.peek() {
        b'=' => {
          self.munch();
          Tag::Eq
        }
        _ => Tag::Assg,
      },
      b'+' => match self.peek() {
        b'=' => {
          self.munch();
          Tag::AddAssg
        }
        _ => Tag::Add,
      },
      b'*' => match self.peek() {
        b'=' => {
          self.munch();
          Tag::MulAssg
        }
        _ => Tag::Mul,
      },
      b'/' => match self.peek() {
        b'=' => {
          self.munch();
          Tag::DivAssg
        }
        _ => Tag::Div,
      },
      b'.' => match self.peek() {
        b'.' => {
          self.munch();
          match self.peek() {
            b'=' => {
              self.munch();
              Tag::ClosedRange
            }
            _ => Tag::OpenRange,
          }
        }
        _ => Tag::Descend,
      },
      b'>' => match self.peek() {
        b'=' => {
          self.munch();
          Tag::GreaterEq
        }
        _ => Tag::Greater,
      },
      b'<' => match self.peek() {
        b'=' => {
          self.munch();
          Tag::LessEq
        }
        _ => Tag::Less,
      },
      b'!' => match self.munch() {
        b'=' => Tag::NotEq,
        _ => err!("`=` must follow a `!`. Ligi uses `not` for its unary not operator."),
      },
      b'`' => Tag::Label(self.munch_word()),
      b'#' => Tag::Tag,
      b'"' => {
        let resStr = self.input;
        let mut len = 0 as usize;

        while self.peek() != b'"' {
          let cur = self.peek();
          self.munch();
          len += 1;

          if cur == 0 {
            err!("Expected \", found EOF");
          }
          if cur == b'\n' {
            err!("Newline terminated string at");
          }
          if cur == b'\\' {
            len += 1;
            self.munch();
          }
        }
        self.munch(); // Eat the ending "
        if let Ok(s) = from_utf8(&resStr[0..len]) {
          Tag::StrLit(String::from(s))
        } else {
          err!("Invalid UTF-8 string!");
        }
      }
      b'\\' => {
        // TODO: This doesn't handle EOF terminated MLStrings
        if self.munch() != b'\\' {
          err!("Only valid continuation of \\ is `\\\\ `");
        }
        self.munch(); // Always eat the first char after `\\`.
        let mut resStr = self.input;
        let mut resString = String::new();
        let mut len = 0 as usize;

        while self.peek() != b'\n' {
          self.munch();
          len += 1;
        }
        resString.push_str(from_utf8(&resStr[0..len]).unwrap());

        loop {
          self.skip_ws();
          if self.peek() != b'\\' {
            break;
          } else {
            self.munch();
          }

          if self.munch() != b'\\' {
            err!("Only valid continuation of \\ is `\\\\ `");
          }
          self.munch(); // As always, skip the first character after `\\`
          resStr = self.input;
          len = 0;

          while self.peek() != b'\n' {
            self.munch();
            len += 1;
          }
          resString.push('\n'); // Always add a newline if the user used a newline
          resString.push_str(from_utf8(&resStr[0..len]).unwrap());
        }

        Tag::StrLit(resString)
      }

      _ => err!("Failed!"),
    };


    return Ok(res);
  }
}
