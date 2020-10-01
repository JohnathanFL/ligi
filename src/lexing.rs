use std::cell::RefCell;

use crate::words::{StrCache, StrID, Token};

pub struct Lexer<'a, I: Iterator<Item = char>> {
  pub cache: &'a RefCell<StrCache>,

  /// Did we just skip_ws that brought us to a new line?
  /// This flag is kept on when emitting a Dedent token, causing us to always
  /// emit a Newline after 1+ Dedents
  newlined: bool,
  pub cur: char,

  /// Zero-based
  pub line: usize,
  /// Zero-based
  pub col: usize,

  /// Each usize is the indent level for that block
  /// If empty, the indentation is 0
  pub indents: Vec<usize>,
  /// The StrID of a Word or Sigil that begins a comment.
  pub commentor: StrID,

  pub input: I,
}

fn is_sigil(c: char) -> bool {
  match c {
    '~' | '!' | '$' | '%' | '^' | '&' | '*' | '-' | '+' | '=' | '<' | '>' | '.' | '?' | '/'
    | '|' | ':' => true,
    _ => false,
  }
}

fn is_word(c: char) -> bool {
  match c {
    '@' | '_' => true,
    c if c.is_ascii_alphanumeric() => true,
    _ => false,
  }
}

impl<'a, I: Iterator<Item = char>> Lexer<'a, I> {
  pub fn new(cache: &'a RefCell<StrCache>, mut input: I, commentor: StrID) -> Lexer<'a, I> {
    Lexer {
      cache,
      newlined: false,
      cur: input.next().unwrap_or('\0'),
      line: 0,
      col: 0,
      indents: Vec::new(),
      commentor,
      input,
    }
  }

  fn cur_dent(&self) -> usize {
    if self.indents.is_empty() { 0 }
    else { self.indents[self.indents.len() - 1] }
  }

  fn push_dent(&mut self) {
    println!("Indenting {}->{}", self.cur_dent(), self.col);
    self.indents.push(self.col);
  }

  fn pop_dent(&mut self) {
    self.indents.pop();
  }
  
  fn advance(&mut self) -> char {
    let old = self.cur;
    self.cur = if let Some(c) = self.input.next() {
      c
    } else {
      '\0'
    };

    if old != '\0' {
      self.col += 1;
      if old == '\n' {
        self.line += 1;
        self.col = 0;
      }
    }


    old
  }

  fn skip_ws(&mut self) {
    while self.cur.is_ascii_whitespace() {
      let skipped = self.advance();
      if skipped == '\n' { self.newlined = true; }
    }
  }

  fn scan_until(&mut self, cond: fn(char) -> bool) -> StrID {
    let mut res = String::new();
    loop {
      res.push(self.advance());
      if !cond(self.cur) {
        break;
      }
      if self.cur == '\0' {
        break;
      }
    }
    self.cache.borrow_mut().str_id(res)
  }

  fn scan_single(&mut self) -> StrID {
    self.scan_until(|_| false)
  }

  fn scan_quoted(&mut self) -> StrID {
    self.advance();
    let res = self.scan_until(|c| c == '"');
    self.advance();
    res
  }
  fn scan_word(&mut self) -> StrID {
    self.scan_until(|c| is_word(c))
  }
  fn scan_sigil(&mut self) -> StrID {
    self.scan_until(|c| is_sigil(c))
  }

  pub fn scan(&mut self) -> Token {
    self.skip_ws();
    if self.newlined {
      if self.col == self.cur_dent() {
        self.newlined = false;
        return Token::Newline;
      } else if self.col > self.cur_dent() {
        self.newlined = false;
        self.push_dent();
        return Token::Indent;
      } else if self.col < self.cur_dent() {
        self.pop_dent();
        // if self.col == self.cur_dent() { self.newlined = false; }
        return Token::Dedent;
      }
    }

    let res = match self.cur {
      '\0' => Token::EOF,
      '{' | '}' | '(' | ')' | '[' | ']' | ',' | ';' => Token::Punctuation(self.scan_single()),
      c if is_word(c) => Token::Word(self.scan_word()),
      c if is_sigil(c) => Token::Sigil(self.scan_sigil()),
      '\\' => {
        self.advance();
        match self.cur {
          c if is_word(c) => Token::Strop(self.scan_word()),
          c if is_sigil(c) => Token::Strop(self.scan_sigil()),
          '"' => Token::Strop(self.scan_quoted()),
          '\\' => Token::Str(self.scan_until(|c| c == '\n')),
          _ => panic!("Expected a word, sigil, quoted string, or another `\\`."),
        }
      },
      _ => panic!("Expected punctuation, a word, a sigil, a string, or a stropping."),
    };

    match res {
      Token::Word(s) | Token::Sigil(s) if s == self.commentor => {
        while self.cur != '\n' { self.advance(); }
        return self.scan();
      },
      _ => return res,
    }
  }
}
