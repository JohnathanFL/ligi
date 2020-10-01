use crate::bimap::BiMap;

pub type StrID = usize;
pub struct StrCache(usize, BiMap<StrID, String>);
impl StrCache {
  pub fn new() -> StrCache {
    StrCache(0, BiMap::new())
  }

  pub fn str_id<S: Into<String>>(&mut self, s: S) -> StrID {
    let s = s.into();
    if let Some(i) = self.1.get_by_right(&s) { *i }
    else {
      self.1.insert(self.0, s);
      let res = self.0;
      self.0 += 1;

      res
    }

  }

  pub fn id_str(&self, id: StrID) -> &String {
    self
      .1
      .get_by_left(&id)
      .expect("Somehow we have a rogue StrID in the program!")
  }

  pub fn pretty(&self, tok: Token) -> String {
    match tok {
      Token::Strop(s) => format!("Strop(\\`{}`)", self.id_str(s)),
      Token::Word(s) => format!("Word(`{}`)", self.id_str(s)),
      Token::Sigil(s) => format!("Sigil(\\`{}`)", self.id_str(s)),
      Token::Tag(s) => format!("Tag(#`{}`)", self.id_str(s)),

      Token::Punctuation(s) => format!("Punctuation(`{}`)", self.id_str(s)),

      Token::Str(s) => format!("Str(`{}`)", self.id_str(s)),

      _ => format!("{:?}", tok),
    }

  }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token {
  Strop(StrID),
  Word(StrID),
  Sigil(StrID),
  Tag(StrID),

  Punctuation(StrID),

  Str(StrID),

  Indent,
  Dedent,
  Newline,
  EOF,
}
