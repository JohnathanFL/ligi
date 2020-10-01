#![feature(or_patterns)]

extern crate bimap;
use std::cell::RefCell;

/// Parsing stuff common to any possible lexer/parser
pub mod words;
/// Ligi's lexer
pub mod lexing;
use words::{StrCache, StrID, Token};
use lexing::{Lexer};


fn main() {
  let input = include_str!("../example.li");
  let cache = RefCell::new(StrCache::new());
  let commentor = cache.borrow_mut().str_id("--");
  let mut lexer = Lexer::new(&cache, input.chars(), commentor);

  loop {
    let tok = lexer.scan();
    println!("{}", cache.borrow().pretty(tok));
    if tok == Token::EOF {
      break;
    }
  }
}
