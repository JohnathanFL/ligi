#![feature(stmt_expr_attributes)]
mod ast;
mod lexer;
mod parser;

use std::fs::read_to_string;


fn main() {
    let input = read_to_string("example.li").unwrap();
    let mut lex = lexer::Lexer::from(0, &input);
    while let Ok(r) = lex.lex() {
      println!("{:#?}", r);
      if r.tag == lexer::Tag::EOF { break; }
    }
}
