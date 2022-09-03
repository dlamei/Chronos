#![allow(dead_code)]
mod error;
mod lexer;
mod parser;

use crate::lexer::*;
use std::{env, fs};

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    // let code = fs::read_to_string("syntax.ch").expect("Something went wrong reading the file");
    let code = "1 + 2 * 3".to_string();

    let mut tokens = lex_tokens(&code);

    print_tokens(&code, &tokens);

    let errors = tokens
        .iter()
        .filter(|tok| matches!(tok.typ, TokenType::Error));

    // println!("n_errors: {}", errors.count());

    for err in errors {
        println!("{}", error::underline_code(&code, &err.range));
    }

    tokens = filter_tokens(tokens);

    parser::parse_tokens(tokens);
}
