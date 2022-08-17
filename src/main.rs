#![allow(dead_code)]

mod lexer;
mod parser;
mod error;

use crate::lexer::*;
use std::{env, fs};

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let code = fs::read_to_string("syntax.ch").expect("Something went wrong reading the file");

    let tokens = lex_tokens(&code);

    print_tokens(&code, &tokens);

    let errors = tokens
        .iter()
        .filter(|tok| matches!(tok.typ, TokenType::Error));

    println!("n_errors: {}", errors.count());
}
