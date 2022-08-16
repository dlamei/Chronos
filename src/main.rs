#![allow(dead_code)]

mod parser;
mod tokens;

use crate::parser::*;
use crate::tokens::*;
use std::{env, fs};
fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let code = fs::read_to_string("syntax.ch").expect("Something went wrong reading the file");

    let tokens = lex_tokens(&code);

    print_tokens(&code, &tokens);

    let n_erros = tokens
        .iter()
        .filter(|(tok, _)| matches!(tok, TokenType::Error))
        .count();

    println!("nErrors: {}", n_erros);
}
