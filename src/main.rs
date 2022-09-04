#![allow(dead_code)]
mod error;
mod interpreter;
mod lexer;
mod parser;

use std::env;
// use std::{env, fs};

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    // let code = fs::read_to_string("syntax.ch").expect("Something went wrong reading the file");
    let code = "1 * 1 + (2 * 2 == 4)".to_string();

    let mut tokens = lexer::lex_tokens(&code);

    lexer::print_tokens(&code, &tokens);

    let errors = tokens
        .iter()
        .filter(|tok| matches!(tok.typ, lexer::TokenType::Error));

    // println!("n_errors: {}", errors.count());

    for err in errors {
        println!("{}", error::underline_code(&code, &err.range));
    }

    tokens = lexer::filter_tokens(tokens);

    if let Some(ast) = parser::parse_tokens(tokens) {
        println!("{:?}", ast.typ);

        println!("result: {}", interpreter::visit_node(&ast));
    }
}
