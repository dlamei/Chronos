#![allow(dead_code)]
mod chtype;
mod error;
mod interpreter;
mod lexer;
mod parser;

use std::env;
// use std::{env, fs};

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    // let code = fs::read_to_string("syntax.ch").expect("Something went wrong reading the file");
    let code = "2 * * 5".to_string();

    let (mut tokens, err_flag) = lexer::lex_tokens(&code);

    lexer::print_tokens(&code, &tokens);

    if err_flag {
        let errors = tokens
            .iter()
            .filter(|tok| matches!(tok.typ, lexer::TokenType::Error));

        for err in errors {
            println!("Lexer: could not lex char:");
            println!("{}", error::underline_code(&code, &err.range));
        }
        return;
    }

    lexer::filter_tokens(&mut tokens);

    let ast = parser::parse_tokens(tokens);
    println!("{:?}", ast);

    println!("{}", ast.flags.contains(parser::NodeFlags::ERROR));

    if ast.flags.contains(parser::NodeFlags::ERROR) {
        parser::print_errors(&ast, &code);
    }
}
