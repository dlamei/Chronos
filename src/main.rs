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

    // let code = &fs::read_to_string("syntax.ch").expect("Something went wrong reading the file");
    let code = "1 + -2 * 3.4";

    let (mut tokens, err_flag) = lexer::lex_tokens(&code);

    lexer::print_tokens(&code, &tokens);

    if err_flag {
        let errors = tokens
            .iter()
            .filter(|tok| matches!(tok.typ, lexer::TokenType::Error));

        for err in errors {
            println!("Lexer: could not lex char:");
            println!("{}", error::underline_code(code, &err.range));
        }
        return;
    }

    lexer::filter_tokens(&mut tokens);

    let ast = parser::parse_tokens(tokens);
    println!("{}", ast);

    if ast.flags.contains(parser::NodeFlags::ERROR) {
        parser::print_errors(&ast, code);
        return;
    }

    match interpreter::interpret(&ast) {
        Ok(e) => println!("{}", e),
        Err(e) => interpreter::print_error(e, code),
    };
}

#[test]
fn basic_code() {
    use chtype::ChType::*;
    let mut code: Vec<(&str, chtype::ChType)> = Vec::new();
    code.push(("1 + 2 * 3", I32(7)));
    code.push(("1 + 2 * (3 + 2) == 11", Bool(true)));
    code.push(("1 + 2 * (3 + 2) == 10", Bool(false)));
    code.push(("1 + 2 * (3 + 2) != 10", Bool(true)));
    code.push(("1 + - 2 * ((3 + 2) == 5)", I32(-1)));
    code.push(("1 >= 3", Bool(false)));
    code.push(("1 > 1", Bool(false)));
    code.push(("1.1 >= 1", Bool(true)));
    code.push(("1.1 <= 1", Bool(false)));
    code.push(("!(false)", Bool(true)));
    code.push(("(!false) * 3.4", F32(3.4)));

    for (c, v) in code {
        let (mut tokens, _) = lexer::lex_tokens(&c);
        lexer::filter_tokens(&mut tokens);
        let ast = parser::parse_tokens(tokens);
        let val = interpreter::interpret(&ast);
        assert_eq!(val.unwrap(), v);
    }
}
