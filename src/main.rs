#![allow(dead_code)]
mod chvalue;
mod error;
mod interpreter;
mod lexer;
mod parser;


use std::env;
// use std::{env, fs};

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    // let code = &fs::read_to_string("syntax.ch").expect("Something went wrong reading the file");
    // use chvalue::ChValue::*;
    // println!("{:?}", String("a".to_owned()).ge(Char('a')));
    // return;

    let code = "{exp = {b = 2; b = b + 1; b}; exp()}()";

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

fn run_code(c: &str) -> Result<chvalue::ChValue, interpreter::RuntimeErr> {
    let (mut tokens, _) = lexer::lex_tokens(&c);
    lexer::filter_tokens(&mut tokens);
    let ast = parser::parse_tokens(tokens);
    interpreter::interpret(&ast)
}

#[test]
fn basic_code() {
    use chvalue::ChValue::*;
    assert_eq!(run_code("1 + 2 * 3").unwrap(), I32(7));
    assert_eq!(run_code("1 + 2 * (3 + 2) == 11").unwrap(), Bool(true));
    assert_eq!(run_code("1 + 2 * (3 + 2) == 10").unwrap(), Bool(false));
    assert_eq!(run_code("1 + 2 * (3 + 2) != 10").unwrap(), Bool(true));
    assert_eq!(run_code("1 + - 2 * ((3 + 2) == 5)").unwrap(), I32(-1));
    assert_eq!(run_code("1 >= 3").unwrap(), Bool(false));
    assert_eq!(run_code("1 > 1").unwrap(), Bool(false));
    assert_eq!(run_code("1.1 >= 1").unwrap(), Bool(true));
    assert_eq!(run_code("1.1 <= 1").unwrap(), Bool(false));
    assert_eq!(run_code("!(false)").unwrap(), Bool(true));
    assert_eq!(run_code("(!false) * 3.4").unwrap(), F32(3.4));
    assert_eq!(run_code("{a = (3 == 2) + 2 * -1.52;;; a}()").unwrap(), F32(-3.04));
}
