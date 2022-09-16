#![allow(dead_code)]
mod chvalue;
mod error;
mod interpreter;
mod lexer;
mod parser;

use std::{env, io};
// use std::{env, fs};

fn main() {
    env::set_var("RUST_BACKTRACE", "1");
    let args: Vec<String> = env::args().collect();

    println!("{:?}", args);

    if args.len() >= 2 && args[1] == "repl" {
        loop {
            let mut code = String::new();
            io::stdin().read_line(&mut code).unwrap();

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
            println!("parser: {}", ast);

            if ast.flags.contains(parser::NodeFlags::ERROR) {
                parser::print_errors(&ast, &code);
                return;
            }

            match interpreter::interpret(&ast) {
                Ok(e) => println!("{}", e),
                Err(e) => interpreter::print_error(e, &code),
            };
        }
    } else {
        // let code = "{a = 3; a = a / 2.0; a}()";
        let code = "{a = 3; a /= 1.5}()";

        let (mut tokens, err_flag) = lexer::lex_tokens(code);

        lexer::print_tokens(code, &tokens);

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
        println!("parser: {}", ast);

        if ast.flags.contains(parser::NodeFlags::ERROR) {
            parser::print_errors(&ast, code);
            return;
        }

        match interpreter::interpret(&ast) {
            Ok(e) => println!("{:?}", e),
            Err(e) => interpreter::print_error(e, code),
        };
    }
}

fn run_code(c: &str) -> Result<chvalue::ChValue, interpreter::RuntimeErr> {
    let (mut tokens, _) = lexer::lex_tokens(c);
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
    assert_eq!(
        run_code("{a = (3 == 2) + 2 * -1.52;;; a}()").unwrap(),
        F32(-3.04)
    );
    assert_eq!(run_code("{a = {2i8 + 3u8}; return a;}()()").unwrap(), I8(5));
    assert_eq!(run_code("{a = 3; {a = a + 2.3}(); a}()").unwrap(), F32(5.3));

    assert_eq!(run_code("{a = 3; a += 2; a}()").unwrap(), I32(5));
    assert_eq!(run_code("{a = 3; a -= 2; a}()").unwrap(), I32(1));
    assert_eq!(run_code("{a = 3; a *= 2; a}()").unwrap(), I32(6));
    assert_eq!(run_code("{a = 3; a /= 2; a}()").unwrap(), I32(1));
    assert_eq!(run_code("{a = 3; a /= 2.0; a}()").unwrap(), F32(1.5));
}

#[test]
fn unry_test() {
    use chvalue::ChValue::*;
    assert_eq!(run_code("1 + - - - !false").unwrap(), I32(0));
}

#[test]
fn eval_lit() {
    use chvalue::ChValue::*;

    assert_eq!(run_code("41").unwrap(), I32(41));
    assert_eq!(run_code("41.0").unwrap(), F32(41.0));

    assert_eq!(run_code("12i8").unwrap(), I8(12));
    assert_eq!(run_code("12u8").unwrap(), U8(12));

    assert_eq!(run_code("123i16").unwrap(), I16(123));
    assert_eq!(run_code("123u16").unwrap(), U16(123));

    assert_eq!(run_code("1234i32").unwrap(), I32(1234));
    assert_eq!(run_code("1234u32").unwrap(), U32(1234));

    assert_eq!(run_code("1234i64").unwrap(), I64(1234));
    assert_eq!(run_code("1234u64").unwrap(), U64(1234));

    assert_eq!(run_code("1234isize").unwrap(), ISize(1234));
    assert_eq!(run_code("1234usize").unwrap(), USize(1234));

    assert_eq!(run_code("12345i128").unwrap(), I128(12345));
    assert_eq!(run_code("12345u128").unwrap(), U128(12345));

    assert_eq!(run_code("3.141f32").unwrap(), F32(3.141));
    assert_eq!(run_code("3.141f64").unwrap(), F64(3.141));
}
