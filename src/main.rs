#![allow(dead_code)]
mod chvalue;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod primitive;

use std::{env, fs, io};

fn run(c: &str) {
    let mut tokens: Vec<lexer::Token> = lexer::lex_tokens(c);
    lexer::print_tokens(c, &tokens);

    tokens
        .iter()
        .filter(|tok| matches!(tok.typ, lexer::TokenType::Error))
        .for_each(|err| {
            println!(
                "Lexer: could not lex: \n{}",
                error::underline_code(c, &err.range)
            )
        });
    lexer::filter_tokens(&mut tokens);

    let ast = parser::parse_tokens(tokens);

    println!("parser: {ast}");
    parser::print_errors(&ast, c);

    if !ast.flags.contains(parser::NodeFlags::ERROR) {
        match interpreter::interpret(&ast) {
            Ok(e) => println!("interpreter: {}", e),
            Err(e) => interpreter::print_error(e, c),
        };
    }
}

fn main() {
    env::set_var("RUST_BACKTRACE", "1");
    let args: Vec<String> = env::args().collect();

    if args.len() >= 2 && args[1] == "repl" {
        loop {
            let mut code = String::new();
            io::stdin().read_line(&mut code).unwrap();
            let code = code.trim();
            run(code);
        }
    }
    if args.len() >= 3 && args[1] == "file" {
        let filename = &args[2];
        let code = fs::read_to_string(filename).expect("could not find file: {filename}");
        run(&code);
    } else {
        let code = "{val = 3; ref = &val; *ref = 4; val}()";
        run(&code);
    }
}

fn run_test(c: &str) -> Result<primitive::Primitive, interpreter::RuntimeErr> {
    let mut tokens: Vec<lexer::Token> = lexer::lex_tokens(c);
    lexer::filter_tokens(&mut tokens);
    let ast = parser::parse_tokens(tokens);
    match interpreter::interpret(&ast) {
        Ok(v) => Ok(v.value),
        Err(e) => Err(e),
    }
}

#[test]
fn basic_code() {
    use primitive::Primitive::*;
    assert_eq!(run_test("1 + 2 * 3").unwrap(), I32(7));
    assert_eq!(run_test("1 + 2 * (3 + 2) == 11").unwrap(), Bool(true));
    assert_eq!(run_test("1 + 2 * (3 + 2) == 10").unwrap(), Bool(false));
    assert_eq!(run_test("1 + 2 * (3 + 2) != 10").unwrap(), Bool(true));
    assert_eq!(run_test("1 + - 2 * ((3 + 2) == 5)").unwrap(), I32(-1));

    assert_eq!(run_test("1 >= 3").unwrap(), Bool(false));
    assert_eq!(run_test("1 > 1").unwrap(), Bool(false));
    assert_eq!(run_test("1.1 >= 1").unwrap(), Bool(true));
    assert_eq!(run_test("1.1 <= 1").unwrap(), Bool(false));
    assert_eq!(run_test("!(false)").unwrap(), Bool(true));
    assert_eq!(run_test("(!false) * 3.4").unwrap(), F32(3.4));
    assert_eq!(
        run_test("{a = (3 == 2) + 2 * -1.52;;; a}()").unwrap(),
        F32(-3.04)
    );
    assert_eq!(run_test("{a = {2i8 + 3u8}; return a;}()()").unwrap(), I8(5));
    assert_eq!(run_test("{a = 3; {a = a + 2.3}(); a}()").unwrap(), F32(5.3));

    assert_eq!(run_test("{a = 3; a += 2; a}()").unwrap(), I32(5));
    assert_eq!(run_test("{a = 3; a -= 2; a}()").unwrap(), I32(1));
    assert_eq!(run_test("{a = 3; a *= 2; a}()").unwrap(), I32(6));
    assert_eq!(run_test("{a = 3; a /= 2; a}()").unwrap(), I32(1));
    assert_eq!(run_test("{a = 3; a /= 2.0; a}()").unwrap(), F32(1.5));
}

#[test]
fn unry_test() {
    use primitive::Primitive::*;
    assert_eq!(run_test("1 + - - - !false").unwrap(), I32(0));
}

#[test]
fn ref_test() {
    use primitive::Primitive::*;
    assert_eq!(
        run_test("{l = 3; r = 4; res = &l + &r; res}()").unwrap(),
        I32(7)
    );
    assert_eq!(
        run_test("{l = 3; r = 4; res = &l * r; res}()").unwrap(),
        I32(12)
    );
    assert_eq!(
        run_test("{l = 3; r = 4.0; res = l / &r; res}()").unwrap(),
        F32(3.0 / 4.0)
    );
    assert_eq!(
        run_test("{ref = {a = 3; &a}(); *ref += 2; *ref}()").unwrap(),
        I32(5)
    );
    assert_eq!(
        run_test("{val = 4; res = {ref = &val; val *= 1.5; *ref}(); res}()").unwrap(),
        F32(6.0)
    );
    assert_eq!(
        run_test("{ref = {a = 4; &a}(); *ref = 3; *ref}()").unwrap(),
        I32(3)
    );
}

#[test]
fn eval_lit() {
    use primitive::Primitive::*;

    assert_eq!(run_test("41").unwrap(), I32(41));
    assert_eq!(run_test("41.0").unwrap(), F32(41.0));

    assert_eq!(run_test("12i8").unwrap(), I8(12));
    assert_eq!(run_test("12u8").unwrap(), U8(12));

    assert_eq!(run_test("123i16").unwrap(), I16(123));
    assert_eq!(run_test("123u16").unwrap(), U16(123));

    assert_eq!(run_test("1234i32").unwrap(), I32(1234));
    assert_eq!(run_test("1234u32").unwrap(), U32(1234));

    assert_eq!(run_test("1234i64").unwrap(), I64(1234));
    assert_eq!(run_test("1234u64").unwrap(), U64(1234));

    assert_eq!(run_test("1234isize").unwrap(), ISize(1234));
    assert_eq!(run_test("1234usize").unwrap(), USize(1234));

    assert_eq!(run_test("12345i128").unwrap(), I128(12345));
    assert_eq!(run_test("12345u128").unwrap(), U128(12345));

    assert_eq!(run_test("3.141f32").unwrap(), F32(3.141));
    assert_eq!(run_test("3.141f64").unwrap(), F64(3.141));
}
