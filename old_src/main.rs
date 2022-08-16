#![allow(dead_code, clippy::single_match)]

mod chronos;
mod compiler;
mod datatypes;
mod errors;
mod interpreter;
mod lexer;
mod parser;

use chronos::*;
use std::io::{self, Write};

fn main() {
    let mut c = Compiler::new();

    loop {
        let mut buffer = String::new();

        print!("chronos > ");
        let _ = io::stdout().flush();
        io::stdin()
            .read_line(&mut buffer)
            .expect("Error while reading from STDIN");

        match c.interpret(String::from("<stdin>"), buffer) {
            Ok(result) => println!("{}", result),
            Err(mut e) => {
                e.set_files(c.file_manager.files.clone());
                e.print();
            }
        }
    }
}
