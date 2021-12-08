#![allow(dead_code)]

use std::io::{self, Write};

mod chronos;
mod errors;
use chronos::*;

fn main() {
    let mut c = Compiler::new();

    loop {
        let mut buffer = String::new();

        print!("chronos > ");
        let _ = io::stdout().flush();
        io::stdin()
            .read_line(&mut buffer)
            .expect("Error while reading from STDIN");

        match c.interpret(String::from("STDIN"), buffer) {
            Ok(result) => println!("{}", result),
            Err(e) => eprintln!("{}", e),
        }
    }
}
