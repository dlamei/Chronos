#![allow(dead_code)]

use std::io::{self, Write};

mod chronos;
mod errors;
use chronos::*;

fn main() {
    loop {
        let mut buffer = String::new();

        print!("chronos > ");
        let _ = io::stdout().flush();
        io::stdin()
            .read_line(&mut buffer)
            .expect("Error while reading from STDIN");

        match interpret(String::from("STDIN"), buffer) {
            Ok(result) => println!("{:?}", result.get_value()),
            Err(e) => eprintln!("{}", e),
        }
    }
}
