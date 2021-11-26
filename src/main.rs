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

        match run("STDIN".to_string(), buffer) {
            Ok(tokens) => println!("{:?}", tokens),
            Err(e) => eprintln!("{}", e),
        }
        //let tokens = run(buffer).unwrap_or_else(|x| panic!("{}", x));
        //println!("{:?}", tokens);
    }
}
