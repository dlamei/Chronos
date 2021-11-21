use std::io::{self, Write};

mod chronos;
use chronos::*;

fn main() {

    loop {
        let mut buffer = String::new();

        print!("chronos > ");
        let _ = io::stdout().flush();
        io::stdin().read_line(&mut buffer).expect("Error while from STDIN");

        println!("{}", buffer);
    }

}
