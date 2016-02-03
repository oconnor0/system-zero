extern crate system_zero_core;

use std::io;
use std::io::Write;
use system_zero_core::*;
use system_zero_core::ast::*;

const PROMPT: &'static str = "> ";

fn repl() -> io::Result<()> {
  loop {
    let mut input = String::new();
    print!("{}", PROMPT);
    try!(io::stdout().flush());
    match io::stdin().read_line(&mut input) {
      Ok(_) => {
        let line: &str = &input[..];
        match parse_one(line) {
          Ok(one) => print!("{:?}", one.normalize()),
          Err(error) => println!("Couldn't parse: {:?}", error),
        }
      }
      Err(error) => println!("Couldn't read: {}", error),
    }
  }
}

fn main() {
  println!("szi - system zero interpreter");
  println!("Copyright (C) 2016 Matthew O'Connor <thegreendragon@gmail.com>");
  std::process::exit(repl().is_ok() as i32)
}
