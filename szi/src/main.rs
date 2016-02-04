// Copyright (c) 2016, Matthew O'Connor
extern crate system_zero_core;

use std::io;
use std::io::{BufRead, Write};
use system_zero_core::*;
use system_zero_core::ast::*;

const PROMPT: &'static str = "> ";

fn prompt() -> () {
  print!("{}", PROMPT);
  io::stdout().flush().unwrap()
}

fn repl() -> io::Result<()> {
  prompt();
  let stdin = io::stdin();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    if line.len() > 0 {
      match parse_one(&line[..]) {
        Ok(one) => print!("{:?}", one.normalize()),
        Err(error) => println!("Couldn't parse: {:?}", error),
      }
    }
    prompt();
  }
  Ok(())
}

fn main() {
  println!("szi - system zero interpreter");
  println!("Copyright (c) 2016, Matthew O'Connor <thegreendragon@gmail.com>");
  std::process::exit(repl().is_ok() as i32)
}
