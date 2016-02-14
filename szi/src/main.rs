// Copyright (c) 2016, Matthew O'Connor
extern crate system_zero_core;

use std::io;
use std::io::{BufRead, Write};
use system_zero_core::*;
use system_zero_core::ast::*;

const BOOL: &'static str = include_str!("bool.sz");
const NAT: &'static str = include_str!("nat.sz");

fn load_prelude(env: &mut Env) {
  env.load(BOOL);
  env.load(NAT);
}

const PROMPT: &'static str = "> ";

fn prompt() -> () {
  print!("{}", PROMPT);
  io::stdout().flush().unwrap()
}

fn repl() -> io::Result<()> {
  let mut history: Vec<String> = vec![];
  let mut env = Env::new();
  load_prelude(&mut env);
  prompt();
  let stdin = io::stdin();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    if line == ":env" {
      print!("{:?}", env);
    } else if line == ":history" {
      println!("{:?}", history);
    } else if line == ":quit" || line == ":exit" {
      break;
    } else if line == ":clear" {
      env.clear();
      load_prelude(&mut env);
    } else if line.starts_with("--") {
      ()
    } else if line.len() > 0 {
      history.push(line.clone());
      match parse_one(&line[..]) {
        Ok(one) => {
          match one {
            One::Def(ref def) => {
              let def = def.normalize_in(&mut env);
              let one = One::Def(def.clone());
              print!("{:?}", one);
              env.push(def);
            }
            One::Expr(ref e) => {
              print!("{:?}", One::Expr(e.normalize_in(&mut env)))
            }
          }
        }
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
  // Return 0 if repl returned Ok.
  std::process::exit(!repl().is_ok() as i32)
}
