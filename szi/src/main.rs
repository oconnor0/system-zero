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

#[derive(Debug, Eq, PartialEq)]
struct Env(Vec<Def>);

impl IntoIterator for Env {
    type Item = Def;
    type IntoIter = ::std::vec::IntoIter<Def>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Env {
  fn new() -> Env {
    Env(Vec::new())
  }

  fn get_val(self, var: &Var) -> Option<Def> {
    self.into_iter().rev().find(|&ref def| {
      match *def {
        Def::Val(ref v, _) => *v == *var,
        Def::Ty(_, _) => false,
      }
    })
  }

  fn push(&mut self, def: Def) {
    self.0.push(def)
  }
}

fn repl() -> io::Result<()> {
  let mut history: Vec<String> = vec![];
  let mut env = Env::new();
  prompt();
  let stdin = io::stdin();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    if line == ":env" {
      println!("{:?}", env);
    } else if line == ":history" {
      println!("{:?}", history);
    } else if line == ":quit" {
      break;
    } else if line.len() > 0 {
      history.push(line.clone());
      match parse_one(&line[..]) {
        Ok(one) => {
          match one {
            One::Def(ref def) => {
              print!("{:?}", one);
              env.push(def.clone());
            }
            One::Expr(_) => print!("{:?}", one.normalize()),
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
