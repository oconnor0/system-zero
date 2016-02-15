// Copyright (c) 2016, Matthew O'Connor
extern crate system_zero_core;

use std::io;
use std::io::{BufRead, Write};
use system_zero_core::*;
use system_zero_core::ast::*;

const BOOL: &'static str = include_str!("bool.sz");
const NAT: &'static str = include_str!("nat.sz");
const LIST: &'static str = include_str!("list.sz");

fn load_prelude(env: &mut Env) {
  env.load(BOOL);
  env.load(NAT);
  env.load(LIST);
}

const PROMPT: &'static str = "> ";

fn prompt() -> () {
  print!("{}", PROMPT);
  io::stdout().flush().unwrap()
}

fn support(line: &String, mut env: &mut Env, history: &Vec<String>) {
  if line == ":env" {
    print!("{:?}", env);
  } else if line == ":history" {
    println!("{:?}", history);
  } else if line == ":quit" || line == ":exit" {
    std::process::exit(1);
  } else if line == ":clear" {
    env.clear();
    load_prelude(&mut env);
  } else if line.starts_with(":ty ") {
    let name = &line[4..];
    match env.get_ty(&Var::new(name, 0)) {
      Some(ref def) => println!("{:?}", def),
      None => println!("No type for `{}`", name),
    }
  } else if line.starts_with(":val ") {
    let name = &line[5..];
    match env.get_val(&Var::new(name, 0)) {
      Some(ref def) => println!("{:?}", def),
      None => println!("No value for `{}`", name),
    }
  } else if line.starts_with(":def ") {
    let name = &line[5..];
    match env.get_ty(&Var::new(name, 0)) {
      Some(ref def) => println!("{:?}", def),
      None => println!("No type for `{}`", name),
    }
    match env.get_val(&Var::new(name, 0)) {
      Some(ref def) => println!("{:?}", def),
      None => println!("No value for `{}`", name),
    }
  }
}

fn eval<'input>(line: &'input str, mut env: &mut Env) -> Result<'input, One> {
  match parse_one(&line) {
    Ok(one) => {
      match one {
        One::Def(ref def) => {
          let def = def.normalize_in(&mut env);
          let one = One::Def(def.clone());
          env.push(def);
          Ok(one)
        }
        One::Expr(ref e) => Ok(One::Expr(e.normalize_in(&mut env))),
      }
    }
    Err(error) => Err(error),
  }
}

fn repl() -> io::Result<()> {
  let mut history: Vec<String> = vec![];
  let mut env = Env::new();
  load_prelude(&mut env);
  prompt();
  let stdin = io::stdin();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    history.push(line.clone());
    if line.starts_with("--") {
      ()
    } else if line.starts_with(":") {
      support(&line, &mut env, &history);
    } else if line.len() > 0 {
      match eval(&line[..], &mut env) {
        Ok(one) => print!("{:?}", one),
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
  repl().unwrap()
}
