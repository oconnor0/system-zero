// Copyright (c) 2016, Matthew O'Connor
extern crate system_zero_core;

use std::fmt::{Debug, Formatter};
use std::io;
use std::io::{BufRead, Write};
use system_zero_core::*;
use system_zero_core::ast::*;

const BOOL: &'static str = include_str!("bool.sz");

const PROMPT: &'static str = "> ";

fn prompt() -> () {
  print!("{}", PROMPT);
  io::stdout().flush().unwrap()
}

#[derive(Eq, PartialEq)]
struct Env(Vec<Def>);

impl Debug for Env {
  fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
    for def in &self.0 {
      try!(writeln!(fmt, "{:?}", def));
    }
    Ok(())
  }
}

impl Env {
  fn new() -> Env {
    Env(Vec::new())
  }

  fn get_val(&self, var: &Var) -> Option<&Def> {
    self.0.iter().rev().find(|def| {
      match **def {
        Def::Val(ref v, _) => *v == *var,
        Def::Ty(_, _) => false,
      }
    })
  }

  fn push(&mut self, def: Def) {
    self.0.push(def)
  }

  fn pop(&mut self) {
    self.0.pop();
  }

  fn load(&mut self, file: &'static str) {
    for line in file.lines() {
      if line.is_empty() || line.starts_with("--") {
        ()
      } else if line.len() > 0 {
        match parse_one(&line[..]) {
          Ok(one) => {
            match one {
              One::Def(ref def) => {
                let def = normalize_def_in(def, self);
                self.push(def);
              }
              One::Expr(_) => (),
            }
          }
          Err(error) => println!("Couldn't parse: {:?}", error),
        }
      }
    }
  }
}

fn normalize_in(e: &Expr, env: &mut Env) -> Expr {
  match *e {
    Expr::Const(ref constant) => Expr::Const(constant.clone()),
    Expr::Var(ref var) => {
      match env.get_val(var) {
        Some(ref def) => def.expr().clone(),
        None => Expr::Var(var.clone()),
      }
    }
    Expr::Lam(ref var, ref ty, ref body) => {
      env.push(Def::Val(var.clone(), Expr::Var(var.clone())));
      let ty = normalize_in(ty, env);
      let body = normalize_in(body, env);
      let lam = Expr::Lam(var.clone(), Box::new(ty), Box::new(body));
      env.pop();
      lam
    }
    Expr::Pi(ref var, ref ty, ref body) => {
      env.push(Def::Ty(var.clone(), Expr::Var(var.clone())));
      let ty = normalize_in(ty, env);
      let body = normalize_in(body, env);
      let pi = Expr::Pi(var.clone(), Box::new(ty), Box::new(body));
      env.pop();
      pi
    }
    Expr::App(ref f, ref arg) => {
      let f = normalize_in(f, env);
      let arg = normalize_in(arg, env);
      if let Expr::Lam(var, _, body) = f {
        env.push(Def::Val(var.clone(), arg));
        let lam = normalize_in(&*body, env);
        env.pop();
        lam
      } else if let Expr::Pi(var, _, body) = f {
        env.push(Def::Ty(var.clone(), arg));
        let pi = normalize_in(&*body, env);
        env.pop();
        pi
      } else {
        Expr::App(Box::new(f), Box::new(arg))
      }
    }
  }
}

fn normalize_def_in(def: &Def, env: &mut Env) -> Def {
  match *def {
    Def::Val(ref var, ref e) => Def::Val(var.clone(), normalize_in(e, env)),
    Def::Ty(ref var, ref e) => Def::Ty(var.clone(), normalize_in(e, env)),
  }
}

fn repl() -> io::Result<()> {
  let mut history: Vec<String> = vec![];
  let mut env = Env::new();
  env.load(BOOL);
  prompt();
  let stdin = io::stdin();
  for line in stdin.lock().lines() {
    let line = line.unwrap();
    if line == ":env" {
      print!("{:?}", env);
    } else if line == ":history" {
      println!("{:?}", history);
    } else if line == ":quit" {
      break;
    } else if line.starts_with("--") {
      ()
    } else if line.len() > 0 {
      history.push(line.clone());
      match parse_one(&line[..]) {
        Ok(one) => {
          match one {
            One::Def(ref def) => {
              let def = normalize_def_in(def, &mut env);
              let one = One::Def(def.clone());
              print!("{:?}", one);
              env.push(def);
            }
            One::Expr(ref e) => {
              print!("{:?}", One::Expr(normalize_in(e, &mut env)))
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
