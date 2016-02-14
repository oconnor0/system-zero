// Copyright (c) 2016, Matthew O'Connor

//! Abstract syntax tree for System Zero Core
//!
//! Defines AST and traits for debugging and normalizing expressions.

use std::fmt::{Debug, Formatter, Error};
use parser::parse_One;

/// `Const` defines the builtin types of types.
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Const {
  /// `Data` is finite and all functions on it must be total.
  Data,
  /// `Codata` is potentially infinite and corecursion must be productive.
  Codata,
}

/// `Var` is the label for a bound variable.
#[derive(Clone, Eq, PartialEq)]
pub struct Var {
  /// `name` is the label.
  name: String,
  /// `idx` is the De Bruijn index, in theory.
  idx: i32,
}

/// `Expr` represents all computations in System Zero Core.
#[derive(Clone, Eq, PartialEq)]
pub enum Expr {
  // Type system constants
  Const(Const),
  // Bound variables
  Var(Var),
  // Lambda
  Lam(Var, Box<Expr>, Box<Expr>),
  // "forall"
  Pi(Var, Box<Expr>, Box<Expr>),
  // Function application
  App(Box<Expr>, Box<Expr>),
}

/// `Def`s are bindings between `Var` and values or types.
#[derive(Clone, Eq, PartialEq)]
pub enum Def {
  Val(Var, Expr),
  Ty(Var, Expr),
}

/// `One` represents one top-level element. It needs a better name.
#[derive(Clone, Eq, PartialEq)]
pub enum One {
  Def(Def),
  Expr(Expr),
}

/// `Mod` is all of the code for a given module.
#[derive(Clone, Eq, PartialEq)]
pub struct Mod {
  listing: Vec<One>,
}

/// `Env` defines an environment expressions are normalized in.
#[derive(Eq, PartialEq)]
pub struct Env(Vec<Def>);

// # Traits

/// `Normalize` converts the value to a strongly normalized version.
/// Normalization in a total typed lambda calculus is essentially inlining
/// until nothing else can be.
pub trait Normalize {
  fn normalize(&self) -> Self;
}

/// `NormalizeIn` converts the value to a strongly normalized version by
/// replacing free variables with defined variables in the `Env`.
/// Normalization in a total typed lambda calculus is essentially inlining
/// until nothing else can be.
pub trait NormalizeIn {
  fn normalize_in(&self, &mut Env) -> Self;
}

/// Implementations of traits for `Const`
impl Debug for Const {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use self::Const::*;
    match *self {
      Data => write!(fmt, "data"),
      Codata => write!(fmt, "codata"),
    }
  }
}

/// Implementations of traits for `Var`
impl Var {
  pub fn new<'input>(name: &'input str, idx: i32) -> Var {
    Var {
      name: name.to_string(),
      idx: idx,
    }
  }
}

impl Debug for Var {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    if self.name.len() == 0 {
      Ok(())
    } else if self.idx == 0 {
      write!(fmt, "{}", self.name)
    } else {
      write!(fmt, "{}@{}", self.name, self.idx)
    }
  }
}

/// Implementations of traits for `Expr`
impl Expr {
  pub fn constant(constant: Const) -> Expr {
    Expr::Const(constant)
  }
  pub fn var(v: &Var) -> Expr {
    Expr::Var(v.clone())
  }
  pub fn lam(var: Var, ty: Expr, body: Expr) -> Expr {
    Expr::Lam(var, Box::new(ty), Box::new(body))
  }
  pub fn pi(var: Var, ty: Expr, body: Expr) -> Expr {
    Expr::Pi(var, Box::new(ty), Box::new(body))
  }
  pub fn app(f: Expr, arg: Expr) -> Expr {
    Expr::App(Box::new(f), Box::new(arg))
  }

  pub fn is_constant(&self) -> bool {
    match *self {
      Expr::Const(_) => true,
      _ => false,
    }
  }
  pub fn is_var(&self) -> bool {
    match *self {
      Expr::Var(_) => true,
      _ => false,
    }
  }
  pub fn is_lam(&self) -> bool {
    match *self {
      Expr::Lam(_, _, _) => true,
      _ => false,
    }
  }
  pub fn is_pi(&self) -> bool {
    match *self {
      Expr::Pi(_, _, _) => true,
      _ => false,
    }
  }
  pub fn is_app(&self) -> bool {
    match *self {
      Expr::App(_, _) => true,
      _ => false,
    }
  }
}

fn replace(val: &Var, with: &Expr, body: &Expr) -> Expr {
  use self::Expr::*;
  // println!("replace {} with {} in {}",
  //          val.to_string(),
  //          with.to_string(),
  //          body.to_string());
  match *body {
    Const(_) => body.clone(),
    Var(ref var) => {
      if *var == *val {
        with.clone()
      } else {
        body.clone()
      }
    }
    Lam(ref var, ref ty, ref body) => {
      if *val == *var {
        // println!("stopping at {}", val.to_string());
        Lam(var.clone(), ty.clone(), body.clone())
      } else {
        let ty = Box::new(replace(val, with, &ty));
        let body = Box::new(replace(val, with, &body));
        let lam = Lam(var.clone(), ty, body);
        // println!("replaced = {}", lam.to_string());
        lam
      }
    }
    Pi(ref var, ref ty, ref body) => {
      if *val == *var {
        // println!("stopping at {}", val.to_string());
        Pi(var.clone(), ty.clone(), body.clone())
      } else {
        let ty = Box::new(replace(val, with, &ty));
        let body = Box::new(replace(val, with, &body));
        let pi = Pi(var.clone(), ty, body);
        // println!("replaced = {}", pi.to_string());
        pi
      }
    }
    App(ref f, ref arg) => {
      let f = Box::new(replace(val, with, &f));
      let arg = Box::new(replace(val, with, &arg));
      let app = App(f, arg);
      // println!("replaced = {}", app.to_string());
      app
    }
  }
}

impl Debug for Expr {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use self::Expr::*;
    match *self {
      Const(ref constant) => write!(fmt, "{:?}", constant),
      Var(ref var) => write!(fmt, "{:?}", var),
      Lam(ref var, ref ty, ref body) => {
        write!(fmt, "\\({:?} : {:?}) -> {:?}", var, ty, body)
      }
      Pi(ref var, ref ty, ref body) => {
        if var.name.len() == 0 {
          if ty.is_lam() || ty.is_pi() || ty.is_app() {
            write!(fmt, "({:?}) -> {:?}", ty, body)
          } else {
            write!(fmt, "{:?} -> {:?}", ty, body)
          }
        } else {
          write!(fmt, "forall ({:?} : {:?}) -> {:?}", var, ty, body)
        }
      }
      App(ref f, ref arg) => {
        if f.is_lam() || f.is_pi() {
          try!(write!(fmt, "("))
        } else {
          try!(Ok(()))
        }
        try!(write!(fmt, "{:?}", f));
        if f.is_lam() || f.is_pi() {
          try!(write!(fmt, ")"))
        } else {
          try!(Ok(()))
        }

        try!(write!(fmt, " "));

        if arg.is_lam() || arg.is_pi() || arg.is_app() {
          try!(write!(fmt, "("));
        } else {
          try!(Ok(()));
        }
        try!(write!(fmt, "{:?}", arg));
        if arg.is_lam() || arg.is_pi() || arg.is_app() {
          try!(write!(fmt, ")"))
        } else {
          try!(Ok(()))
        }

        Ok(())
      }
    }
  }
}

impl Normalize for Expr {
  fn normalize(&self) -> Expr {
    use self::Expr::*;
    // println!("normalize {}", self.to_string());
    match *self {
      Const(ref constant) => Const(constant.clone()),
      Var(ref var) => Var(var.clone()),
      Lam(ref var, ref ty, ref body) => {
        let l = &Lam(var.clone(),
                     Box::new(ty.normalize()),
                     Box::new(body.normalize()));
        l.clone()
      }
      Pi(ref var, ref ty, ref body) => {
        let p = &Pi(var.clone(),
                    Box::new(ty.normalize()),
                    Box::new(body.normalize()));
        p.clone()
      }
      App(ref f, ref arg) => {
        let f = f.normalize();
        // let f = *f.clone();
        let arg = arg.normalize();
        if let Lam(var, _, body) = f {
          let lam = replace(&var, &arg, &body).normalize();
          // println!("normalized = {}", lam.to_string());
          lam
        } else if let Pi(var, _, body) = f {
          let pi = replace(&var, &arg, &body).normalize();
          // println!("normalized = {}", pi.to_string());
          pi
        } else {
          // panic!("f isn't a function {}", f.to_string())
          App(Box::new(f), Box::new(arg))
        }
      }
    }
  }
}

impl NormalizeIn for Expr {
  fn normalize_in(&self, env: &mut Env) -> Expr {
    match *self {
      Expr::Const(ref constant) => Expr::Const(constant.clone()),
      Expr::Var(ref var) => {
        match env.get_val(var) {
          Some(ref def) => def.expr().clone(),
          None => Expr::Var(var.clone()),
        }
      }
      Expr::Lam(ref var, ref ty, ref body) => {
        env.push(Def::Val(var.clone(), Expr::Var(var.clone())));
        let ty = ty.normalize_in(env);
        let body = body.normalize_in(env);
        let lam = Expr::Lam(var.clone(), Box::new(ty), Box::new(body));
        env.pop();
        lam
      }
      Expr::Pi(ref var, ref ty, ref body) => {
        env.push(Def::Ty(var.clone(), Expr::Var(var.clone())));
        let ty = ty.normalize_in(env);
        let body = body.normalize_in(env);
        let pi = Expr::Pi(var.clone(), Box::new(ty), Box::new(body));
        env.pop();
        pi
      }
      Expr::App(ref f, ref arg) => {
        let f = f.normalize_in(env);
        let arg = arg.normalize_in(env);
        if let Expr::Lam(var, _, body) = f {
          env.push(Def::Val(var.clone(), arg));
          let lam = body.normalize_in(env);
          env.pop();
          lam
        } else if let Expr::Pi(var, _, body) = f {
          env.push(Def::Ty(var.clone(), arg));
          let pi = body.normalize_in(env);
          env.pop();
          pi
        } else {
          Expr::App(Box::new(f), Box::new(arg))
        }
      }
    }
  }
}

/// Implementations of traits for `Def`
impl Def {
  pub fn expr(&self) -> &Expr {
    use self::Def::*;
    match *self {
      Val(_, ref e) => e,
      Ty(_, ref e) => e,
    }
  }
}

impl Debug for Def {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use self::Def::*;
    match *self {
      Val(ref n, ref e) => write!(fmt, "{:?} = {:?}.", n, e),
      Ty(ref n, ref t) => write!(fmt, "{:?} : {:?}.", n, t),
    }
  }
}

impl Normalize for Def {
  fn normalize(&self) -> Def {
    use self::Def::*;
    match *self {
      Val(ref n, ref e) => Def::Val(n.clone(), e.normalize()),
      Ty(ref n, ref t) => Def::Ty(n.clone(), t.normalize()),
    }
  }
}

impl NormalizeIn for Def {
  fn normalize_in(&self, env: &mut Env) -> Def {
    match *self {
      Def::Val(ref var, ref e) => Def::Val(var.clone(), e.normalize_in(env)),
      Def::Ty(ref var, ref e) => Def::Ty(var.clone(), e.normalize_in(env)),
    }
  }
}

/// Implementations of traits for `One`
impl Debug for One {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use self::One::*;
    match *self {
      Def(ref d) => write!(fmt, "{:?}\n", d),
      Expr(ref e) => write!(fmt, "{:?}.\n", e),
    }
  }
}

impl Normalize for One {
  fn normalize(&self) -> One {
    use self::One::*;
    match *self {
      Def(ref d) => One::Def(d.normalize()),
      Expr(ref e) => One::Expr(e.normalize()),
    }
  }
}

/// Implementations of traits for an entire `Mod`
impl Mod {
  pub fn new(listing: Vec<One>) -> Mod {
    Mod { listing: listing }
  }
}

impl Debug for Mod {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    for ref o in self.listing.iter() {
      try!(write!(fmt, "{:?}", o));
    }
    Ok(())
  }
}

impl Normalize for Mod {
  fn normalize(&self) -> Mod {
    let mut listing = Vec::new();
    for ref o in self.listing.iter() {
      listing.push(o.normalize());
    }
    Mod::new(listing)
  }
}

/// Implementations of traits for `Env`
impl Env {
  pub fn new() -> Env {
    Env(Vec::new())
  }

  pub fn clear(&mut self) {
    self.0.clear();
  }

  pub fn get_val(&self, var: &Var) -> Option<&Def> {
    self.0.iter().rev().find(|def| {
      match **def {
        Def::Val(ref v, _) => *v == *var,
        Def::Ty(_, _) => false,
      }
    })
  }

  pub fn push(&mut self, def: Def) {
    self.0.push(def)
  }

  pub fn pop(&mut self) {
    self.0.pop();
  }

  pub fn load(&mut self, file: &'static str) {
    for line in file.lines() {
      if line.is_empty() || line.starts_with("--") {
        ()
      } else if line.len() > 0 {
        match parse_One(&line[..]) {
          Ok(one) => {
            match one {
              One::Def(ref def) => {
                let def = def.normalize_in(self);
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

impl Debug for Env {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    for def in &self.0 {
      try!(writeln!(fmt, "{:?}", def));
    }
    Ok(())
  }
}

/// Tests for AST
#[cfg(test)]
fn constant(c: Const) -> Expr {
  Expr::constant(c)
}

#[cfg(test)]
fn var(v: &Var) -> Expr {
  Expr::var(v)
}

#[cfg(test)]
fn lam(var: Var, ty: Expr, body: Expr) -> Expr {
  Expr::lam(var, ty, body)
}

#[cfg(test)]
fn pi(var: Var, ty: Expr, body: Expr) -> Expr {
  Expr::pi(var, ty, body)
}

#[cfg(test)]
fn app(f: Expr, arg: Expr) -> Expr {
  Expr::app(f, arg)
}

#[test]
fn test_to_string() {
  let codata = Const::Codata;
  assert_eq!("codata", format!("{:?}", codata));
  let a = Var::new("a", 0);
  let x = Var::new("x", 0);
  let expra = var(&a);
  let exprx = var(&x);
  assert_eq!("x", format!("{:?}", x));
  let id = pi(a.clone(), constant(Const::Data), lam(x, expra, exprx));
  assert_eq!("forall (a : data) -> \\(x : a) -> x", format!("{:?}", id));
  let apply_id = app(app(id, var(&Var::new("int", 0))), var(&Var::new("1", 0)));
  assert_eq!("(forall (a : data) -> \\(x : a) -> x) int 1",
             format!("{:?}", apply_id));
}

#[test]
fn test_id_eq_id2() {
  let a = Var::new("a", 0);
  let x = Var::new("x", 0);
  let id = Var::new("id", 0);
  let unused = Var::new("", 0);
  // id's type
  let ty = pi(a.clone(),
              constant(Const::Data),
              pi(unused, Expr::var(&a), Expr::var(&a)));
  // id's implementation
  let id_impl = lam(a.clone(),
                    constant(Const::Data),
                    lam(x.clone(), Expr::var(&a), Expr::var(&x)));
  // implementation of id applied to itself applied to id
  let id2app = app(lam(id.clone(),
                       ty.clone(),
                       app(app(Expr::var(&id.clone()), ty.clone()),
                           Expr::var(&id.clone()))),
                   id_impl.clone());
  assert_eq!(id2app.normalize(), id_impl);
}
