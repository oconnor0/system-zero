// Copyright (c) 2016, Matthew O'Connor

//! Abstract syntax tree for System Zero Core
//!
//! Defines AST and traits for debugging and normalizing expressions.

use std::fmt::{Debug, Formatter, Error};

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

// # Traits

/// `Normalize` converts the value to a strongly normalized version.
/// Normalization in a total typed lambda calculus is essentially inlining
/// until nothing else can be.
pub trait Normalize {
  fn normalize(&self) -> Self;
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
          write!(fmt, "{:?} -> {:?}", ty, body)
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

impl Normalize for Def {
  fn normalize(&self) -> Def {
    use self::Def::*;
    match *self {
      Val(ref n, ref e) => Def::Val(n.clone(), e.normalize()),
      Ty(ref n, ref t) => Def::Ty(n.clone(), t.normalize()),
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

/// Implementations of traits for `One`
impl Normalize for One {
  fn normalize(&self) -> One {
    use self::One::*;
    match *self {
      Def(ref d) => One::Def(d.normalize()),
      Expr(ref e) => One::Expr(e.normalize()),
    }
  }
}

impl Debug for One {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use self::One::*;
    match *self {
      Def(ref d) => write!(fmt, "{:?}\n", d),
      Expr(ref e) => write!(fmt, "{:?}.\n", e),
    }
  }
}

/// Implementations of traits for an entire `Mod`
impl Mod {
  pub fn new(listing: Vec<One>) -> Mod {
    Mod { listing: listing }
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

impl Debug for Mod {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    for ref o in self.listing.iter() {
      try!(write!(fmt, "{:?}", o));
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
