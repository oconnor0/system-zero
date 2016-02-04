// Copyright (c) 2016, Matthew O'Connor
use std::fmt::{Debug, Formatter, Error};

/// Definition of core abstract syntax tree for System Zero
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Const {
  Data,
  Codata,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Var<'input> {
  name: &'input str,
  idx: i32,
}

// "Everything" is an expression in System Zero.
#[derive(Clone, Eq, PartialEq)]
// #[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr<'input> {
  // Type system constants
  Const(Const),
  // Bound variables
  Var(Var<'input>),
  // Lambda
  Lam(Var<'input>, Box<Expr<'input>>, Box<Expr<'input>>),
  // "forall"
  Pi(Var<'input>, Box<Expr<'input>>, Box<Expr<'input>>),
  // Function application
  App(Box<Expr<'input>>, Box<Expr<'input>>),
}

// Definitions of values and types.
#[derive(Clone, Eq, PartialEq)]
pub enum Def<'input> {
  Val(Var<'input>, Box<Expr<'input>>),
  Ty(Var<'input>, Box<Expr<'input>>),
}

// Represents one top-level element.
#[derive(Clone, Eq, PartialEq)]
pub enum One<'input> {
  Def(Box<Def<'input>>),
  Expr(Box<Expr<'input>>),
}

// All of the code for a given module.
#[derive(Clone, Eq, PartialEq)]
pub struct Mod<'input> {
  listing: Vec<Box<One<'input>>>,
}

/// Traits
pub trait Normalize {
  fn normalize(&self) -> Self;
}

/// Traits for Const
impl Debug for Const {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use self::Const::*;
    match *self {
      Data => write!(fmt, "data"),
      Codata => write!(fmt, "codata"),
    }
  }
}

/// Traits for Var
impl<'input> Var<'input> {
  pub fn new(name: &'input str, idx: i32) -> Var {
    Var {
      name: name,
      idx: idx,
    }
  }
}

impl<'input> Debug for Var<'input> {
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

/// Traits for Expr
impl<'input> Expr<'input> {
  pub fn constant(constant: Const) -> Expr<'input> {
    Expr::Const(constant)
  }
  pub fn var(v: &Var<'input>) -> Expr<'input> {
    Expr::Var(v.clone())
  }
  pub fn lam(var: Var<'input>,
             ty: Expr<'input>,
             body: Expr<'input>)
             -> Expr<'input> {
    Expr::Lam(var, Box::new(ty), Box::new(body))
  }
  pub fn pi(var: Var<'input>,
            ty: Expr<'input>,
            body: Expr<'input>)
            -> Expr<'input> {
    Expr::Pi(var, Box::new(ty), Box::new(body))
  }
  pub fn app(f: Expr<'input>, arg: Expr<'input>) -> Expr<'input> {
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

fn replace<'input>(val: &Var<'input>,
                   with: &Expr<'input>,
                   body: &Expr<'input>)
                   -> Expr<'input> {
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

impl<'input> Normalize for Expr<'input> {
  fn normalize(&self) -> Expr<'input> {
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

impl<'input> Debug for Expr<'input> {
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

/// Traits for Def
impl<'input> Normalize for Def<'input> {
  fn normalize(&self) -> Def<'input> {
    use self::Def::*;
    match *self {
      Val(ref n, ref e) => Def::Val(n.clone(), Box::new(e.normalize())),
      Ty(ref n, ref t) => Def::Ty(n.clone(), Box::new(t.normalize())),
    }
  }
}

impl<'input> Debug for Def<'input> {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use self::Def::*;
    match *self {
      Val(ref n, ref e) => write!(fmt, "{:?} = {:?}.", n, e),
      Ty(ref n, ref t) => write!(fmt, "{:?} : {:?}.", n, t),
    }
  }
}

/// Traits for a single one.
impl<'input> Normalize for One<'input> {
  fn normalize(&self) -> One<'input> {
    use self::One::*;
    match *self {
      Def(ref d) => One::Def(Box::new(d.normalize())),
      Expr(ref e) => One::Expr(Box::new(e.normalize())),
    }
  }
}

impl<'input> Debug for One<'input> {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use self::One::*;
    match *self {
      Def(ref d) => write!(fmt, "{:?}\n", d),
      Expr(ref e) => write!(fmt, "{:?}.\n", e),
    }
  }
}

/// Traits for an entire module.
impl<'input> Mod<'input> {
  pub fn new(listing: Vec<Box<One<'input>>>) -> Mod {
    Mod { listing: listing }
  }
}

impl<'input> Normalize for Mod<'input> {
  fn normalize(&self) -> Mod<'input> {
    let mut listing = Vec::new();
    for ref o in self.listing.iter() {
      listing.push(Box::new(o.normalize()));
    }
    Mod::new(listing)
  }
}

impl<'input> Debug for Mod<'input> {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    for ref o in self.listing.iter() {
      try!(write!(fmt, "{:?}", o));
    }
    Ok(())
  }
}

/// Tests for AST
#[cfg(test)]
fn constant<'input>(c: Const) -> Expr<'input> {
  Expr::constant(c)
}

#[cfg(test)]
fn var<'input>(v: &Var<'input>) -> Expr<'input> {
  Expr::var(v)
}

#[cfg(test)]
fn lam<'input>(var: Var<'input>,
               ty: Expr<'input>,
               body: Expr<'input>)
               -> Expr<'input> {
  Expr::lam(var, ty, body)
}

#[cfg(test)]
fn pi<'input>(var: Var<'input>,
              ty: Expr<'input>,
              body: Expr<'input>)
              -> Expr<'input> {
  Expr::pi(var, ty, body)
}

#[cfg(test)]
fn app<'input>(f: Expr<'input>, arg: Expr<'input>) -> Expr<'input> {
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
