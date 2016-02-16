// Copyright (c) 2016, Matthew O'Connor

//! Abstract syntax tree for System Zero Core
//!
//! Defines AST and traits for debugging and normalizing expressions.

use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter, Error};
use parser::parse_One;

/// `Const` defines the builtin type constants in the System Zero Core.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Const {
  /// `Data` is finite and all functions on it must be total.
  Data,
  /// `Codata` is potentially infinite and corecursion must be productive.
  Codata,
  /// `Box` is the type of `Data`.
  Box,
  /// `Cobox` is the type of `Codata`.
  Cobox,
}

/// `Var` is the label for a bound variable.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Var {
  /// `name` is the label.
  name: String,
}

/// `Expr` represents all computations in System Zero Core.
#[derive(Clone, PartialEq, Eq, Hash)]
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
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Def {
  Val(Var, Expr),
  Ty(Var, Expr),
}

/// `One` represents one top-level element. It needs a better name.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum One {
  Def(Def),
  Expr(Expr),
}

/// `Mod` is all of the code for a given module.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Mod {
  listing: Vec<One>,
}

/// `Env` defines an environment expressions are normalized in.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Env(Vec<Def>);

type BoundVarN = HashMap<Var, u32>;

// # Traits

/// `Canonicalize` replaces free variables with bound variables defined in a
/// given environment by wrapping the `Expr` in a lambda and applying it to the
/// `Expr` defined in the environment.
pub trait Canonicalize {
  fn canonicalize(&self, env: &Env) -> Self;
}

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

/// `TypeError` represents all errors in type checking.
pub enum TypeError {
  /// A mismatch between the expected and found types.
  Mismatch(Expr, Expr),
  /// A type for the given variable is missing.
  Missing(Var),
  /// A given expression cannot be typed.
  Cannot(Expr),
}

/// Type checks and either returns its type (as an `Expr`) or a `TypeError`.
pub trait TypeCheck {
  fn type_check(&self) -> Result<Expr, TypeError>;
}

/// Implementations of traits for `Const`
impl Debug for Const {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      Const::Data => write!(fmt, "data"),
      Const::Codata => write!(fmt, "codata"),
      Const::Box => write!(fmt, "box"),
      Const::Cobox => write!(fmt, "cobox"),
    }
  }
}

impl TypeCheck for Const {
  fn type_check(&self) -> Result<Expr, TypeError> {
    match *self {
      Const::Data => Ok(Expr::Const(Const::Box)),
      Const::Codata => Ok(Expr::Const(Const::Cobox)),
      Const::Box => Err(TypeError::Cannot(Expr::Const(Const::Box))),
      Const::Cobox => Err(TypeError::Cannot(Expr::Const(Const::Cobox))),
    }
  }
}

/// Implementations of traits for `Var`
impl Var {
  pub fn new<'input>(name: &'input str) -> Var {
    Var { name: name.to_string() }
  }
}

impl Debug for Var {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self.name)
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

/// Finds all free variables in the given expression.
fn find_free_vars_rec(expr: &Expr,
                      bindings: &mut BoundVarN,
                      free: &mut HashSet<Var>) {
  match *expr {
    Expr::Const(_) => (),
    Expr::Var(ref var) => {
      if !bindings.contains_key(var) {
        // Found a free var.
        free.insert(var.clone());
      }
    }
    Expr::Lam(ref var, ref ty, ref body) => {
      {
        find_free_vars_rec(ty, bindings, free);
      }
      {
        let counter = bindings.entry(var.clone()).or_insert(0);
        *counter += 1;
      }
      {
        find_free_vars_rec(body, bindings, free);
      }
      {
        let counter = bindings.entry(var.clone()).or_insert(0);
        *counter -= 1;
      }
    }
    Expr::Pi(ref var, ref ty, ref body) => {
      {
        find_free_vars_rec(ty, bindings, free);
      }
      {
        let counter = bindings.entry(var.clone()).or_insert(0);
        *counter += 1;
      }
      {
        find_free_vars_rec(body, bindings, free);
      }
      {
        let counter = bindings.entry(var.clone()).or_insert(0);
        *counter -= 1;
      }
    }
    Expr::App(ref f, ref arg) => {
      {
        find_free_vars_rec(f, bindings, free);
      }
      {
        find_free_vars_rec(arg, bindings, free);
      }
    }
  }
}


fn find_free_vars(expr: &Expr) -> HashSet<Var> {
  let mut bindings = BoundVarN::new();
  let mut free = HashSet::new();
  find_free_vars_rec(expr, &mut bindings, &mut free);
  free
}

/// Attempt new normalization technique.
impl NormalizeIn for Expr {
  fn normalize_in(&self, env: &mut Env) -> Expr {
    self.canonicalize(env).normalize()
  }
}

impl Canonicalize for Expr {
  fn canonicalize(&self, env: &Env) -> Self {
    let mut app = self.clone();
    for free in find_free_vars(self) {
      if let Some(ref def_val) = env.get_val(&free) {
        if let Some(ref def_ty) = env.get_ty(&free) {
          let lam = Expr::Lam(free.clone(),
                              Box::new(def_ty.expr().clone()),
                              Box::new(app));
          app = Expr::App(Box::new(lam), Box::new(def_val.expr().clone()));
        }
      }
    }
    app
  }
}

/// Implementations of traits for `Def`
impl Def {
  pub fn expr(&self) -> &Expr {
    // use self::Def::*;
    match *self {
      Def::Val(_, ref e) => e,
      Def::Ty(_, ref e) => e,
    }
  }

  pub fn var(&self) -> &Var {
    // use self::Def::*;
    match *self {
      Def::Val(ref v, _) => v,
      Def::Ty(ref v, _) => v,
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
      Def::Ty(_, _ /* ref var, ref e */) => self.clone(), //Def::Ty(var.clone(), e.normalize_in(env)),
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

  pub fn get_ty(&self, var: &Var) -> Option<&Def> {
    self.0.iter().rev().find(|def| {
      match **def {
        Def::Val(_, _) => false,
        Def::Ty(ref v, _) => *v == *var,
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
  let a = Var::new("a");
  let x = Var::new("x");
  let expra = var(&a);
  let exprx = var(&x);
  assert_eq!("x", format!("{:?}", x));
  let id = pi(a.clone(), constant(Const::Data), lam(x, expra, exprx));
  assert_eq!("forall (a : data) -> \\(x : a) -> x", format!("{:?}", id));
  let apply_id = app(app(id, var(&Var::new("int"))), var(&Var::new("1")));
  assert_eq!("(forall (a : data) -> \\(x : a) -> x) int 1",
             format!("{:?}", apply_id));
}

#[test]
fn test_id_eq_id2() {
  let a = Var::new("a");
  let x = Var::new("x");
  let id = Var::new("id");
  let unused = Var::new("");
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
