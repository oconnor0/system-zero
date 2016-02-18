// Copyright (c) 2016, Matthew O'Connor

//! Tests System Zero Core

#[cfg(test)]

// use std::collections::{HashMap, HashSet};
// use std::fmt::{Debug, Formatter, Error};

use ast::*;
// use parser::parse_One;

/// Tests for AST

fn id(v: &str) -> Var { Var::new(v) }
fn constant(c: Const) -> Box<Expr> { Box::new(Expr::Const(c)) }
fn var(v: &str) -> Box<Expr> { Box::new(Expr::Var(id(v))) }
fn lam(var: Var, ty: Box<Expr>, body: Box<Expr>) -> Box<Expr> {
  Box::new(Expr::Lam(var, ty, body))
}
fn pi(var: Var, ty: Box<Expr>, body: Box<Expr>) -> Box<Expr> {
  Box::new(Expr::Pi(var, ty, body))
}
fn app(f: Box<Expr>, arg: Box<Expr>) -> Box<Expr> {
  Box::new(Expr::App(f, arg))
}

#[test]
fn test_const_to_string() {
  use ast::Const::*;
  use ast::Expr::*;
  assert_eq!("data", format!("{:?}", Data));
  assert_eq!("data", format!("{:?}", Const(Data)));
  assert_eq!("codata", format!("{:?}", Codata));
  assert_eq!("codata", format!("{:?}", Const(Codata)));
  assert_eq!("box", format!("{:?}", Box));
  assert_eq!("box", format!("{:?}", Const(Box)));
  assert_eq!("cobox", format!("{:?}", Cobox));
  assert_eq!("cobox", format!("{:?}", Const(Cobox)));
}

#[test]
fn test_var_to_string() {
  assert_eq!("a", format!("{:?}", id("a")));
  assert_eq!("b", format!("{:?}", var("b")));
}

#[test]
fn test_lam_to_string() {
  use ast::Const::*;
  assert_eq!("\\(a : data) -> \\(b : a) -> b",
             format!("{:?}",
                     lam(id("a"),
                         constant(Data),
                         lam(id("b"), var("a"), var("b")))));
}

#[test]
fn test_pi_to_string() {
  use ast::Const::*;
  assert_eq!("forall (a : codata) -> a -> a",
             format!("{:?}",
                     pi(id("a"),
                        constant(Codata),
                        pi(id(""), var("a"), var("a")))));
}

#[test]
fn test_app_to_string() {
  use ast::Const::*;
  assert_eq!("(\\(a : data) -> \\(b : a) -> b) int one",
             format!("{:?}",
                     app(app(lam(id("a"),
                                 constant(Data),
                                 lam(id("b"), var("a"), var("b"))),
                             var("int")),
                         var("one"))));
  assert_eq!("a b c (d e)",
             format!("{:?}",
                     app(app(app(var("a"), var("b")), var("c")),
                         app(var("d"), var("e")))));
  assert_eq!("a (b c) d (e f)",
             format!("{:?}",
                     app(app(app(var("a"), app(var("b"), var("c"))),
                             var("d")),
                         app(var("e"), var("f")))));
}

#[test]
fn test_id_eq_id2() {
  use ast::Const::*;
  let a = id("a");
  let x = id("x");
  let id = id("id");
  let unused = Var::unused();
  // id's type
  let ty = pi(a.clone(), constant(Data), pi(unused, var("a"), var("a")));
  // id's implementation
  let id_impl = lam(id("a"),
                    constant(Data),
                    lam(x.clone(), var("a"), var("x")));
  // implementation of id applied to itself applied to id
  let id2app = app(lam(id.clone(),
                       ty.clone(),
                       app(app(var("id"), ty.clone()), var("id"))),
                   id_impl.clone());
  assert_eq!(id2app.normalize(), *id_impl);
}

/*
// Copyright (c) 2016, Matthew O'Connor
extern crate system_zero_core;
use system_zero_core::ast::*;

fn constant(c: Const) -> Expr {
  Expr::constant(c)
}

fn var(v: &Var) -> Expr {
  Expr::var(v)
}

fn lam(var: Var, ty: Expr, body: Expr) -> Expr {
  Expr::lam(var, ty, body)
}

fn pi(var: Var, ty: Expr, body: Expr) -> Expr {
  Expr::pi(var, ty, body)
}

fn app(f: Expr, arg: Expr) -> Expr {
  Expr::app(f, arg)
}

fn main() {
  let v_bool = Var::new("bool", 0);
  let v_true = Var::new("true", 0);
  let v_false = Var::new("false", 0);
  let v_if = Var::new("if", 0);
  let v_b = Var::new("b", 0);
  let v_r = Var::new("r", 0);
  let v_x = Var::new("x", 0);
  let v_y = Var::new("y", 0);
  let v_0 = Var::new("0", 0);
  let v_1 = Var::new("1", 0);
  let v_nat = Var::new("nat", 0);
  let v_and = Var::new("and", 0);
  let v_or = Var::new("or", 0);
  let v_xor = Var::new("xor", 0);
  let v_not = Var::new("not", 0);
  let unused = Var::new("", 0);

  let ty_bool = constant(Const::Data);
  println!("bool : {:?}.", ty_bool);
  let ty_true = var(&v_bool);
  println!("true : {:?}.", ty_true);
  let ty_false = var(&v_bool);
  println!("false : {:?}.", ty_false);
  let ty_if = lam(unused.clone(),
                  var(&v_bool),
                  pi(v_r.clone(),
                     constant(Const::Data),
                     lam(unused.clone(),
                         var(&v_r),
                         lam(unused.clone(), var(&v_r), var(&v_r)))));
  println!("if : {:?}.", ty_if);
  println!("");

  let impl_bool = pi(v_r.clone(),
                     constant(Const::Data),
                     lam(unused.clone(),
                         var(&v_r),
                         lam(unused.clone(), var(&v_r), var(&v_r))));
  println!("bool = {:?}.", impl_bool);
  let impl_true = pi(v_r.clone(),
                     constant(Const::Data),
                     lam(v_x.clone(),
                         var(&v_r),
                         lam(unused.clone(), var(&v_r), var(&v_x))));
  println!("true = {:?}.", impl_true);
  let impl_false = pi(v_r.clone(),
                      constant(Const::Data),
                      lam(unused.clone(),
                          var(&v_r),
                          lam(v_y.clone(), var(&v_r), var(&v_y))));
  println!("false = {:?}.", impl_false);
  let impl_if = lam(v_b.clone(), var(&v_bool), var(&v_b));
  println!("if = {:?}.", impl_if);
  println!("");

  let if_true = lam(v_bool.clone(),
                    ty_bool.clone(),
                    lam(v_true.clone(),
                        ty_true.clone(),
                        lam(v_false.clone(),
                            ty_false.clone(),
                            lam(v_if.clone(),
                                ty_if.clone(),
                                app(app(app(app(var(&v_if),
                                                var(&v_true)),
                                            var(&v_nat)),
                                        var(&v_1)),
                                    var(&v_0))))));
  println!("{:?}", if_true);

  let program = if_true;
  let execute = app(app(app(app(program, impl_bool.clone()),
                            impl_true.clone()),
                        impl_false.clone()),
                    impl_if.clone());
  println!("{:?}", execute);

  let result = execute.normalize();
  println!("= {:?}", result);
  println!("");

  let p2 = lam(v_false.clone(),
               ty_false.clone(),
               app(app(app(var(&v_false), var(&v_nat)), var(&v_1)), var(&v_0)));
  println!("{:?}", p2);

  let r2 = app(p2, impl_false.clone());
  println!("{:?}", r2);
  println!("= {:?}", r2.normalize());
  println!("");

  let ty_and = lam(unused.clone(),
                   var(&v_bool),
                   lam(unused.clone(), var(&v_bool), var(&v_bool)));
  println!("and : {:?}.", ty_and);
  let impl_and = lam(v_x.clone(),
                     var(&v_bool),
                     lam(v_y.clone(),
                         var(&v_bool),
                         app(app(app(app(var(&v_if), var(&v_x)),
                                     var(&v_bool)),
                                 var(&v_y)),
                             var(&v_false))));
  println!("and = {:?}.", impl_and);
  println!("");

  let and_prog = lam(v_and.clone(),
                     ty_and.clone(),
                     app(app(var(&v_and), var(&v_true)), var(&v_false)));
  let and_prog_app = app(and_prog.clone(), impl_and.clone());
  println!("{:?}", and_prog_app);

  let ctx_bool_with_and = app(app(app(app(lam(v_bool.clone(),
                                              ty_bool.clone(),
                                              lam(v_true.clone(),
                                                  ty_true.clone(),
                                                  lam(v_false.clone(),
                                                      ty_false.clone(),
                                                      lam(v_if.clone(),
                                                          ty_if.clone(),
                                                          and_prog_app)))),
                                          impl_bool.clone()),
                                      impl_true.clone()),
                                  impl_false.clone()),
                              impl_if.clone());
  println!("{:?}", ctx_bool_with_and);
  println!("and true false = {:?}.", ctx_bool_with_and.normalize());
  println!("");
}
*/
