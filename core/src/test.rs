// Copyright (c) 2016, Matthew O'Connor

//! Tests System Zero Core

#[cfg(test)]

// use std::collections::{HashMap, HashSet};
// use std::fmt::{Debug, Formatter, Error};

use ast::*;
// use parser::parse_One;

/// Tests for AST

fn constant(c: Const) -> Box<Expr> { Box::new(Expr::Const(c)) }
fn var(v: &str) -> Box<Expr> { Box::new(Expr::Var(Var::new(v))) }
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
  assert_eq!("a", format!("{:?}", Var::new("a")));
  assert_eq!("b", format!("{:?}", var("b")));
}

#[test]
fn test_lam_to_string() {
  use ast::Const::*;
  assert_eq!("\\(a : data) -> \\(b : a) -> b",
             format!("{:?}",
                     lam(Var::new("a"),
                         constant(Data),
                         lam(Var::new("b"), var("a"), var("b")))));
}

#[test]
fn test_pi_to_string() {
  use ast::Const::*;
  assert_eq!("forall (a : codata) -> a -> a",
             format!("{:?}",
                     pi(Var::new("a"),
                        constant(Codata),
                        pi(Var::new(""), var("a"), var("a")))));
}

#[test]
fn test_app_to_string() {
  use ast::Const::*;
  assert_eq!("(\\(a : data) -> \\(b : a) -> b) int one",
             format!("{:?}",
                     app(app(lam(Var::new("a"),
                                 constant(Data),
                                 lam(Var::new("b"), var("a"), var("b"))),
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
  let a = Var::new("a");
  let x = Var::new("x");
  let id = Var::new("id");
  let unused = Var::unused();
  // id's type
  let ty = pi(a.clone(), constant(Data), pi(unused, var("a"), var("a")));
  // id's implementation
  let id_impl = lam(Var::new("a"),
                    constant(Data),
                    lam(x.clone(), var("a"), var("x")));
  // implementation of id applied to itself applied to id
  let id2app = app(lam(id.clone(),
                       ty.clone(),
                       app(app(var("id"), ty.clone()), var("id"))),
                   id_impl.clone());
  assert_eq!(id2app.normalize(), *id_impl);
}
