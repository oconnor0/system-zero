// Copyright (c) 2016, Matthew O'Connor
extern crate system_zero_core;

use system_zero_core::*;
use system_zero_core::ast::Normalize;

#[cfg(not(test))]
fn main() {
  // use ast::*;
  //
  // let codata = Const::Codata;
  // println!("{:?}", codata);
  // let a = Var::new("a", 0);
  // let x = Var::new("x", 0);
  // let expra = var(&a);
  // let exprx = var(&x);
  // println!("{:?}", x);
  // let id = lam(a.clone(), constant(Const::Data), lam(x, expra, exprx));
  // println!("{:?}", id);
  // let apply_id = app(app(id, var(&Var::new("int", 0))), var(&Var::new("1", 0)));
  // println!("{:?}", apply_id);

  println!("{:?}", parse_expr("a").unwrap());
  println!("{:?}", parse_expr("data").unwrap());
  println!("{:?}", parse_expr("codata").unwrap());
  println!("{:?}", parse_expr("\\ (a : data) -> a").unwrap());
  println!("{:?}",
           parse_expr("\\ (a : data) -> \\ (a : data) -> a").unwrap());
  println!("{:?}", parse_expr("forall (a : data) -> a").unwrap());
  println!("{:?}",
           parse_expr("forall (a : data) -> forall (b : data) -> a").unwrap());
  println!("{:?}",
           parse_expr("forall (a : data) -> \\ (a : data) -> a").unwrap());
  println!("{:?}", parse_expr("a -> a").unwrap());
  println!("{:?}", parse_expr("a -> a -> a").unwrap());
  println!("{:?}", parse_expr("a -> a -> a -> a").unwrap());
  println!("{:?}", parse_expr("a b (c -> d)").unwrap());
  println!("{:?}", parse_expr("forall (a : data) -> a -> a").unwrap());
  println!("{:?}", parse_expr("(a -> b) c (d e)").unwrap());
  println!("{:?}", parse_expr("(a -> b) c (d e)").unwrap());
  match parse_expr("1") {
    Ok(_) => (),
    Err(e) => {
      println!("{:?}", e);
      ()
    }
  }
  println!("{:?}",
           parse_def("id : forall (a : data) -> a -> a.").unwrap());
  println!("{:?}",
           parse_def("id = \\(a : data) -> \\(x : a) -> x.").unwrap());
  println!("{:?}",
           parse_expr("forall (a : data -> data) -> a").unwrap());

  let m = r#"id : forall (a : data) -> a -> a.
id = \(a : data) -> \(x : a) -> x.
(\(id_b : forall (b : data) -> b -> b)
   -> id_b (forall (b : data) -> b -> b) id_b
) id."#;
  println!("-- module id");
  println!("{:?}", parse_mod(m).unwrap());
  println!("-- normalized module id");
  println!("{:?}", parse_mod(m).unwrap().normalize());
}
