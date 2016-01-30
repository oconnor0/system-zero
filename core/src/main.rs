pub mod calc;
pub mod ast; //::calc;
mod parser;

#[test]
fn calculator4() {
  assert_eq!(&format!("{:?}", calc::parse_Expr("a - 22 * 44 + 66").unwrap()),
             "((a - (22 * 44)) + 66)");
}

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

  println!("{:?}", parser::parse_Expr("a").unwrap());
  println!("{:?}", parser::parse_Expr("data").unwrap());
  println!("{:?}", parser::parse_Expr("codata").unwrap());
  println!("{:?}", parser::parse_Expr("\\ (a : data) -> a").unwrap());
  println!("{:?}",
           parser::parse_Expr("\\ (a : data) -> \\ (a : data) -> a").unwrap());
  println!("{:?}",
           parser::parse_Expr("forall (a : data) -> a").unwrap());
  println!("{:?}",
           parser::parse_Expr("forall (a : data) -> forall (b : data) -> a")
             .unwrap());
  println!("{:?}",
           parser::parse_Expr("forall (a : data) -> \\ (a : data) -> a")
             .unwrap());
  println!("{:?}", parser::parse_Expr("a -> a").unwrap());
  // println!("{:?}", parser::parse_Expr("forall (a : data) -> a -> a").unwrap());
}
