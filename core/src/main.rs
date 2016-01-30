pub mod calc;
pub mod ast; //::calc;

#[test]
fn calculator4() {
  assert_eq!(&format!("{:?}", calc::parse_Expr("a - 22 * 44 + 66").unwrap()),
             "((a - (22 * 44)) + 66)");
}

#[cfg(not(test))]
fn main() {
  use ast::*;

  let codata = Const::Codata;
  println!("{:?}", codata);
  let a = Var::new("a", 0);
  let x = Var::new("x", 0);
  let expra = var(&a);
  let exprx = var(&x);
  println!("{:?}", x);
  let id = lam(a.clone(), constant(Const::Data), lam(x, expra, exprx));
  println!("{:?}", id);
  let apply_id = app(app(id, var(&Var::new("int", 0))), var(&Var::new("1", 0)));
  println!("{:?}", apply_id);
}
