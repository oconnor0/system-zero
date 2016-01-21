extern crate core;
use core::ast::*;

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
  println!("bool : {}.", ty_bool.to_string());
  let ty_true = var(&v_bool);
  println!("true : {}.", ty_true.to_string());
  let ty_false = var(&v_bool);
  println!("false : {}.", ty_false.to_string());
  let ty_if = lam(unused.clone(),
                  var(&v_bool),
                  pi(v_r.clone(),
                     constant(Const::Data),
                     lam(unused.clone(),
                         var(&v_r),
                         lam(unused.clone(), var(&v_r), var(&v_r)))));
  println!("if : {}.", ty_if.to_string());
  println!("");

  let impl_bool = pi(v_r.clone(),
                     constant(Const::Data),
                     lam(unused.clone(),
                         var(&v_r),
                         lam(unused.clone(), var(&v_r), var(&v_r))));
  println!("bool = {}.", impl_bool.to_string());
  let impl_true = pi(v_r.clone(),
                     constant(Const::Data),
                     lam(v_x.clone(),
                         var(&v_r),
                         lam(unused.clone(), var(&v_r), var(&v_x))));
  println!("true = {}.", impl_true.to_string());
  let impl_false = pi(v_r.clone(),
                      constant(Const::Data),
                      lam(unused.clone(),
                          var(&v_r),
                          lam(v_y.clone(), var(&v_r), var(&v_y))));
  println!("false = {}.", impl_false.to_string());
  let impl_if = lam(v_b.clone(), var(&v_bool), var(&v_b));
  println!("if = {}.", impl_if.to_string());
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
  println!("{}", if_true.to_string());

  let program = if_true;
  let execute = app(app(app(app(program, impl_bool.clone()),
                            impl_true.clone()),
                        impl_false.clone()),
                    impl_if.clone());
  println!("{}", execute.to_string());

  let result = execute.normalize();
  println!("= {}", result.to_string());
  println!("");

  let p2 = lam(v_false.clone(),
               ty_false.clone(),
               app(app(app(var(&v_false), var(&v_nat)), var(&v_1)), var(&v_0)));
  println!("{}", p2.to_string());

  let r2 = app(p2, impl_false.clone());
  println!("{}", r2.to_string());
  println!("= {}", r2.normalize().to_string());
  println!("");

  let ty_and = lam(unused.clone(),
                   var(&v_bool),
                   lam(unused.clone(), var(&v_bool), var(&v_bool)));
  println!("and : {}.", ty_and.to_string());
  // TODO: Does this need to be applied to if?
  let impl_and = app(lam(v_if.clone(),
                         ty_if.clone(),
                         lam(v_x.clone(),
                             var(&v_bool),
                             lam(v_y.clone(),
                                 var(&v_bool),
                                 app(app(app(app(var(&v_if), var(&v_x)),
                                             var(&v_bool)),
                                         var(&v_y)),
                                     var(&v_false))))),
                     impl_if.clone());
  println!("and = {}.", impl_and.to_string());
  println!("");

  let and_prog = lam(v_bool.clone(),
                     ty_bool.clone(),
                     lam(v_true.clone(),
                         ty_true.clone(),
                         lam(v_false.clone(),
                             ty_false.clone(),
                             lam(v_if.clone(),
                                 ty_if.clone(),
                                 lam(v_and.clone(),
                                     ty_and.clone(),
                                     app(app(var(&v_and),
                                             // var(&v_if)),
                                             var(&v_true)),
                                         var(&v_false)))))));
  // println!("{}", and_prog.to_string());
  let run_and_prog = app(app(app(app(app(and_prog, impl_bool.clone()),
                                     impl_true.clone()),
                                 impl_false.clone()),
                             impl_if.clone()),
                         impl_and.clone());
  println!("{}", run_and_prog.to_string());
  println!("= {}", run_and_prog.normalize().to_string());
}

#[test]
fn test_to_string() {
  let codata = Const::Codata;
  assert_eq!("codata", codata.to_string());
  let a = Var::new("a", 0);
  let x = Var::new("x", 0);
  let expra = var(&a);
  let exprx = var(&x);
  assert_eq!("x", x.to_string());
  let id = pi(a.clone(), constant(Const::Data), lam(x, expra, exprx));
  assert_eq!("forall (a : data) -> (x : a) -> x", id.to_string());
  let apply_id = app(app(id, var(&Var::new("int", 0))), var(&Var::new("1", 0)));
  assert_eq!("(forall (a : data) -> (x : a) -> x) int 1",
             apply_id.to_string());
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
              lam(unused, Expr::var(&a), Expr::var(&a)));
  // id's implementation
  let id_impl = pi(a.clone(),
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
