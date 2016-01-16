trait Normalize {
  fn normalize(&self) -> Self;
}

#[derive(Clone, Copy, Debug)]
enum Const {
  Data,
  Codata,
}

impl Normalize for Const {
  fn normalize(&self) -> Const {
    *self
  }
}

impl ToString for Const {
  fn to_string(&self) -> String {
    match *self {
      Const::Data => "data".to_string(),
      Const::Codata => "codata".to_string(),
    }
  }
}

#[derive(Clone, Debug)]
struct Var {
  name: String,
  idx: i32,
}

impl Var {
  pub fn new(name: &str, idx: i32) -> Var {
    Var {
      name: name.to_string(),
      idx: idx,
    }
  }
}

fn vn(name: &str, idx: i32) -> Var {
  Var::new(name, idx)
}

fn v(name: &str) -> Var {
  vn(name, 0)
}

impl Normalize for Var {
  fn normalize(&self) -> Var {
    self.clone()
  }
}

impl ToString for Var {
  fn to_string(&self) -> String {
    if self.idx == 0 {
      self.name.clone()
    } else {
      self.name.clone() + "@" + &self.idx.to_string()
    }
  }
}

#[derive(Clone, Debug)]
enum Expr {
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
    use Expr::*;
    match *self {
      Var(_) => true,
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

impl Normalize for Expr {
  fn normalize(&self) -> Expr {
    use Expr::*;
    match *self {
      Const(ref constant) => Const(constant.normalize()),
      Var(ref var) => Var(var.normalize()),
      Lam(ref var, ref ty, ref body) => {
        let l = &Lam(var.normalize(),
                     Box::new(ty.normalize()),
                     Box::new(body.normalize()));
        l.clone()
      }
      Pi(ref var, ref ty, ref body) => {
        let p = &Pi(var.normalize(),
                    Box::new(ty.normalize()),
                    Box::new(body.normalize()));
        p.clone()
      }
      App(ref f, ref arg) => {
        let a = &App(Box::new(f.normalize()), Box::new(arg.normalize()));
        a.clone()
      }
    }
  }
}

impl ToString for Expr {
  fn to_string(&self) -> String {
    use Expr::*;
    match *self {
      Const(ref constant) => constant.to_string(),
      Var(ref var) => var.to_string(),
      Lam(ref var, ref ty, ref body) => {
        "(".to_string() + &var.to_string() + " : " + &ty.to_string() + ") -> " +
        &body.to_string()
      }
      Pi(ref var, ref ty, ref body) => {
        "forall (".to_string() + &var.to_string() + " : " + &ty.to_string() +
        ") -> " + &body.to_string()
      }
      App(ref f, ref arg) => {
        if f.is_lam() || f.is_pi() {
          "(".to_string() + &f.to_string() + ") " + &arg.to_string()
        } else {
          f.to_string() + " " + &arg.to_string()
        }
      }
    }
  }
}

fn main() {
  println!("{}", Const::Data.to_string());
  let a = v("a");
  let x = v("x");
  let expra = var(&a);
  let exprx = var(&x);
  println!("{}", x.to_string());
  let id = pi(a.clone(), constant(Const::Data), lam(x, expra, exprx));
  println!("{}", id.to_string());
  let apply_id = app(app(id, var(&v("int"))), var(&v("1")));
  println!("{}", apply_id.to_string());
  println!("{}", apply_id.normalize().to_string());
}

#[test]
fn test_to_string() {
  let codata = Const::Codata;
  println!("{}", codata.to_string());
  assert_eq!("codata", codata.to_string());
  let a = v("a");
  let x = v("x");
  let expra = var(&a);
  let exprx = var(&x);
  println!("{}", x.to_string());
  assert_eq!("x", x.to_string());
  let id = pi(a.clone(), constant(Const::Data), lam(x, expra, exprx));
  println!("{}", id.to_string());
  assert_eq!("forall (a : data) -> (x : a) -> x", id.to_string());
  let apply_id = app(app(id, var(&v("int"))), var(&v("1")));
  println!("{}", apply_id.to_string());
  assert_eq!("(forall (a : data) -> (x : a) -> x) int 1",
             apply_id.to_string());
}
