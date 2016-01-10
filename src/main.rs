#[derive(Clone, Copy, Debug)]
enum Const {
  Data,
  Codata,
}

impl ToString for Const {
  fn to_string(&self) -> String {
    use Const::*;
    match *self {
      Data => "data".to_string(),
      Codata => "codata".to_string(),
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

impl ToString for Var {
  fn to_string(&self) -> String {
    self.name.clone()
     // + "@" + &self.idx.to_string()
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
}

impl ToString for Expr {
  fn to_string(&self) -> String {
    use Expr::*;
    match self {
      &Const(ref constant) => constant.to_string(),
      &Var(ref var) => var.to_string(),
      &Lam(ref var, ref ty, ref body) => {
        var.to_string() + ":" + &ty.to_string() + " -> " + &body.to_string()
      }
      &Pi(ref var, ref ty, ref body) => {
        "forall (".to_string() + &var.to_string() + ":" + &ty.to_string() +
        ") -> " + &body.to_string()
      }
      &App(ref f, ref arg) => f.to_string() + " " + &arg.to_string(),
    }
  }
}

fn main() {
  println!("{}", Const::Data.to_string());
  println!("{:?}", Expr::constant(Const::Data));
  let a = Var::new("a", 0);
  let x = Var::new("x", 0);
  let expra = Expr::var(&a);
  let exprx = Expr::var(&x);
  println!("{}", x.to_string());
  let id = Expr::pi(a.clone(),
                    Expr::constant(Const::Data),
                    Expr::lam(x, expra, exprx));
  println!("{}", id.to_string());
}
