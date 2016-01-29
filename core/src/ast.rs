pub trait Normalize {
  fn normalize(&self) -> Self;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Const {
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Var {
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

  pub fn shift(&self) -> Var {
    Var {
      name: self.name.clone(),
      idx: self.idx + 1,
    }
  }
}

impl Normalize for Var {
  fn normalize(&self) -> Var {
    self.clone()
  }
}

impl ToString for Var {
  fn to_string(&self) -> String {
    if self.name.len() == 0 {
      "".to_string()
    } else if self.idx == 0 {
      self.name.clone()
    } else {
      self.name.clone() + "@" + &self.idx.to_string()
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

pub fn constant(c: Const) -> Expr {
  Expr::constant(c)
}
pub fn var(v: &Var) -> Expr {
  Expr::var(v)
}
pub fn lam(var: Var, ty: Expr, body: Expr) -> Expr {
  Expr::lam(var, ty, body)
}
pub fn pi(var: Var, ty: Expr, body: Expr) -> Expr {
  Expr::pi(var, ty, body)
}
pub fn app(f: Expr, arg: Expr) -> Expr {
  Expr::app(f, arg)
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

// fn find_first_var(body: &Expr) -> Var {
//   use self::Expr::*;
//   match *body {
//     Const(_) => panic!("constant"),
//     Var(ref var) => var.clone(),
//     Lam(ref var, _, _) => var.clone(),
//     Pi(ref var, _, _) => var.clone(),
//     App(ref f, _) => find_first_var(f),
//   }
// }

impl Normalize for Expr {
  fn normalize(&self) -> Expr {
    use self::Expr::*;
    // println!("normalize {}", self.to_string());
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

impl ToString for Expr {
  fn to_string(&self) -> String {
    use self::Expr::*;
    match *self {
      Const(ref constant) => constant.to_string(),
      Var(ref var) => var.to_string(),
      Lam(ref var, ref ty, ref body) => {
        let vars = var.to_string();
        // if vars.len() == 0 {
        //   "\\".to_string() + &ty.to_string() + " -> " + &body.to_string()
        // } else {
          "\\(".to_string() + &var.to_string() + " : " + &ty.to_string() +
          ") -> " + &body.to_string()
        // }
      }
      Pi(ref var, ref ty, ref body) => {
        let vars = var.to_string();
        if vars.len() == 0 {
          // "forall (".to_string() +
          ty.to_string() + " -> " + &body.to_string()
        } else {
          "forall (".to_string() + &var.to_string() + " : " +
          &ty.to_string() + ") -> " + &body.to_string()
        }
      }
      App(ref f, ref arg) => {
        let mut s = String::new();
        if f.is_lam() || f.is_pi() {
          s.push('(');
          s.push_str(&f.to_string()[..]);
          s.push(')');
        } else {
          s.push_str(&f.to_string()[..]);
        }
        s.push(' ');
        if arg.is_lam() || arg.is_pi() || arg.is_app() {
          s.push('(');
          s.push_str(&arg.to_string()[..]);
          s.push(')');
        } else {
          s.push_str(&arg.to_string()[..]);
        }
        s
      }
    }
  }
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
