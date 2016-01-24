use lexer::*;
use std::iter::Iterator;
use std::iter::Peekable;
use std::result;

// Informal grammar for System Zero
//
// definition = <variable> <equal> expr <dot>
// | <variable> <colon> expr <dot>
//
// expr = const
// | var
// | lam
// | pi
// | app
// | <lparen> expr <rparen>
//
// const = <variable>("data")
// | <variable>("codata")
//
// var = <variable>
//
// lam = <lparen> <variable> <colon> expr <rparen> <arrow> expr
// | expr <arrow> expr
//
// pi = <forall> <lparen> <variable> <colon> expr <rparen> <arrow> expr
//
// app = expr expr

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Const {
  Data,
  Codata,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Var {
  name: String,
}

impl Var {
  pub fn new(name: &str) -> Var {
    Var { name: name.to_string() }
  }
}

impl ToString for Var {
  fn to_string(&self) -> String {
    self.name.clone()
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprTree {
  // Type system constants
  Const(Const),
  // Bound variables
  Var(Var),
  // Lambda
  Lam(Var, Box<ExprTree>, Box<ExprTree>),
  // "forall"
  Pi(Var, Box<ExprTree>, Box<ExprTree>),
  // Function application
  App(Box<ExprTree>, Box<ExprTree>),
}

pub struct Parser<I>
  where I: Clone + Iterator<Item = char>
{
  lexer: Peekable<Lexer<I>>,
}

pub type Error = String;
pub type Result<T> = result::Result<T, Error>;

impl<I: Clone + Iterator<Item = char>> Parser<I> {
  fn new(lexer: Lexer<I>) -> Parser<I> {
    Parser { lexer: lexer.peekable() }
  }

  // Informal grammar for System Zero
  //
  // definition = <variable> <equal> expr <dot>
  // | <variable> <colon> expr <dot>
  //
  // expr = const
  // | var
  // | lam
  // | pi
  // | app
  // | <lparen> expr <rparen>
  //
  // const = <data>
  // | <codata>
  //
  // var = <variable>
  //
  // lam = <lparen> <variable> <colon> expr <rparen> <arrow> expr
  // | expr <arrow> expr
  //
  // pi = <forall> <lparen> <variable> <colon> expr <rparen> <arrow> expr
  //
  // app = expr expr

  pub fn parse_expr(&mut self) -> Result<ExprTree> {
    use self::ExprTree::*;
    match self.lexer.next() {
      None => Err("eof".to_string()),
      Some(tok) => {
        match tok {
          Token::Data => Ok(Const(self::Const::Data)),
          Token::Codata => Ok(Const(self::Const::Codata)),
          Token::Forall => {
            // parse Pi
            if let Some(Token::LParen) = self.lexer.next() {
              if let Some(Token::Variable(ref name)) = self.lexer.next() {
                if let Some(Token::Colon) = self.lexer.next() {
                  let ty = try!(self.parse_expr());
                  if let Some(Token::RParen) = self.lexer.next() {
                    if let Some(Token::Arrow) = self.lexer.next() {
                      let expr = try!(self.parse_expr());
                      Ok(ExprTree::Pi(self::Var::new(name),
                                      Box::new(ty),
                                      Box::new(expr)))
                    } else {
                      Err("expected '->'".to_string())
                    }
                  } else {
                    Err("expected ')'".to_string())
                  }
                } else {
                  Err("expected ':'".to_string())
                }
              } else {
                Err("expected variable name".to_string())
              }
            } else {
              Err("expected '('".to_string())
            }
          }
          Token::Variable(ref name) => Ok(Var(self::Var::new(name))),
          _ => Err("eof".to_string()),
        }
      }
    }
  }
}

pub fn parse<I: Clone + Iterator<Item = char>>(chars: I) -> Result<ExprTree> {
  let lexer = lex(chars);
  let mut parser = Parser::new(lexer);
  parser.parse_expr()
}

#[test]
fn test_app() {
  let p = parse("a b".chars());
  // assert_eq!(p.next(), None);
  assert_eq!(p.ok().unwrap(), ExprTree::Var(Var::new("a")));
  let fa = parse("forall (a : data) -> a".chars());
  assert_eq!(fa.ok().unwrap(),
             ExprTree::Pi(Var::new("a"),
                          Box::new(ExprTree::Const(Const::Data)),
                          Box::new(ExprTree::Var(Var::new("a")))));
}
