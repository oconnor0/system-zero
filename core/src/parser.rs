use lexer::*;
use std::iter::Iterator;
use std::iter::Peekable;
use std::result;
// use itertools::Itertools;

/*
Informal BNF grammar for System Zero

definition = <variable> <equal> expr <dot>
           | <variable> <colon> expr <dot>

expr = const
     | var
     | lam
     | pi
     | app
     | <lparen> expr <rparen>

const = <variable>("data")
      | <variable>("codata")

var = <variable>

lam = <lparen> <variable> <colon> expr <rparen> <arrow> expr
    | expr <arrow> expr

pi = <forall> <lparen> <variable> <colon> expr <rparen> <arrow> expr

app = expr expr

*/

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Const {
  Data,
  Codata,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Var {
  name: String
}

impl Var {
  pub fn new(name: &str) -> Var {
    Var {
      name: name.to_string()
    }
  }
}

impl ToString for Var {
  fn to_string(&self) -> String {
    self.name.clone()
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Tree {
  // Type system constants
  Const(Const),
  // Bound variables
  Var(Var),
  // Lambda
  Lam(Var, Box<Tree>, Box<Tree>),
  // "forall"
  Pi(Var, Box<Tree>, Box<Tree>),
  // Function application
  App(Box<Tree>, Box<Tree>),
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

  pub fn parse(&mut self) -> Result<Tree> {
    match self.lexer.peek() {
      None => Err("eof".to_string()),
      Some(tok) => {
        match *tok {
          Token::Variable(ref name) => Ok(Tree::Var(Var::new(name))),
          _ => Err("eof".to_string()),
        }
      }
    }
  }
}

pub fn parse<I: Clone + Iterator<Item = char>>(chars: I) -> Result<Tree> {
  let lexer = lex(chars);
  let mut parser = Parser::new(lexer);
  parser.parse()
}

#[test]
fn test_app() {
  let p = parse("a b".chars());
  // assert_eq!(p.next(), None);
  assert_eq!(p.ok().unwrap(), Tree::Var(Var::new("a")));
}
