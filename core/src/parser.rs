use lexer::*;
use std::iter::Iterator;
use std::iter::Peekable;
// use itertools::Itertools;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Const {
  Data,
  Codata,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Var {
  name: String,
  idx: i32,
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

impl<I: Clone + Iterator<Item = char>> Parser<I> {
  fn new(lexer: Lexer<I>) -> Parser<I> {
    Parser { lexer: lexer.peekable() }
  }
}

pub fn parse<I: Clone + Iterator<Item = char>>(chars: I) -> Parser<I> {
  let lexer = lex(chars);
  Parser::new(lexer)
}

impl<I: Clone + Iterator<Item = char>> Iterator for Parser<I> {
  type Item = Tree;
  fn next(&mut self) -> Option<Tree> {
    match self.lexer.peek() {
      None => None,
      Some(tok) => {
        match tok {
          _ => None,
        }
      }
    }
  }
}

#[test]
fn test_app() {
  let mut p = parse("a b".chars());
  // assert_eq!(p.next(), None);
  assert_eq!(p.next(), None);
}
