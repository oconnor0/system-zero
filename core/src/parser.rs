use lexer::*;
use std::iter::Iterator;
use std::iter::Peekable;
use std::result;
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

impl Var {
  pub fn new(name: &str, idx: i32) -> Var {
    Var {
      name: name.to_string(),
      idx: idx,
    }
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
          Token::Variable(ref name) => Ok(Tree::Var(Var::new(name, 0))),
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

// impl<I: Clone + Iterator<Item = char>> Iterator for Parser<I> {
//   type Item = Tree;
//   fn next(&mut self) -> Option<Tree> {
//     match self.lexer.peek() {
//       None => None,
//       Some(tok) => {
//         match tok {
//           _ => None,
//         }
//       }
//     }
//   }
// }

#[test]
fn test_app() {
  let p = parse("a b".chars());
  // assert_eq!(p.next(), None);
  assert_eq!(p.ok().unwrap(), Tree::Var(Var::new("a", 0)));
}
