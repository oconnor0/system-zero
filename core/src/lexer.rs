use std::iter::Iterator;
use std::iter::Peekable;
use itertools::Itertools;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
  LParen,
  RParen,
  Arrow,
  Dot,
  Colon,
  Equal,
  Variable(String),
}

impl ToString for Token {
  fn to_string(&self) -> String {
    match *self {
      Token::LParen => "(".to_string(),
      Token::RParen => ")".to_string(),
      Token::Arrow => "->".to_string(),
      Token::Dot => ".".to_string(),
      Token::Colon => ":".to_string(),
      Token::Equal => "=".to_string(),
      Token::Variable(ref name) => name.clone(),
    }
  }
}

pub struct Lexer<I>
  where I: Clone + Iterator<Item = char>
{
  iter: Peekable<I>,
}

impl<I: Clone + Iterator<Item = char>> Lexer<I> {
  fn new(iter: I) -> Lexer<I> {
    Lexer { iter: iter.peekable() }
  }
}

impl<I: Clone + Iterator<Item = char>> Iterator for Lexer<I> {
  type Item = Token;
  fn next(&mut self) -> Option<Token> {
    match *self.iter.peek().unwrap_or(&'\0') {
      '(' => {
        self.iter.next();
        Some(Token::LParen)
      }
      ')' => {
        self.iter.next();
        Some(Token::RParen)
      }
      ':' => {
        self.iter.next();
        Some(Token::Colon)
      }
      '.' => {
        self.iter.next();
        Some(Token::Dot)
      }
      '=' => {
        self.iter.next();
        Some(Token::Equal)
      }
      '-' => {
        self.iter.next();
        if self.iter.peek().unwrap_or(&'\0') == &'>' {
          self.iter.next();
          Some(Token::Arrow)
        } else {
          None
        }
      }
      'A'...'Z' | 'a'...'z' | '_' => {
        Some(Token::Variable(self.iter
                                 .take_while_ref(|&c| {
                                   c.is_lowercase() || c.is_uppercase() ||
                                   c == '_'
                                 })
                                 .collect::<String>()))
      }
      '\0' => None,
      _ => {
        self.iter.next();
        self.next()
      }
    }
  }
}

pub fn lex<I: Clone + Iterator<Item = char>>(iter: I) -> Lexer<I> {
  Lexer::new(iter)
}

#[test]
fn test_to_string() {
  assert_eq!("(", Token::LParen.to_string());
  assert_eq!(")", Token::RParen.to_string());
  assert_eq!("->", Token::Arrow.to_string());
  assert_eq!(".", Token::Dot.to_string());
  assert_eq!(":", Token::Colon.to_string());
  assert_eq!("=", Token::Equal.to_string());
  assert_eq!("a", Token::Variable("a".to_string()).to_string());
}

#[test]
fn test_lex() {
  let mut l = lex("type = id : (a : data) -> a -> a.".chars());
  assert_eq!(l.next(), Some(Token::Variable("type".to_string())));
  assert_eq!(l.next(), Some(Token::Equal));
  assert_eq!(l.next(), Some(Token::Variable("id".to_string())));
  assert_eq!(l.next(), Some(Token::Colon));
  assert_eq!(l.next(), Some(Token::LParen));
  assert_eq!(l.next(), Some(Token::Variable("a".to_string())));
  assert_eq!(l.next(), Some(Token::Colon));
  assert_eq!(l.next(), Some(Token::Variable("data".to_string())));
  assert_eq!(l.next(), Some(Token::RParen));
  assert_eq!(l.next(), Some(Token::Arrow));
  assert_eq!(l.next(), Some(Token::Variable("a".to_string())));
  assert_eq!(l.next(), Some(Token::Arrow));
  assert_eq!(l.next(), Some(Token::Variable("a".to_string())));
  assert_eq!(l.next(), Some(Token::Dot));
  assert_eq!(l.next(), None);
}
