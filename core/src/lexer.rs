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
  Forall,
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
      Token::Forall => "forall".to_string(),
      Token::Variable(ref name) => name.clone(),
    }
  }
}

pub struct Lexer<I>
  where I: Clone + Iterator<Item = char>
{
  chars: Peekable<I>,
}

impl<I: Clone + Iterator<Item = char>> Lexer<I> {
  fn new(iter: I) -> Lexer<I> {
    Lexer { chars: iter.peekable() }
  }
}

impl<I: Clone + Iterator<Item = char>> Iterator for Lexer<I> {
  type Item = Token;
  fn next(&mut self) -> Option<Token> {
    match *self.chars.peek().unwrap_or(&'\0') {
      '(' => {
        self.chars.next();
        Some(Token::LParen)
      }
      ')' => {
        self.chars.next();
        Some(Token::RParen)
      }
      ':' => {
        self.chars.next();
        Some(Token::Colon)
      }
      '.' => {
        self.chars.next();
        Some(Token::Dot)
      }
      '=' => {
        self.chars.next();
        Some(Token::Equal)
      }
      '-' => {
        self.chars.next();
        if self.chars.peek().unwrap_or(&'\0') == &'>' {
          self.chars.next();
          Some(Token::Arrow)
        } else {
          None
        }
      }
      'A'...'Z' | 'a'...'z' | '_' => {
        let name = self.chars
                       .take_while_ref(|&c| {
                         c.is_lowercase() || c.is_uppercase() || c == '_'
                       })
                       .collect::<String>();
        if name == "forall" {
          Some(Token::Forall)
        } else {
          Some(Token::Variable(name))
        }
      }
      '\0' => None,
      _ => {
        self.chars.next();
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
  let mut l = lex("type = id : forall (a : data) -> a -> a.".chars());
  assert_eq!(l.next(), Some(Token::Variable("type".to_string())));
  assert_eq!(l.next(), Some(Token::Equal));
  assert_eq!(l.next(), Some(Token::Variable("id".to_string())));
  assert_eq!(l.next(), Some(Token::Colon));
  assert_eq!(l.next(), Some(Token::Forall));
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
