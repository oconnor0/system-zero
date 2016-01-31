extern crate lalrpop_util;

pub mod ast;
mod parser;

use self::lalrpop_util::ParseError;

pub type Result<'input, T> =
  std::result::Result<T, ParseError<usize, (usize, &'input str), ()>>;

pub fn parse_def<'input>(input: &'input str)
                        -> Result<'input, ast::Def<'input>> {
  parser::parse_Def(input)
}

pub fn parse_expr<'input>(input: &'input str)
                          -> Result<'input, ast::Expr<'input>> {
  parser::parse_Expr(input)
}
