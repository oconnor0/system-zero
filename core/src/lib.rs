// Copyright (c) 2016, Matthew O'Connor
extern crate lalrpop_util;

pub mod ast;
mod parser;

use self::lalrpop_util::ParseError;

pub type Result<'input, T> =
  std::result::Result<T, ParseError<usize, (usize, &'input str), ()>>;

pub fn parse_mod<'input>(input: &'input str)
                         -> Result<'input, Box<ast::Mod<'input>>> {
  parser::parse_Mod(input)
}

pub fn parse_one<'input>(input: &'input str)
                         -> Result<'input, Box<ast::One<'input>>> {
  parser::parse_One(input)
}

pub fn parse_def<'input>(input: &'input str)
                         -> Result<'input, Box<ast::Def<'input>>> {
  parser::parse_Def(input)
}

pub fn parse_expr<'input>(input: &'input str)
                          -> Result<'input, Box<ast::Expr<'input>>> {
  parser::parse_Expr(input)
}
