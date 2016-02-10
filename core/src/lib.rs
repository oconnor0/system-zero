// Copyright (c) 2016, Matthew O'Connor

//! System Zero Core defines the core abstract syntax tree for System Zero.
//! The AST is an implementation of a total, dependently typed lambda calculus.
//! This is strongly normalizing lambda calculus and a trait to normalize is
//! implemented.

extern crate lalrpop_util;

pub mod ast;
mod parser;

use self::lalrpop_util::ParseError;

/// The specific result returned by the defined parse functions.
pub type Result<'input, T> =
  std::result::Result<T, ParseError<usize, (usize, &'input str), ()>>;

/// Parses a module in a `&'input str` into either an
/// `ast::Mod` or reports a `lalrpop_util::ParseError`.
pub fn parse_mod<'input>(input: &'input str) -> Result<'input, ast::Mod> {
  parser::parse_Mod(input)
}

/// Parses a single-entry in a `&'input str` into
/// either an `ast::One` or reports a `lalrpop_util::ParseError`.
pub fn parse_one<'input>(input: &'input str) -> Result<'input, ast::One> {
  parser::parse_One(input)
}

/// Parses a type or value binding in a `&'input str` into
/// either an `ast::Def` or reports a `lalrpop_util::ParseError`.
pub fn parse_def<'input>(input: &'input str) -> Result<'input, ast::Def> {
  parser::parse_Def(input)
}

/// Parses an expression in a `&'input str` into
/// either an `ast::Expr` or reports a `lalrpop_util::ParseError`.
pub fn parse_expr<'input>(input: &'input str) -> Result<'input, ast::Expr> {
  parser::parse_Expr(input)
}
