mod tests;
pub(crate) mod types;
pub(crate) mod parsing;

use crate::scanner::{Token, TokenInContext, Literal};
use crate::scanner;
use crate::scanner::Token::{MINUS, AND, OR, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, PLUS, SLASH, MULT, LITERAL, LPAREN, RPAREN, BANG_EQUAL, EQUAL_EQUAL, VAR, SEMICOLON};
use crate::source_ref::SourceRef;
use std::rc::Rc;
use colored::*;

use crate::parser::types::{ExprTy, Expr, UnaryOp, Stmt, LogicalOp, ParserError, BinOp, Tokens, ExprInContext};
use crate::parser::parsing::{parse};

