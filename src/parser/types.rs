use crate::scanner::{Token, TokenInContext};
use crate::scanner::Token::{MINUS, AND, OR, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, PLUS, SLASH, MULT, BANG_EQUAL, EQUAL_EQUAL};
use crate::source_ref::SourceRef;
use std::fmt::{Debug, Formatter};
use crate::parser::{Class, ParserFunc};
use crate::resolver::{Resolved, ScopeSize};


#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct ParserError {
    pub msg: String,
    pub context: SourceRef,
}

impl ParserError {
    pub fn new(msg: String, context: SourceRef) -> ParserError { ParserError { msg, context } }
}

pub type Tokens = Vec<TokenInContext>;

pub type ExprTy = Box<ExprInContext>;

#[derive(Clone)]
pub struct ExprInContext {
    pub context: SourceRef,
    pub expr: Expr,
}

impl Debug for ExprInContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("ExprInContext [expr:{:?}]", self.expr))
    }
}

impl PartialEq for ExprInContext {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl ExprInContext {
    pub fn new(expr: Expr, context: SourceRef) -> ExprInContext {
        ExprInContext { expr, context }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprTy),
    Block(Box<Vec<Stmt>>, Option<ScopeSize>),
    Print(ExprTy),
    Variable(String, Option<ExprTy>, Option<Resolved>),
    If(ExprTy, Box<Stmt>, Option<Box<Stmt>>),
    While(ExprTy, Box<Stmt>),
    Function(ParserFunc, Option<Resolved>),
    Return(Option<ExprTy>, SourceRef),
    Class(Class, Option<Resolved>, Option<ScopeSize>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOp {
    AND,
    OR,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(ExprTy, BinOp, ExprTy),
    Call(ExprTy, Vec<ExprTy>),
    Grouping(ExprTy),
    Get(ExprTy, String),
    Literal(crate::runtime::Value),
    Unary(UnaryOp, ExprTy),
    Set(ExprTy, String, ExprTy),
    Variable(String, Option<Resolved>),
    Assign(String, ExprTy, Option<Resolved>),
    Logical(ExprTy, LogicalOp, ExprTy),
    This(Option<Resolved>),
}

#[allow(non_camel_case_types)]
#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum BinOp {
    EQUAL_EQUAL,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    PLUS,
    SLASH,
    MULT,
    MINUS,
    GREATER,
    GREATER_EQUAL,
    AND,
    OR,
}

#[allow(non_camel_case_types)]
#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum UnaryOp {
    MINUS,
    BANG,
}

impl LogicalOp {
    pub fn new(tk: Token) -> LogicalOp {
        match tk {
            AND => LogicalOp::AND,
            OR => LogicalOp::OR,
            _ => panic!("{:?} is not a valid logical op", tk)
        }
    }
}

impl BinOp {
    pub fn new(tk: Token) -> BinOp {
        match tk {
            PLUS => BinOp::PLUS,
            MULT => BinOp::MULT,
            EQUAL_EQUAL => BinOp::EQUAL_EQUAL,
            BANG_EQUAL => BinOp::BANG_EQUAL,
            MINUS => BinOp::MINUS,
            SLASH => BinOp::SLASH,
            GREATER => BinOp::GREATER,
            GREATER_EQUAL => BinOp::GREATER_EQUAL,
            LESS => BinOp::LESS,
            LESS_EQUAL => BinOp::LESS_EQUAL,
            AND => BinOp::AND,
            OR => BinOp::OR,
            _ => panic!("{:?} is not a valid binary op", tk)
        }
    }
}

impl UnaryOp {
    pub fn new(tk: Token) -> UnaryOp {
        match tk {
            Token::BANG => UnaryOp::BANG,
            Token::MINUS => UnaryOp::MINUS,
            _ => panic!("{:?} is not a valid unary op", tk)
        }
    }
}

pub type ExprResult = Result<ExprTy, ParserError>;