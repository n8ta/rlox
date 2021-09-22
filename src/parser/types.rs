use crate::scanner::{Token, TokenInContext, Literal};
use crate::scanner;
use crate::scanner::Token::{MINUS, AND, OR, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, PLUS, SLASH, MULT, LITERAL, LPAREN, RPAREN, BANG_EQUAL, EQUAL_EQUAL, VAR, SEMICOLON};
use crate::source_ref::SourceRef;
use std::rc::Rc;
use crate::parser::Expr::Logical;
use colored::*;
use crate::interpreter::{Interpreter, RuntimeException, interpret};
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use crate::environment::Env;
use std::borrow::BorrowMut;
use std::cell::RefCell;


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

#[derive(Clone, Debug)]
pub struct ExprInContext {
    pub context: SourceRef,
    pub expr: Expr,
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

pub trait Callable {
    fn arity(&self) -> u8;
    fn call(&self, globals: Env, args: Vec<Literal>, callsite: SourceRef) -> Result<Literal, RuntimeException>;
}


#[derive(Clone, Debug, PartialEq)]
struct FuncInner {
    name: String,
    args: Vec<(String, SourceRef)>,
    body: Vec<Stmt>,
    name_context: SourceRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    inner: Rc<FuncInner>
}


impl Func {
    pub fn new(name: String, args: Vec<(String, SourceRef)>, body: Vec<Stmt>, name_context: SourceRef) -> Func {
        let inner = FuncInner { name, args, body, name_context };
        Func { inner: Rc::new(inner) }
    }
    pub fn name(&self) -> &str {
        &self.inner.name
    }
}

impl Callable for Func {
    fn arity(&self) -> u8 {
        self.inner.args.len() as u8
    }
    fn call(&self, globals: Env, args: Vec<Literal>, callsite: SourceRef) -> Result<Literal, RuntimeException> {
        let mut new_env = Env::new(Some(globals.clone()));
        for i in 0..self.inner.args.len() {
            new_env.declare(&self.inner.args[i].0.clone(), &args[i]);
        }
        interpret(&self.inner.body, new_env, globals)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(ExprTy),
    Block(Vec<Stmt>),
    Print(ExprTy),
    Variable(String, Option<ExprTy>),
    If(ExprTy, Box<Vec<Stmt>>, Option<Box<Vec<Stmt>>>),
    While(ExprTy, Box<Stmt>),
    Function(Func),
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
    Literal(scanner::Literal),
    Unary(UnaryOp, ExprTy),
    Variable(String),
    Assign(String, ExprTy),
    Logical(ExprTy, LogicalOp, ExprTy),
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