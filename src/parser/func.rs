use std::cell::RefCell;
use crate::source_ref::SourceRef;
use crate::runtime::environment::Env;
use crate::parser::types::{Stmt};
use std::rc::Rc;

#[derive(Clone, Debug)]
/// Parser Representation of a Func
pub struct ParserFunc {
    pub inner: Rc<ParserFuncInner>,
}

#[derive(Clone, Debug)]
pub struct ParserFuncInner {
    pub name: String,
    pub args: Vec<(String, SourceRef)>,
    pub body: RefCell<Stmt>,
    pub name_context: SourceRef,
}
impl ParserFuncInner {
    fn new(name: String, args: Vec<(String, SourceRef)>, body: Stmt, name_context: SourceRef) -> ParserFuncInner {
        ParserFuncInner { name, args, body: RefCell::new(body), name_context }
    }
}

impl ParserFunc {
    pub fn new(name: String, args: Vec<(String, SourceRef)>, body: Stmt, name_context: SourceRef) -> ParserFunc {
        ParserFunc { inner: Rc::new(ParserFuncInner::new(name, args, body, name_context)) }
    }
    pub fn name(&self) -> &str {
        &self.inner.name
    }
}