use std::cell::RefCell;
use crate::source_ref::SourceRef;
use crate::parser::types::{Stmt};
use std::rc::Rc;
use crate::resolver::ScopeSize;

#[derive(Clone, serde::Serialize, Debug)]
/// Parser Representation of a Func
pub struct ParserFunc {
    pub inner: Rc<ParserFuncInner>,
}

#[derive(Clone, serde::Serialize, Debug)]
pub struct ParserFuncInner {
    pub scope_size: RefCell<Option<ScopeSize>>,
    pub name: String,
    pub args: Vec<(String, SourceRef)>,
    pub body: RefCell<Stmt>,
    #[serde(skip_serializing)]
    pub name_context: SourceRef,
}
impl ParserFuncInner {
    fn new(name: String, args: Vec<(String, SourceRef)>, body: Stmt, name_context: SourceRef) -> ParserFuncInner {
        ParserFuncInner { name, args, body: RefCell::new(body), name_context, scope_size: RefCell::new(None) }
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