use std::cell::RefCell;
use crate::source_ref::SourceRef;
use crate::parser::types::{Stmt};
use std::rc::Rc;
use crate::resolver::ScopeSize;
use crate::scanner::StringInContext;

#[derive(Clone, serde::Serialize, Debug)]
/// Parser Representation of a Func
pub struct ParserFunc {
    pub inner: Rc<ParserFuncInner>,
}

#[derive(Clone, serde::Serialize, Debug)]
pub struct ParserFuncInner {
    pub scope_size: RefCell<Option<ScopeSize>>,
    pub name: StringInContext,
    pub args: Vec<(StringInContext, SourceRef)>,
    pub body: RefCell<Stmt>,
    #[serde(skip_serializing)]
    pub name_context: SourceRef,
    #[serde(skip_serializing)]
    pub context: SourceRef,
}
impl ParserFuncInner {
    fn new(name: StringInContext, args: Vec<(StringInContext, SourceRef)>, body: Stmt, name_context: SourceRef, context: SourceRef) -> ParserFuncInner {
        ParserFuncInner { name, args, body: RefCell::new(body), name_context, scope_size: RefCell::new(None), context }
    }
}

impl ParserFunc {
    pub fn new(name: StringInContext, args: Vec<(StringInContext, SourceRef)>, body: Stmt, name_context: SourceRef, context: SourceRef) -> ParserFunc {
        ParserFunc { inner: Rc::new(ParserFuncInner::new(name, args, body, name_context, context)) }
    }
    pub fn name(&self) -> &StringInContext {
        &self.inner.name
    }
}