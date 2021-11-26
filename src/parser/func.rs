use crate::source_ref::SourceRef;
use crate::runtime::environment::Env;
use crate::parser::types::{Stmt};
use std::rc::Rc;

#[derive(Clone, Debug)]
/// Parser Representation of a Func
pub struct ParserFunc {
    pub name: String,
    pub args: Vec<(String, SourceRef)>,
    pub body: Vec<Stmt>,
    pub name_context: SourceRef,
}

impl ParserFunc {
    pub fn new(name: String, args: Vec<(String, SourceRef)>, body: Vec<Stmt>, name_context: SourceRef) -> ParserFunc {
        ParserFunc { name, args, body, name_context }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
}