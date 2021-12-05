use std::cell::RefCell;
use std::rc::Rc;
use crate::{SourceRef};
use crate::parser::{ExprTy, ParserFunc};
use crate::scanner::StringInContext;

#[derive(Clone, serde::Serialize, Debug)]
pub struct Class {
    pub inner: Rc<ClassInner>,

}

#[derive(serde::Serialize, Debug)]
pub struct ClassInner {
    name: StringInContext,
    pub methods: RefCell<Vec<ParserFunc>>,
    #[serde(skip_serializing)]
    pub super_class: Option<RefCell<SuperClass>>,
}

#[derive(serde::Serialize, Debug)]
pub struct SuperClass {
    pub parent: ExprTy,
    pub name: String,
}

impl SuperClass {
    pub fn new(parent: ExprTy, name: String) -> SuperClass { SuperClass { parent, name } }
}

impl Class {
    pub fn new(name: StringInContext, methods: Vec<ParserFunc>, super_class: Option<RefCell<SuperClass>>) -> Class {
        Class { inner: Rc::new(ClassInner { super_class, name, methods: RefCell::new(methods) }) }
    }
    pub fn context(&self) -> SourceRef {
        self.inner.name.context.clone()
    }
    pub fn name(&self) -> &StringInContext {
        &self.inner.name
    }
}