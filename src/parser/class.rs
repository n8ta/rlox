use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use serde::ser::{Error};
use serde::{Serialize, Serializer};
use crate::{Callable, SourceRef};
use crate::parser::{ParserFunc};
use crate::runtime::func::Func;
use crate::runtime::instance::Instance;
use crate::runtime::value::Value;
use crate::runtime::interpreter::RuntimeException;
use crate::scanner::StringInContext;

#[derive(Clone, serde::Serialize, Debug)]
pub struct Class {
    pub inner: Rc<ClassInner>,

}

#[derive(serde::Serialize, Debug)]
pub struct ClassInner {
    name: StringInContext,
    #[serde(skip_serializing)]
    context: SourceRef,
    pub methods: RefCell<Vec<ParserFunc>>,
    #[serde(skip_serializing)]
    pub runtime_methods: RefCell<HashMap<String, Func>>,
}

impl Class {
    pub fn new(name: StringInContext, context: SourceRef, methods: Vec<ParserFunc>) -> Class {
        Class { inner: Rc::new(ClassInner { name, context, methods: RefCell::new(methods), runtime_methods: RefCell::new(HashMap::new()) }) }
    }
    pub fn context(&self) -> SourceRef {
        self.inner.context.clone()
    }
}

impl Callable for Class {
    fn arity(&self) -> u8 {
        0
    }

    fn call(&self, _args: Vec<Value>, callsite: SourceRef) -> Result<Value, RuntimeException> {
        let inst = Instance::new(self.clone());
        if let Some(bound_method) = inst.find_method("init", &callsite) {
            bound_method.call(vec![], callsite)?;
        }
        Ok(Value::INSTANCE(inst))
    }
    fn name(&self) -> &StringInContext {
        &self.inner.name
    }
}