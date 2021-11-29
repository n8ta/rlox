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

#[derive(Clone, serde::Serialize, Debug)]
pub struct Class {
    pub inner: Rc<ClassInner>,

}

#[derive(serde::Serialize, Debug)]
pub struct ClassInner {
    name: String,
    #[serde(skip_serializing)]
    context: SourceRef,
    pub methods: RefCell<Vec<ParserFunc>>,
    #[serde(skip_serializing)]
    pub runtime_methods: RefCell<HashMap<String, Func>>,
}

impl Class {
    pub fn new(name: String, context: SourceRef, methods: Vec<ParserFunc>) -> Class {
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

    fn call(&self, _args: Vec<Value>, _callsite: SourceRef) -> Result<Value, RuntimeException> {
        Ok(Value::INSTANCE(Instance::new(self.clone())))
    }
    fn name(&self) -> &str {
        &self.inner.name
    }
}