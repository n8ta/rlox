use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::{Callable, SourceRef};
use crate::parser::{Expr, ParserFunc};
use crate::parser::types::Variable;
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
    pub methods: RefCell<Vec<ParserFunc>>,
    #[serde(skip_serializing)]
    pub runtime_methods: RefCell<HashMap<String, Func>>,
    pub super_class: Option<Variable>,
}

impl Class {
    pub fn new(name: StringInContext, methods: Vec<ParserFunc>, super_class: Option<Variable>) -> Class {
        Class { inner: Rc::new(ClassInner { super_class, name, methods: RefCell::new(methods), runtime_methods: RefCell::new(HashMap::new()) }) }
    }
    pub fn context(&self) -> SourceRef {
        self.inner.name.context.clone()
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