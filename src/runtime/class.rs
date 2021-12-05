use std::collections::HashMap;
use std::rc::Rc;
use crate::runtime::func::Func;
use crate::{Callable, RuntimeException, SourceRef, StringInContext, Value};
use crate::parser::Class;
use crate::runtime::Instance;

#[derive(Clone, serde::Serialize, Debug)]
pub struct RtClass {
    pub inner: Rc<RtClassInner>,

}

#[derive(serde::Serialize, Debug)]
pub struct RtClassInner {
    pub class: Class,
    pub methods: HashMap<String, Func>,
    pub super_class: Option<RtClass>,
}

impl Callable for RtClass {
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
        &self.inner.class.name()
    }
}

impl RtClass {
    pub fn new(class: Class, methods: HashMap<String, Func>, super_class: Option<RtClass>) -> RtClass {
        RtClass { inner: Rc::new(RtClassInner { class, methods, super_class })}
    }
}