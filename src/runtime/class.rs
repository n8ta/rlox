use std::collections::HashMap;
use std::rc::Rc;
use crate::runtime::func::Func;
use crate::{Callable, RuntimeException, SourceRef, StringInContext, Value};
use crate::parser::{Class};
use crate::resolver::Resolved;
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

    fn call(&self, args: Vec<Value>, callsite: &SourceRef) -> Result<Value, RuntimeException> {
        let inst = Instance::new(self.clone());
        if let Some(bound_method) = inst.find_method("init", &callsite) {
            bound_method.call(args, callsite)?;
        }
        Ok(Value::INSTANCE(inst))
    }
    fn name(&self) -> &StringInContext {
        &self.inner.class.name()
    }
}

impl RtClass {
    pub fn new(class: Class, methods: HashMap<String, Func>, super_class: Option<RtClass>) -> RtClass {
        RtClass { inner: Rc::new(RtClassInner { class, methods, super_class }) }
    }
    pub fn find_method(&self, method: &str, context: &SourceRef) -> Result<Func, RuntimeException> {
        if let Some(method) = self.inner.methods.get(method) {
            Ok(method.clone())
        } else if let Some(parent) = &self.inner.super_class {
            parent.find_method(method, context)
        } else {
            Err(RuntimeException::new(format!("Couldn't find method '{}' on class '{}' and it has no super class", method, self.name().string), context.clone()))
        }
    }
}