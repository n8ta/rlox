use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::runtime::value::Value;
use crate::{Callable, RuntimeException, SourceRef};
use crate::runtime::class::RtClass;
use crate::runtime::func::Func;

#[derive(Clone, serde::Serialize, Debug)]
pub struct Instance {
    pub class: RtClass,
    fields: Rc<RefCell<HashMap<String, Value>>>,
}

impl Instance {
    pub fn new(class: RtClass) -> Instance {
        Instance { class, fields: Rc::new(RefCell::new(HashMap::new())) }
    }
    pub fn name(&self) -> &str {
        &self.class.name().string
    }

    pub fn find_method(&self, method: &str, context: &SourceRef) -> Option<Func> {
        match self.class.find_method(method, context) {
            Err(_) => None,
            Ok(func)  => Some(func.bind(&self)),
        }
    }

    pub fn get(&self, field: &str, context: &SourceRef) -> Result<Value, RuntimeException> {
        let fields = self.fields.borrow();
        let methods = &self.class.inner.methods;

        match fields.get(field) {
            None => {
                match self.find_method(field, context) {
                    None => Err(RuntimeException::new(
                        format!("Unable to find '{}' on {} it has fields \"{}\" and methods \"{}\"",
                                field,
                                &self.class.name().string,
                                fields.iter().map(|(a, _b)| a.to_string()).collect::<Vec<String>>().join(", "),
                                methods.iter().map(|(a, _b)| a.to_string()).collect::<Vec<String>>().join(", ")),
                                context.clone())),
                    Some(bound_method) => {
                        Ok(Value::FUNC(Rc::new(bound_method)))
                    }
                }
            }
            Some(value) => Ok(value.clone())
        }
    }
    pub fn set(&mut self, field: &str, value: Value) {
        self.fields.borrow_mut().insert(field.to_string(), value);
    }
}
