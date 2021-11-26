use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::runtime::value::Value;
use crate::parser::Class;
use crate::{Callable, RuntimeException, SourceRef};
use crate::runtime::func::Func;

#[derive(Clone, Debug)]
pub struct Instance {
    pub class: Class,
    fields: Rc<RefCell<HashMap<String, Value>>>,
}

impl Instance {
    pub fn new(class: Class) -> Instance {
        Instance { class, fields: Rc::new(RefCell::new(HashMap::new())) }
    }
    pub fn name(&self) -> &str {
        self.class.name()
    }

    pub fn get(&self, field: &str, context: &SourceRef) -> Result<Value, RuntimeException> {
        let fields = self.fields.borrow_mut();
        let methods = self.class.inner.runtime_methods.borrow_mut();

        match fields.get(field) {
            None => {
                match methods.get(field) {
                    None => Err(RuntimeException::new(
                        format!("Unable to find field {} on {} it has fields \"{}\"",
                                field,
                                self.class.name(),
                                fields.iter().map(|(a, b)| a.to_string()).collect::<Vec<String>>().join(", ")),
                        context.clone())),
                    Some(func) => Ok(func.clone()),
                }
            }
            Some(value) => Ok(value.clone())
        }
    }
    pub fn set(&mut self, field: &str, value: Value) {
        self.fields.borrow_mut().insert(field.to_string(), value);
    }
}
