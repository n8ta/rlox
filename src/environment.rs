use std::collections::HashMap;
use crate::scanner::Literal;
use crate::interpreter::RuntimeException;
use crate::source_ref::SourceRef;
use std::rc::Rc;
use std::cell::{RefCell};

pub struct Env {
    env: HashMap<String, Literal>,
    enclosing: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new(enclosing: Option<Rc<RefCell<Env>>>) -> Env { Env { env: HashMap::new(), enclosing } }

    pub fn declare(&mut self, key: &str, value: &Literal) {
        self.env.insert(key.to_string(), value.clone());
    }

    pub fn fetch(&mut self, key: &str, context: &SourceRef) -> Result<Literal, RuntimeException> {
        let result = self.env.get(key);
        if let Some(res) = result {
            return Ok(res.clone());
        };

        if let Some(parent) = self.enclosing.clone() {
            let parent = parent.clone();
            let mut env = parent.borrow_mut();
            let parent_get = env.fetch(key, context);
            if parent_get.is_ok() {
                return parent_get;
            }
        }
        return Err(RuntimeException::new(format!("Variable {} is undefined", key), context.clone()));
    }

    pub fn assign(&mut self, key: &str, value: &Literal, context: &SourceRef) -> Result<(), RuntimeException> {
        match self.env.get(key) {
            None => {
                match &mut self.enclosing {
                    None => Err(RuntimeException::new(format!("Cannot assign to {} it hasn't been declared", key), context.clone())),
                    Some(enclosing) => {
                        enclosing.clone().borrow_mut().assign(key, value, context)
                    }
                }
            }
            Some(_cur) => {
                self.env.insert(key.to_string(), value.clone());
                Ok(())
            }
        }
    }
}

#[test]
fn set_get() {
    let mut env = Env::new(None);
    let src = SourceRef::new(0, 0, 0, Rc::new(String::from("testing")));
    for i in 0..100 {
        env.declare(&format!("val-{}", i), &Literal::NUMBER(i as f64));
    }
    for i in 0..100 {
        assert_eq!(env.fetch(&format!("val-{}", i), &src).unwrap(), Literal::NUMBER(i as f64));
    }
    env.assign("val-10", &Literal::NUMBER(12.3), &src).unwrap();
    env.assign("val-10", &Literal::NUMBER(14.3), &src).unwrap();
    env.assign("val-10", &Literal::NUMBER(15.3), &src).unwrap();
    assert_eq!(env.fetch("val-10", &src).unwrap(), Literal::NUMBER(15.3));
}

#[test]
#[should_panic]
fn get_unset() {
    let mut env = Env::new(None);
    let src = SourceRef::new(0, 0, 0, Rc::new(String::from("test")));
    env.fetch("key", &src).unwrap();
}