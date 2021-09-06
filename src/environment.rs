use std::collections::HashMap;
use crate::scanner::Literal;
use crate::interpreter::RuntimeException;
use crate::source_ref::SourceRef;

pub struct Env {
    env: HashMap<String, Literal>,
}

impl Env {
    pub fn new() -> Env { Env { env: HashMap::new() } }
    pub fn declare(&mut self, key: &str, value: &Literal) {
        self.env.insert(key.to_string(), value.clone());
    }
    pub fn get(&mut self, key: &str, context: &SourceRef) -> Result<Literal, RuntimeException> {
        match self.env.get(key) {
            None => Err(RuntimeException::new(format!("Variable {} is undefined", key), context.clone())),
            Some(lit) => Ok(lit.clone()),
        }
    }
    pub fn assign(&mut self, key: &str, value: &Literal, context: &SourceRef) -> Result<(), RuntimeException> {
        match self.env.get(key) {
            None => Err(RuntimeException::new(format!("Cannot assign to {} it hasn't been declared", key), context.clone())),
            Some(_cur) => {
                self.env.insert(key.to_string(), value.clone());
                Ok(())
            },
        }
    }
}

#[test]
fn set_get() {
    let mut env = Env::new();
    let src = SourceRef::new(0,0,0);
    for i in 0..100 {
        env.declare(&format!("val-{}", i), &Literal::NUMBER(i as f64));
    }
    for i in 0..100 {
        assert_eq!(env.get(&format!("val-{}", i), &src).unwrap(), Literal::NUMBER(i as f64));
    }
    env.assign("val-10", &Literal::NUMBER(12.3), &src);
    env.assign("val-10", &Literal::NUMBER(14.3), &src);
    env.assign("val-10", &Literal::NUMBER(15.3), &src);
    assert_eq!(env.get("val-10", &src).unwrap(), Literal::NUMBER(15.3));
}

#[test]
#[should_panic]
fn get_unset() {
    let mut env = Env::new();
    let src = SourceRef::new(0,0,0);
    env.get("key", &src).unwrap();
}