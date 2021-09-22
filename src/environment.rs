use std::collections::HashMap;
use crate::scanner::Literal;
use crate::interpreter::RuntimeException;
use crate::source_ref::{SourceRef, Source};
use std::rc::Rc;
use std::cell::{RefCell};

#[derive(Clone, Debug)]
struct EnvInner {
    env: HashMap<String, Literal>,
    enclosing: Option<Env>,
}

#[derive(Clone, Debug)]
pub struct Env {
    inner: Rc<RefCell<EnvInner>>
}

impl Env {
    pub fn new(parent: Option<Env>) -> Env {
        Env { inner: Rc::new(RefCell::new(EnvInner::new(parent))) }
    }
    pub fn declare(&mut self, key: &str, value: &Literal) {
        let mut inner = self.inner.borrow_mut();
        inner.declare(key, value);
    }
    pub fn fetch(&mut self, key: &str, context: &SourceRef) -> Result<Literal, RuntimeException> {
        let mut inner = self.inner.borrow_mut();
        inner.fetch(key, context)
    }
    pub fn assign(&mut self, key: &str, value: &Literal, context: &SourceRef) -> Result<(), RuntimeException> {
        let mut inner = self.inner.borrow_mut();
        inner.assign(key, value, context)
    }
}

impl EnvInner {
    fn new(enclosing: Option<Env>) -> EnvInner { EnvInner { env: HashMap::new(), enclosing } }
    fn declare(&mut self, key: &str, value: &Literal) {
        self.env.insert(key.to_string(), value.clone());
    }
    fn fetch(&mut self, key: &str, context: &SourceRef) -> Result<Literal, RuntimeException> {
        let result = self.env.get(key);
        if let Some(res) = result {
            return Ok(res.clone());
        };

        if let Some(mut parent) = self.enclosing.clone() {
            return parent.fetch(key, context)
        }
        return Err(RuntimeException::new(format!("Variable {} is undefined", key), context.clone()));
    }
    fn assign(&mut self, key: &str, value: &Literal, context: &SourceRef) -> Result<(), RuntimeException> {
        match self.env.get(key) {
            None => {
                match &mut self.enclosing {
                    None => Err(RuntimeException::new(format!("Cannot assign to {} it hasn't been declared", key), context.clone())),
                    Some(enclosing) => {
                        enclosing.assign(key, value, context)
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
    let mut env = EnvInner::new(None);
    let src = SourceRef::new(0, 0, 0, Rc::new(Source::new(String::from("testing"))));
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
    let mut env = EnvInner::new(None);
    let src = SourceRef::new(0, 0, 0, Rc::new(Source::new(String::from("test"))));
    env.fetch("key", &src).unwrap();
}

#[test]
fn pull_from_parent() {
    let src = SourceRef::new(0, 0, 0, Rc::new(Source::new(String::from("aksjdflaksjfklas"))));
    let lit = Literal::STRING(format!("Helllloooo!"));
    let mut parent = Env::new(None);
    let mut env = Env::new(Some(parent.clone()));
    parent.declare("hello", &lit);
    assert_eq!(env.fetch("hello", &src).unwrap(), lit);
}