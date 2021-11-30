use std::cell::RefCell;
use std::rc::Rc;
use crate::resolver::ScopeSize;
use crate::{RuntimeException, SourceRef, Value};

#[derive(Clone, Debug)]
pub struct FastEnv {
    inner: Rc<RefCell<FastEnvInner>>,
}

#[derive(Debug)]
struct FastEnvInner {
    values: Vec<Value>,
    parent: Option<FastEnv>,
}

impl FastEnvInner {
    fn new(parent: Option<FastEnv>, size: ScopeSize) -> FastEnvInner {
        FastEnvInner { values: vec![Value::NIL; size], parent }
    }
}

impl FastEnv {
    pub fn new(parent: Option<FastEnv>, size: ScopeSize) -> FastEnv {
        FastEnv { inner: Rc::new(RefCell::new(FastEnvInner::new(parent, size))) }
    }
    pub fn declare(&mut self, key: usize, name: &str, value: &Value) {
        let mut inner = self.inner.borrow_mut();
        if (key < inner.values.len()) {
            inner.values[key] = value.clone();
        } else {
            panic!("Compiler error - resolution error: len {}, offset: {}, name: {}", inner.values.len(), key, name)
        }

    }
    pub fn assign(&mut self, key: &str, scope: usize, offset: usize, value: &Value, context: &SourceRef) -> Result<(), RuntimeException> {
        let mut inner = self.inner.borrow_mut();
        if scope == 0 {
            inner.values[offset] = value.clone();
            Ok(())
        } else {
            match &mut inner.parent {
                None => Err(RuntimeException::new(format!("Cannot assign to {} it hasn't been declared", key), context.clone())),
                Some(enclosing) => enclosing.assign(key, scope - 1, offset, value, context),
            }
        }
    }
    pub fn fetch(&self, key: &str, scope: usize, offset: usize, context: &SourceRef) -> Result<Value, RuntimeException> {
        let mut inner = self.inner.borrow();
        if scope == 0 {
            Ok(inner.values[offset].clone())
        } else {
            match &inner.parent {
                None => Err(RuntimeException::new(format!("Cannot assign to {} it hasn't been declared", key), context.clone())),
                Some(enclosing) => enclosing.fetch(key, scope -1 , offset, context)
            }
        }
    }
    pub fn len(&self) -> usize {
        self.inner.borrow().values.len()
    }
    pub fn parent(&self) -> Option<FastEnv> {
        self.inner.borrow().parent.clone()
    }
}
