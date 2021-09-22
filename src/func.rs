use crate::interpreter::{LoxControlFlow, interpret, RuntimeException};
use crate::source_ref::SourceRef;
use crate::scanner::Literal;
use crate::environment::Env;
use crate::parser::types::{Stmt, Callable};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
struct FuncInner {
    pub name: String,
    pub args: Vec<(String, SourceRef)>,
    pub body: Vec<Stmt>,
    pub name_context: SourceRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    inner: Rc<FuncInner>
}


impl Func {
    pub fn new(name: String, args: Vec<(String, SourceRef)>, body: Vec<Stmt>, name_context: SourceRef) -> Func {
        let inner = FuncInner { name, args, body, name_context };
        Func { inner: Rc::new(inner) }
    }
    pub fn name(&self) -> &str {
        &self.inner.name
    }
}

impl Callable for Func {
    fn arity(&self) -> u8 {
        self.inner.args.len() as u8
    }
    fn call(&self, globals: Env, args: Vec<Literal>, _callsite: SourceRef) -> Result<Literal, RuntimeException> {
        let mut new_env = Env::new(Some(globals.clone()));
        for i in 0..self.inner.args.len() {
            new_env.declare(&self.inner.args[i].0.clone(), &args[i]);
        }
        match interpret(&self.inner.body, new_env, globals) {
            Ok(lit) => Ok(lit),
            Err(except) => match except {
                LoxControlFlow::CFRuntime(rt) => Err(rt),
                LoxControlFlow::CFReturn(val, _context) => Ok(val),
            }
        }
    }
    fn name(&self) -> &str {
        self.name()
    }
}