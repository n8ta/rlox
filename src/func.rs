use crate::interpreter::{LoxControlFlow, interpret, RuntimeException};
use crate::source_ref::SourceRef;
use crate::scanner::Literal;
use crate::environment::Env;
use crate::parser::types::{Stmt, Callable};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
/// Parser Representation of a Func
pub struct ParserFunc {
    pub name: String,
    pub args: Vec<(String, SourceRef)>,
    pub body: Vec<Stmt>,
    pub name_context: SourceRef,
}

impl ParserFunc {
    pub fn new(name: String, args: Vec<(String, SourceRef)>, body: Vec<Stmt>, name_context: SourceRef) -> ParserFunc {
        ParserFunc { name, args, body, name_context }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// Runtime Representation of a Func
#[derive(Clone, Debug)]
pub struct Func {
    inner: Rc<FuncInner>
}

impl Func {
    pub(crate) fn new(func: ParserFunc, env: Env, globals: Env) -> Func {
        Func { inner:  Rc::new(FuncInner { func, env, globals }) }
    }
}

#[derive(Clone, Debug)]
struct FuncInner {
    func: ParserFunc,
    env: Env,
    globals: Env,
}

impl Callable for Func {
    fn arity(&self) -> u8 {
        self.inner.func.args.len() as u8
    }
    fn call(&self, args: Vec<Literal>, _callsite: SourceRef) -> Result<Literal, RuntimeException> {
        let mut new_env = Env::new(Some(self.inner.env.clone()));
        for i in 0..self.inner.func.args.len() {
            new_env.declare(&self.inner.func.args[i].0.clone(), &args[i]);
        }
        match interpret(&self.inner.func.body, new_env, self.inner.globals.clone()) {
            Ok(lit) => Ok(lit),
            Err(except) => match except {
                LoxControlFlow::CFRuntime(rt) => Err(rt),
                LoxControlFlow::CFReturn(val, _context) => Ok(val),
            }
        }
    }
    fn name(&self) -> &str {
        &self.inner.func.name
    }
}