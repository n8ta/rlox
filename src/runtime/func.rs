use std::rc::Rc;
use crate::{Callable, SourceRef};
use crate::parser::ParserFunc;
use crate::runtime::environment::Env;
use crate::runtime::interpreter::{interpret, LoxControlFlow, RuntimeException};
use crate::runtime::value::Value;

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
    fn call(&self, args: Vec<Value>, _callsite: SourceRef) -> Result<Value, RuntimeException> {
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