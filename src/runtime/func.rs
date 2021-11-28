use std::rc::Rc;
use crate::{Callable, SourceRef};
use crate::parser::ParserFunc;
use crate::runtime::environment::Env;
use crate::runtime::Instance;
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
    pub fn bind(self, inst: &Instance) -> crate::runtime::func::Func {
        let mut env = Env::new(Some(self.inner.env.clone()));
        env.declare("this", &Value::INSTANCE(inst.clone()));
        Func::new(self.inner.func.clone(), env, self.inner.globals.clone())
    }
    pub fn func(&self) -> ParserFunc {
        self.inner.func.clone()
    }
    pub fn env(&self) -> Env {
        self.inner.env.clone()
    }
    pub fn globals(&self) -> Env {
        self.inner.globals.clone()
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
        self.inner.func.inner.args.len() as u8
    }
    fn call(&self, args: Vec<Value>, _callsite: SourceRef) -> Result<Value, RuntimeException> {
        let mut new_env = Env::new(Some(self.inner.env.clone()));
        for i in 0..self.inner.func.inner.args.len() {
            new_env.declare(&self.inner.func.inner.args[i].0.clone(), &args[i]);
        }
        match interpret(&self.inner.func.inner.body.borrow(), new_env, self.inner.globals.clone()) {
            Ok(lit) => Ok(lit),
            Err(except) => match except {
                LoxControlFlow::CFRuntime(rt) => Err(rt),
                LoxControlFlow::CFReturn(val, _context) => Ok(val),
            }
        }
    }
    fn name(&self) -> &str {
        &self.inner.func.inner.name
    }
}