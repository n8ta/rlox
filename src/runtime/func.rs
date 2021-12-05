use std::rc::Rc;
use crate::{Callable, SourceRef, StringInContext};
use crate::parser::ParserFunc;
use crate::runtime::fast_env::FastEnv;
use crate::runtime::Instance;
use crate::runtime::interpreter::{interpret, LoxControlFlow, RuntimeException};
use crate::runtime::value::Value;

/// Runtime Representation of a Func
#[derive(Clone, Debug, serde::Serialize)]
pub struct Func {
    inner: Rc<FuncInner>,
}


impl Func {
    pub(crate) fn new(func: ParserFunc, env: FastEnv, globals: FastEnv) -> Func {
        Func { inner: Rc::new(FuncInner { func, env, globals }) }
    }
    pub fn bind(self, inst: &Instance) -> crate::runtime::func::Func {
        let mut env = FastEnv::new(Some(self.inner.env.clone()), 1);
        env.declare(0, "this", &Value::INSTANCE(inst.clone()));
        Func::new(self.inner.func.clone(), env, self.inner.globals.clone())
    }
}

#[derive(Clone, Debug, serde::Serialize)]
struct FuncInner {
    func: ParserFunc,
    env: FastEnv,
    globals: FastEnv,
}

impl Callable for Func {
    fn arity(&self) -> u8 {
        self.inner.func.inner.args.len() as u8
    }
    fn call(&self, args: Vec<Value>, _callsite: SourceRef) -> Result<Value, RuntimeException> {
        let size = self.inner.func.inner.scope_size.borrow().unwrap();
        let mut new_env = FastEnv::new(Some(self.inner.env.clone()), size);
        for i in 0..self.inner.func.inner.args.len() {
            new_env.declare(i, &self.inner.func.inner.args[0].0.string, &args[i]);
        }
        match interpret(&self.inner.func.inner.body.borrow(), new_env, self.inner.globals.clone()) {
            Ok(lit) => Ok(lit),
            Err(except) => match except {
                LoxControlFlow::CFRuntime(rt) => Err(rt),
                LoxControlFlow::CFReturn(val, _context) => Ok(val),
            }
        }
    }
    fn name(&self) -> &StringInContext {
        &self.inner.func.inner.name
    }
}