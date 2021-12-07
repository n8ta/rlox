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
    pub fn new(func: ParserFunc, env: FastEnv, globals: FastEnv) -> Func {
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
    fn call(&self, args: Vec<Value>, callsite: &SourceRef) -> Result<Value, RuntimeException> {
        let expected_args = self.inner.func.inner.args.len();
        if args.len() != expected_args {
            return Err(RuntimeException::new2(
                format!("Function {} has {} args but you passed {}", self.name().string, expected_args, args.len()),
                callsite.clone(),
                self.inner.func.inner.context.clone()));
        }
        let size = self.inner.func.inner.scope_size.borrow().unwrap();
        let mut new_env = FastEnv::new(Some(self.inner.env.clone()), size);
        for (i, (arg, src)) in self.inner.func.inner.args.iter().enumerate() {
            new_env.declare(i, &arg.string, &args[i]);
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