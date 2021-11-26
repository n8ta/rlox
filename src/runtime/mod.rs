pub use crate::runtime::interpreter::RuntimeException;
pub use crate::runtime::value::Value;
pub use crate::runtime::instance::Instance;
pub use crate::source_ref::SourceRef;

pub(crate) mod environment;
pub(crate) mod interpreter;
pub(crate) mod value;
pub(crate) mod func;
pub(crate) mod instance;

pub trait Callable {
    fn arity(&self) -> u8;
    fn call(&self, args: Vec<Value>, callsite: SourceRef) -> Result<Value, RuntimeException>;
    fn name(&self) -> &str;
}

pub fn is_equal(left: &Value, right: &Value, context: &SourceRef) -> Result<bool, RuntimeException> {
    match (left, right) {
        (Value::STRING(left), Value::STRING(right)) => Ok(left == right),
        (Value::NUMBER(left), Value::NUMBER(right)) => Ok(left == right),
        (Value::BOOL(left), Value::BOOL(right)) => Ok(left == right),
        (Value::NIL, Value::NIL) => Ok(true),
        _ => Err(RuntimeException::new(format!("Cannot compare {} and {}", left, right), context.clone()))
    }
}