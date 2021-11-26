use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::{Callable, Source, SourceRef};
use crate::parser::Class;
use crate::runtime::instance::Instance;
use crate::runtime::is_equal;


/// Fundamental assumption is that cloning a value is a cheap pointer copy
#[derive(Clone, Debug)]
pub enum Value {
    STRING(String),
    NUMBER(f64),
    BOOL(bool),
    NIL,
    FUNC(Rc<dyn Callable>),
    INSTANCE(Instance),
    CLASS(Class),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        let source = SourceRef::new(0, 0, 0, Rc::new(Source::new(format!(""))));
        match is_equal(&self, other, &source) {
            Ok(res) => res,
            Err(_) => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::STRING(str) => f.write_str(str),
            Value::NUMBER(num) => f.write_str(&format!("{}", num)),
            Value::BOOL(bool) => f.write_str(if *bool { "true" } else { "false" }),
            Value::NIL => f.write_str("NIL"),
            Value::FUNC(func) => f.write_str(&format!("func<{}>", func.name())),
            Value::INSTANCE(i) => f.write_str(&format!("instance<{}>", i.name())),
            Value::CLASS(c) => f.write_str(&format!("class<{}>", c.name())),
        }
    }
}

impl Value {
    pub fn type_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::BOOL(_), Value::BOOL(_)) => true,
            (Value::STRING(_), Value::STRING(_)) => true,
            (Value::NUMBER(_), Value::NUMBER(_)) => true,
            (Value::NIL, Value::NIL) => true,
            (Value::FUNC(_), Value::FUNC(_)) => true,
            (Value::INSTANCE(_), Value::INSTANCE(_)) => true,
            (Value::CLASS(_), Value::CLASS(_)) => true,
            _ => false,
        }
    }
    pub fn truthy(&self) -> bool {
        match self {
            Value::NUMBER(_) => true,
            Value::NIL => false,
            Value::STRING(_) => true,
            Value::BOOL(b) => *b,
            Value::FUNC(_) => true,
            Value::INSTANCE(_) => true,
            Value::CLASS(_) => true,
        }
    }
    pub fn tname(&self) -> &str {
        match self {
            Value::STRING(_) => "STRING",
            Value::NUMBER(_) => "NUMBER",
            Value::BOOL(_) => "BOOL",
            Value::NIL => "NIL",
            Value::FUNC(_) => "FUNC",
            Value::INSTANCE(_) => "INSTANCE",
            Value::CLASS(_) => "CLASS",
        }
    }
}