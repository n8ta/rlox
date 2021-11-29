use crate::parser::{ExprTy, Expr, UnaryOp, Stmt, LogicalOp};
use crate::parser::BinOp::{EQUAL_EQUAL, BANG_EQUAL, PLUS, SLASH, MINUS, MULT, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, AND, OR};
use crate::source_ref::SourceRef;
use crate::runtime::environment::Env;
use std::rc::Rc;
use crate::runtime::{is_equal};
use std::fmt::{Debug};
use crate::runtime::func::Func;
use crate::runtime::Value;
use crate::Value::NIL;
use crate::Callable;
use crate::LoxControlFlow::CFRuntime;
use crate::resolver::Resolved;

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub struct RuntimeException {
    pub msg: String,
    pub context: SourceRef,
}

impl RuntimeException {
    pub fn new(msg: String, context: SourceRef) -> RuntimeException {
        RuntimeException { msg, context }
    }
}

pub enum LoxControlFlow {
    CFRuntime(RuntimeException),
    CFReturn(Value, SourceRef),
}

impl From<RuntimeException> for LoxControlFlow {
    fn from(rt: RuntimeException) -> Self {
        LoxControlFlow::CFRuntime(rt)
    }
}

type InterpreterResult = Result<Value, LoxControlFlow>;

fn is_num(lit: Value, context: &SourceRef) -> Result<f64, RuntimeException> {
    if let Value::NUMBER(num) = lit {
        Ok(num)
    } else {
        Err(RuntimeException::new(format!("Expected a number found {:?}", lit), context.clone()))
    }
}

pub fn interpret(stmts: &Stmt, env: Env, globals: Env) -> InterpreterResult {
    let mut interp = Interpreter::new(env, globals);
    interp.interpret(stmts)
}

pub struct Interpreter {
    pub env: Env,
    pub globals: Env,
}

impl Into<InterpreterResult> for RuntimeException {
    fn into(self) -> InterpreterResult {
        Err(LoxControlFlow::CFRuntime(self))
    }
}

impl Interpreter {
    fn new(env: Env, globals: Env) -> Interpreter { Interpreter { env, globals } }

    fn interpret(&mut self, stmt: &Stmt) -> InterpreterResult {
        match stmt {
            Stmt::Class(class, _scope_size) => {
                self.env.declare(class.name(), &Value::NIL);
                let class_runtime = Value::CLASS(class.clone());
                let mut rt_methods = class.inner.runtime_methods.borrow_mut();
                for method in class.inner.methods.borrow().iter() {
                    let func = Func::new(method.clone(), self.env.clone(), self.globals.clone());
                    rt_methods.insert(func.name().to_string(), func);
                }
                self.env.assign(class.name(), &class_runtime, &class.context())?;
            }
            Stmt::Block(block_stmts, _scope_size) => {
                let parent = self.env.clone();
                let new_scope = Env::new(Some(parent.clone()));
                self.env = new_scope;
                let mut res: InterpreterResult = Ok(Value::NIL);
                for stmt in block_stmts.iter() {
                    res = Ok(self.interpret(stmt)?);
                }
                self.env = parent;
                res?;
            }
            Stmt::Expr(expr) => {
                self.execute_expr(&expr)?;
            }
            Stmt::Print(val) => {
                println!("{}", self.execute_expr(&val)?);
            }
            Stmt::Variable(name, value) => {
                match value {
                    None => {
                        // println!("Declaring {} as nil", &name);
                        self.env.declare(&name, &NIL);
                    }
                    Some(lit) => {
                        let value = &self.execute_expr(&lit)?;
                        // println!("Declaring {} as {}", &name, value);
                        self.env.declare(&name, value);
                    }
                };
            }
            Stmt::If(test, then_branch, else_branch) => {
                let res = self.execute_expr(&test)?;
                if res.truthy() {
                    self.interpret(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.interpret(else_branch)?;
                }
            }
            Stmt::While(test, body) => {
                while self.execute_expr(&test)?.truthy() {
                    self.interpret(&*body)?;
                }
            }
            Stmt::Function(func) => {
                self.env.declare(func.name().clone(),
                                 &Value::FUNC(Rc::new(
                                     Func::new(func.clone(), self.env.clone(), self.globals.clone())
                                 )));
            }
            Stmt::Return(expr, context) => {
                let val = match expr {
                    None => Value::NIL,
                    Some(expr) => self.execute_expr(expr)?,
                };
                return Err(LoxControlFlow::CFReturn(val, context.clone()));
            }
        }
        Ok(NIL)
    }

    fn err(&self, message: String, context: SourceRef) -> InterpreterResult {
        InterpreterResult::Err(LoxControlFlow::CFRuntime(RuntimeException::new(message, context)))
    }

    fn execute_expr(&mut self, expr: &ExprTy) -> InterpreterResult {
        match &expr.expr {
            Expr::This(resolved) => {
                self.lookup_variable("this", resolved, &expr.context)
            }
            Expr::Binary(left, op, right) => {
                let context = left.context.merge(&right.context);

                let left = self.execute_expr(&left)?;
                let right = self.execute_expr(&right)?;
                Ok(match op {
                    // NUM, STRING, BOOL, NIL
                    EQUAL_EQUAL => Value::BOOL(is_equal(&left, &right, &expr.context)?),
                    BANG_EQUAL => Value::BOOL(!is_equal(&left, &right, &expr.context)?),

                    // NUM
                    LESS => Value::BOOL(is_num(left, &context)? < is_num(right, &context)?),
                    LESS_EQUAL => Value::BOOL(is_num(left, &context)? <= is_num(right, &context)?),
                    SLASH => Value::NUMBER(is_num(left, &context)? / is_num(right, &context)?),
                    MULT => Value::NUMBER(is_num(left, &context)? * is_num(right, &context)?),
                    MINUS => Value::NUMBER(is_num(left, &context)? - is_num(right, &context)?),
                    GREATER => Value::BOOL(is_num(left, &context)? > is_num(right, &context)?),
                    GREATER_EQUAL => Value::BOOL(is_num(left, &context)? >= is_num(right, &context)?),

                    // STRING OR NUM
                    PLUS => {
                        match (&left, &right) {
                            (Value::STRING(linner), Value::STRING(rinner)) => Value::STRING(format!("{}{}", linner, rinner)),
                            (Value::NUMBER(linner), Value::NUMBER(rinner)) => Value::NUMBER(linner + rinner),
                            _ => return self.err(format!("Cannot + {} and {}", left.tname(), right.tname()), expr.context.clone()),
                        }
                    }

                    // BOOL ONLY
                    AND => Value::BOOL(left.truthy() && right.truthy()),
                    OR => Value::BOOL(left.truthy() || right.truthy()),
                })
            }
            Expr::Grouping(inner) => self.execute_expr(&inner),
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Unary(op, inner) => {
                match op {
                    UnaryOp::BANG => {
                        if let Expr::Literal(Value::BOOL(bool_l)) = inner.expr {
                            Ok(Value::BOOL(!bool_l))
                        } else {
                            Err(RuntimeException::new(format!("Cannot apply ! to a non-bool {:?}", inner), expr.context.clone()).into())
                        }
                    }
                    UnaryOp::MINUS => {
                        if let Expr::Literal(Value::NUMBER(num)) = inner.expr {
                            Ok(Value::NUMBER(-num))
                        } else {
                            Err(RuntimeException::new(format!("Cannot apply - to a non-number {:?}", inner), expr.context.clone()).into())
                        }
                    }
                }
            }
            Expr::Variable(var, resolved) => {
                if let Some(resolution) = resolved {
                    // println!("Getting {} from locals", var);
                    // println!("/{} at dist {}", var, distance);
                    self.env.get_at(resolution.scope, &var, &expr.context).or_else(|r| r.into())
                } else {
                    // println!("Getting {} from globals", var);
                    self.globals.fetch(&var, &expr.context).or_else(|r| r.into())
                }
            }
            Expr::Assign(var, new_val, _resolved) => {
                let value = self.execute_expr(&new_val)?;
                // println!("Assigning {} as {}", var, value);
                self.env.assign(&var, &value, &expr.context)?;
                Ok(Value::NIL)
            }
            Expr::Logical(left, op, right) => {
                let left = self.execute_expr(&left)?;
                let res = if let LogicalOp::OR = op {
                    if left.truthy() { left } else { self.execute_expr(&right)? }
                } else {
                    if !left.truthy() { left } else { self.execute_expr(&right)? }
                };
                Ok(res)
            }
            Expr::Call(callee, args) => {
                let func = self.execute_expr(callee)?;
                let mut evaluated_args = vec![];
                for arg in args.iter() {
                    evaluated_args.push(self.execute_expr(arg)?)
                }
                if let Value::FUNC(func) = func {
                    Ok(func.call(evaluated_args, callee.context.clone())?)
                } else if let Value::CLASS(class) = func {
                    Ok(class.call(evaluated_args, callee.context.clone())?)
                } else {
                    Err(RuntimeException::new(format!("Cannot call a {}", func.tname()), callee.context.clone()).into())
                }
            }
            Expr::Get(expr, getter_name) => {
                let obj = self.execute_expr(expr)?;
                if let Value::INSTANCE(inst) = obj {
                    inst.get(getter_name, &expr.context).map_err(|e| e.into())
                } else {
                    Err(RuntimeException::new(format!("Cannot use the class.get_property syntax on a non class instance"), expr.context.clone()).into())
                }
            }
            Expr::Set(left, field, right) => {
                let lhs = self.execute_expr(left)?;
                if let Value::INSTANCE(mut inst) = lhs {
                    let rhs = self.execute_expr(right)?;
                    inst.set(field, rhs.clone());
                    Ok(rhs)
                } else {
                    Err(LoxControlFlow::CFRuntime(RuntimeException::new(
                        format!("Cannot assign field {} to a {}", field, lhs.tname()),
                        left.context.merge(&right.context))))
                }
            }
        }
    }

    fn lookup_variable(&self, name: &str, scope: &Option<Resolved>, context: &SourceRef) -> InterpreterResult {
        let res = if let Some(resolved) = scope {
            self.env.get_at(resolved.scope, name, context)
        } else {
            self.globals.fetch(name, context).into()
        };
        res.or_else(|e| InterpreterResult::Err(CFRuntime(e)))
    }
}