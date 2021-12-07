use std::collections::HashMap;
use crate::parser::{ExprTy, Expr, UnaryOp, Stmt, LogicalOp};
use crate::parser::BinOp::{EQUAL_EQUAL, BANG_EQUAL, PLUS, SLASH, MINUS, MULT, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, AND, OR};
use crate::source_ref::SourceRef;
use crate::runtime::fast_env::FastEnv;
use std::rc::Rc;
use crate::runtime::{is_equal};
use std::fmt::{Debug};
use crate::runtime::func::Func;
use crate::runtime::Value;
use crate::Value::NIL;
use crate::Callable;
use crate::LoxControlFlow::CFRuntime;
use crate::resolver::{Resolved};
use crate::runtime::class::RtClass;

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub struct RuntimeException {
    pub msg: String,
    pub context: SourceRef,
    pub context2: Option<SourceRef>,
}

impl RuntimeException {
    pub fn new(msg: String, context: SourceRef) -> RuntimeException {
        RuntimeException { msg, context, context2: None }
    }
    pub fn new2(msg: String, context: SourceRef, context2: SourceRef) -> RuntimeException {
        RuntimeException { msg, context, context2: Some(context2) }
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
        Err(RuntimeException::new(format!("Expected a number found {}", lit.tname()), context.clone()))
    }
}

pub fn interpret(stmts: &Stmt, env: FastEnv, globals: FastEnv) -> InterpreterResult {
    let mut interp = Interpreter::new(env, globals);
    interp.interpret(stmts)
}

pub struct Interpreter {
    pub env: FastEnv,
    pub globals: FastEnv,
}

impl Into<InterpreterResult> for RuntimeException {
    fn into(self) -> InterpreterResult {
        Err(LoxControlFlow::CFRuntime(self))
    }
}

impl Interpreter {
    #[allow(dead_code)]
    fn print_envs(&self) {
        let mut i = 0;
        let mut curr: Option<FastEnv> = Some(self.env.clone());
        while let Some(env) = curr {
            println!("[{}] {}", i, env.len());
            curr = env.parent();
            i += 1;
        }
    }

    fn new(env: FastEnv, globals: FastEnv) -> Interpreter { Interpreter { env, globals } }

    fn interpret(&mut self, stmt: &Stmt) -> InterpreterResult {
        match stmt {
            Stmt::Class(class, resolution, _scope_size) => {
                let resolution = resolution.as_ref().unwrap();
                self.env.declare(resolution.offset, &class.name().string, &Value::NIL);

                // Resolve super class
                let super_class =
                    if let Some(super_class) = &class.inner.super_class {
                        let super_class = super_class.borrow();
                        if let Expr::Variable(_) = &super_class.parent.expr {} else {
                            panic!("Compiler bug, super class must be a variable expression");
                        }
                        let super_class_value = self.execute_expr(&super_class.parent)?;
                        if let Value::CLASS(rtclass) = super_class_value {
                            Some(rtclass.clone())
                        } else {
                            return Err(LoxControlFlow::CFRuntime(RuntimeException::new(
                                format!("Class '{}' had a super class '{}' that resolved to be a {} instead of another class",
                                        class.name().string,
                                        super_class.name,
                                        super_class_value.tname()),
                                class.context(),
                            )));
                        }
                    } else {
                        None
                    };


                // Create runtime methods that are bound to the current local scope
                // If there is a super class create a env with super defined, else just point to parent env
                // resolver does the same.
                let mut methods = HashMap::new();
                let method_env = if let Some(super_class) = super_class.clone() {
                    let mut env_w_super = FastEnv::new(Some(self.env.clone()), 1);
                    env_w_super.declare(0, "super", &Value::CLASS(super_class));
                    env_w_super
                } else {
                    self.env.clone()
                };

                for method in class.inner.methods.borrow().iter() {
                    let func = Func::new(method.clone(), method_env.clone(), self.globals.clone());
                    methods.insert(func.name().string.to_string(), func);
                }

                let class_runtime = Value::CLASS(RtClass::new(class.clone(), methods, super_class));
                self.env.assign(&class.name().string, resolution.scope, resolution.offset, &class_runtime, &class.context())?;
            }
            Stmt::Block(block_stmts, scope_size) => {
                let parent = self.env.clone();
                let new_env = FastEnv::new(Some(parent.clone()), scope_size.unwrap());
                self.env = new_env.clone();

                for stmt in block_stmts.iter() {
                    self.interpret(stmt)?;
                }
                self.env = parent;
            }
            Stmt::Expr(expr) => {
                self.execute_expr(&expr)?;
            }
            Stmt::Print(val) => {
                println!("{}", self.execute_expr(&val)?);
            }
            Stmt::Variable(name, value, resolved, _context) => {
                match value {
                    None => {
                        self.env.declare(resolved.as_ref().unwrap().offset, &name.string, &NIL);
                    }
                    Some(lit) => {
                        let value = &self.execute_expr(&lit)?;
                        // println!("Declaring {} as {}", &name, value);
                        self.env.declare(resolved.as_ref().unwrap().offset, &name.string, value);
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
            Stmt::While(test, body, _scope_size) => {
                while self.execute_expr(&test)?.truthy() {
                    self.interpret(body)?;
                }
            }
            Stmt::Function(func, resolved) => {
                self.env.declare(resolved.as_ref().unwrap().offset, &func.name().string,
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
            Expr::Super(method, resolved) => {
                let scope = resolved.as_ref().unwrap().scope;
                let offset = resolved.as_ref().unwrap().offset;
                // super class value from env
                let super_class = self.env.fetch("super", scope, offset, &method.context).or_else(|r| r.into())?;
                // unwrap into actual Value::CLASS
                let super_class = if let Value::CLASS(super_class) = super_class {
                    super_class
                } else {
                    return Err(LoxControlFlow::CFRuntime(RuntimeException::new(format!("'super' resolved into something that was not a class"), expr.context.clone())));
                };
                let this = self.env.fetch("this", scope - 1, 0, &method.context)?;
                let this = if let Value::INSTANCE(inst) = this {
                    inst
                } else {
                    panic!("Compiler bug: tried to look up this in lexical scope 1 below super but found a {} instead of an instance", this.tname())
                };

                let method = super_class.find_method(&method.string, &method.context)?;
                Ok(Value::FUNC(Rc::new(method.bind(&this))))
            }
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
                            Err(RuntimeException::new(format!("Cannot apply ! to a non-bool"), expr.context.clone()).into())
                        }
                    }
                    UnaryOp::MINUS => {
                        if let Expr::Literal(Value::NUMBER(num)) = inner.expr {
                            Ok(Value::NUMBER(-num))
                        } else {
                            Err(RuntimeException::new(format!("Cannot apply - to a non-number"), expr.context.clone()).into())
                        }
                    }
                }
            }
            Expr::Variable(var) => {
                if let Some(resolution) = &var.resolved {
                    self.env.fetch(&var.name.string, resolution.scope, resolution.offset, &expr.context).or_else(|r| r.into())
                } else {
                    self.globals.fetch("clock", 0, 0, &expr.context).or_else(|r| r.into())
                }
            }
            Expr::Assign(var, new_val, resolved) => {
                let resolved = resolved.as_ref().unwrap();
                let value = self.execute_expr(&new_val)?;
                self.env.assign(&var.string, resolved.scope, resolved.offset, &value, &expr.context)?;
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
                    Ok(func.call(evaluated_args, &callee.context)?)
                } else if let Value::CLASS(class) = func {
                    Ok(class.call(evaluated_args, &callee.context)?)
                } else {
                    Err(RuntimeException::new(format!("Cannot call a {}", func.tname()), callee.context.clone()).into())
                }
            }
            Expr::Get(get_expr, field) => {
                let obj = self.execute_expr(get_expr)?;
                if let Value::INSTANCE(inst) = obj {
                    inst.get(&field.string, &expr.context).map_err(|e| e.into())
                } else {
                    Err(RuntimeException::new(
                        format!("Cannot use the class.get_property syntax on a '{}' only on a class instance", obj.tname()),
                        expr.context.clone()).into())
                }
            }
            Expr::Set(left, field, right) => {
                let lhs = self.execute_expr(left)?;
                if let Value::INSTANCE(mut inst) = lhs {
                    let rhs = self.execute_expr(right)?;
                    inst.set(&field.string, rhs.clone());
                    Ok(rhs)
                } else {
                    Err(LoxControlFlow::CFRuntime(RuntimeException::new(
                        format!("Cannot assign field {} to a {}", field.string, lhs.tname()),
                        left.context.merge(&right.context))))
                }
            }
        }
    }

    fn lookup_variable(&self, name: &str, scope: &Option<Resolved>, context: &SourceRef) -> InterpreterResult {
        let res = if let Some(resolved) = scope {
            self.env.fetch(name, resolved.scope, resolved.offset, context)
        } else {
            self.globals.fetch(name, 0, 0, context).into()
        };
        res.or_else(|e| InterpreterResult::Err(CFRuntime(e)))
    }
}