use crate::parser::types::{ExprTy, Expr, UnaryOp, Stmt, LogicalOp};
use crate::scanner::{Literal};
use crate::parser::types::BinOp::{EQUAL_EQUAL, BANG_EQUAL, PLUS, SLASH, MINUS, MULT, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, AND, OR};
use crate::source_ref::SourceRef;
use crate::environment::Env;
use crate::scanner::Literal::{NIL};
use std::rc::Rc;
use crate::func::Func;
use std::fmt::{Debug};

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
    CFReturn(Literal, SourceRef)
}

impl From<RuntimeException> for LoxControlFlow {
    fn from(rt: RuntimeException) -> Self {
        LoxControlFlow::CFRuntime(rt)
    }
}

type InterpreterResult = Result<Literal, LoxControlFlow>;

fn is_num(lit: Literal, context: &SourceRef) -> Result<f64, RuntimeException> {
    if let Literal::NUMBER(num) = lit {
        Ok(num)
    } else {
        Err(RuntimeException::new(format!("Expected a number found {:?}", lit), context.clone()))
    }
}

pub fn is_equal(left: &Literal, right: &Literal, context: &SourceRef) -> Result<bool, RuntimeException> {
    match (left, right) {
        (Literal::STRING(left), Literal::STRING(right)) => Ok(left == right),
        (Literal::NUMBER(left), Literal::NUMBER(right)) => Ok(left == right),
        (Literal::BOOL(left), Literal::BOOL(right)) => Ok(left == right),
        (Literal::NIL, Literal::NIL) => Ok(true),
        _ => Err(RuntimeException::new(format!("Cannot compare {} and {}", left.tname(), right.tname()), context.clone()))
    }
}

pub fn interpret(stmts: &Vec<Stmt>, env: Env, globals: Env) -> InterpreterResult {
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

    fn interpret(&mut self, stmts: &Vec<Stmt>) -> InterpreterResult {
        for stmt in stmts.iter() {
            match stmt {
                Stmt::Block(block_stmts) => {
                    let parent = self.env.clone();
                    let new_scope = Env::new(Some(parent.clone()));
                    self.env = new_scope;
                    let res = self.interpret(block_stmts);
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
                            self.env.declare(&name, &NIL);
                        }
                        Some(lit) => {
                            let value = &self.execute_expr(&lit)?;
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
                        let b = *body.clone();
                        self.interpret(&vec![b])?;
                    }
                }
                Stmt::Function(func) => {
                    self.env.declare(func.name().clone(),
                                     &Literal::FUNC(Rc::new(
                                         Func::new(func.clone(), self.env.clone(), self.globals.clone())
                                     )));
                }
                Stmt::Return(expr, context) => {
                    let val = match expr {
                        None => Literal::NIL,
                        Some(expr) => self.execute_expr(expr)?,
                    };
                    return Err(LoxControlFlow::CFReturn(val, context.clone()))
                }
            }
        }
        Ok(NIL)
    }

    fn err(&self, message: String, context: SourceRef) -> InterpreterResult {
        InterpreterResult::Err(LoxControlFlow::CFRuntime(RuntimeException::new(message, context)))
    }

    fn execute_expr(&mut self, expr: &ExprTy) -> InterpreterResult {
        match &expr.expr {
            Expr::Binary(left, op, right) => {
                let context = left.context.merge(&right.context);

                let left = self.execute_expr(&left)?;
                let right = self.execute_expr(&right)?;
                Ok(match op {
                    // NUM, STRING, BOOL, NIL
                    EQUAL_EQUAL => Literal::BOOL(is_equal(&left, &right, &expr.context)?),
                    BANG_EQUAL => Literal::BOOL(!is_equal(&left, &right, &expr.context)?),

                    // NUM
                    LESS => Literal::BOOL(is_num(left, &context)? < is_num(right, &context)?),
                    LESS_EQUAL => Literal::BOOL(is_num(left, &context)? <= is_num(right, &context)?),
                    SLASH => Literal::NUMBER(is_num(left, &context)? / is_num(right, &context)?),
                    MULT => Literal::NUMBER(is_num(left, &context)? * is_num(right, &context)?),
                    MINUS => Literal::NUMBER(is_num(left, &context)? - is_num(right, &context)?),
                    GREATER => Literal::BOOL(is_num(left, &context)? > is_num(right, &context)?),
                    GREATER_EQUAL => Literal::BOOL(is_num(left, &context)? >= is_num(right, &context)?),

                    // STRING OR NUM
                    PLUS => {
                        match (&left, &right) {
                            (Literal::STRING(linner), Literal::STRING(rinner)) => Literal::STRING(format!("{}{}", linner, rinner)),
                            (Literal::NUMBER(linner), Literal::NUMBER(rinner)) => Literal::NUMBER(linner + rinner),
                            _ => return self.err(format!("Cannot + {} and {}", left.tname(), right.tname()), expr.context.clone()),
                        }
                    }

                    // BOOL ONLY
                    AND => Literal::BOOL(left.truthy() && right.truthy()),
                    OR => Literal::BOOL(left.truthy() || right.truthy()),
                })
            }
            Expr::Grouping(inner) => self.execute_expr(&inner),
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Unary(op, inner) => {
                match op {
                    UnaryOp::BANG => {
                        if let Expr::Literal(Literal::BOOL(bool_l)) = inner.expr {
                            Ok(Literal::BOOL(!bool_l))
                        } else {
                            Err(RuntimeException::new(format!("Cannot apply ! to a non-bool {:?}", inner), expr.context.clone()).into())
                        }
                    }
                    UnaryOp::MINUS => {
                        if let Expr::Literal(Literal::NUMBER(num)) = inner.expr {
                            Ok(Literal::NUMBER(-num))
                        } else {
                            Err(RuntimeException::new(format!("Cannot apply - to a non-number {:?}", inner), expr.context.clone()).into())
                        }
                    }
                }
            }
            Expr::Variable(var) => {
                if let Some(distance) = expr.scope {
                    self.env.get_at(distance, &var, &expr.context).or_else(|r| r.into())
                } else {
                    self.globals.fetch( &var, &expr.context).or_else(|r| r.into())
                }
            }
            Expr::Assign(var, new_val) => {
                let value = self.execute_expr(&new_val)?;
                self.env.assign(&var, &value, &expr.context)?;
                Ok(Literal::NIL)
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
                if let Literal::FUNC(func) = func {
                    Ok(func.call(evaluated_args, callee.context.clone())?)
                } else {
                    Err(RuntimeException::new(format!("Cannot call a {:?}", func), callee.context.clone()).into())
                }
            }
        }
    }

    fn lookup_variable(&self, name: &str, expr: ExprTy) -> Result<Literal, RuntimeException> {
        if let Some(dist) = expr.scope {
            self.env.get_at(dist, name, &expr.context)
        } else {
            self.globals.fetch(name, &expr.context)
        }
    }
}