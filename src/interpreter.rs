use crate::parser::{ExprTy, Expr, UnaryOp, Stmt};
use crate::scanner::{Literal};
use crate::parser::BinOp::{EQUAL_EQUAL, BANG_EQUAL, PLUS, SLASH, MINUS, MULT, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, AND, OR};
use crate::source_ref::SourceRef;
use crate::environment::Env;
use crate::scanner::Literal::NIL;
use std::rc::Rc;
use std::cell::RefCell;


#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub struct RuntimeException {
    // token: TokenInContext,
    pub msg: String,
    pub context: SourceRef,
}

impl RuntimeException {
    pub fn new(msg: String, context: SourceRef) -> RuntimeException {
        RuntimeException { msg, context }
    }
}

fn is_num(lit: Literal, context: &SourceRef) -> Result<f64, RuntimeException> {
    if let Literal::NUMBER(num) = lit {
        Ok(num)
    } else {
        Err(RuntimeException::new(format!("Expected a number found {:?}", lit), context.clone()))
    }
}

fn is_equal(left: &Literal, right: &Literal, context: SourceRef) -> Result<bool, RuntimeException> {
    match (left, right) {
        (Literal::STRING(left), Literal::STRING(right)) => Ok(left == right),
        (Literal::NUMBER(left), Literal::NUMBER(right)) => Ok(left == right),
        (Literal::BOOL(left), Literal::BOOL(right)) => Ok(left == right),
        (Literal::NIL, Literal::NIL) => Ok(true),
        _ => Err(RuntimeException::new(format!("Cannot compare {} and {}", left.tname(), right.tname()), context))
    }
}


struct Interpreter {
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    fn new(env: Rc<RefCell<Env>>) -> Interpreter { Interpreter { env } }

    fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<Literal, RuntimeException> {
        let mut last = Literal::NIL;
        for stmt in stmts {
            last = match stmt {
                Stmt::Block(block_stmts) => {
                    let parent = self.env.clone();
                    let new_scope = Rc::new(RefCell::new(Env::new(Some(parent.clone()))));
                    self.env = new_scope;
                    let res = self.interpret(block_stmts);
                    self.env = parent;
                    res?
                }
                Stmt::Expr(expr) => self.execute_expr(expr)?,
                Stmt::Print(val) => {
                    println!("{}", self.execute_expr(val)?);
                    NIL
                }
                Stmt::Variable(name, value) => {
                    match value {
                        // todo: fix source ref
                        None => {
                            let mut env = self.env.borrow_mut();
                            env.declare(&name, &NIL);
                        }
                        Some(lit) => {
                            let value = &self.execute_expr(lit)?;
                            self.env.clone().borrow_mut().declare(&name, value);
                        }
                    };
                    NIL
                }
                Stmt::If(test, then_branch, else_branch) => {
                    let res = self.execute_expr(test)?;
                    if res.truthy() {
                        self.interpret(vec![*then_branch])?
                    } else if let Some(else_branch) = else_branch {
                        self.interpret(vec![*else_branch])?
                    } else {
                        NIL
                    }
                }
            }
        }
        Ok(last)
    }

    fn execute_expr(&mut self, expr: ExprTy) -> Result<Literal, RuntimeException> {
        match expr.expr {
            Expr::Binary(left, op, right) => {
                let context = left.context.merge(&right.context);

                let left = self.execute_expr(left)?;
                let right = self.execute_expr(right)?;
                Ok(match op {
                    // NUM, STRING, BOOL, NIL
                    EQUAL_EQUAL => Literal::BOOL(is_equal(&left, &right, expr.context)?),
                    BANG_EQUAL => Literal::BOOL(!is_equal(&left, &right, expr.context)?),

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
                            _ => return Err(RuntimeException::new(format!("Cannot + {} and {}", left.tname(), right.tname()), expr.context)),
                        }
                    }

                    // BOOL ONLY
                    AND => Literal::BOOL(left.truthy() && right.truthy()),
                    OR => Literal::BOOL(left.truthy() || right.truthy()),
                })
            }
            Expr::Grouping(inner) => self.execute_expr(inner),
            Expr::Literal(lit) => Ok(lit),
            Expr::Unary(op, inner) => {
                match op {
                    UnaryOp::BANG => {
                        if let Expr::Literal(Literal::BOOL(bool_l)) = inner.expr {
                            Ok(Literal::BOOL(!bool_l))
                        } else {
                            Err(RuntimeException::new(format!("Cannot apply ! to a non-bool {:?}", inner), expr.context))
                        }
                    }
                    UnaryOp::MINUS => {
                        if let Expr::Literal(Literal::NUMBER(num)) = inner.expr {
                            Ok(Literal::NUMBER(-num))
                        } else {
                            Err(RuntimeException::new(format!("Cannot apply - to a non-number {:?}", inner), expr.context))
                        }
                    }
                }
            }
            Expr::Variable(var) => {
                self.env.borrow_mut().fetch(&var, &expr.context)
            }
            Expr::Assign(var, new_val) => {
                let value = self.execute_expr(new_val)?;
                self.env.borrow_mut().assign(&var, &value, &expr.context)?;
                Ok(Literal::NIL)
            }
        }
    }
}

pub fn interpret(stmts: Vec<Stmt>, env: Rc<RefCell<Env>>) -> Result<Literal, RuntimeException> {
    let mut interp = Interpreter::new(env);
    interp.interpret(stmts)
}