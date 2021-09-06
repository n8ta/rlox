use crate::parser::{ExprTy, Expr, UnaryOp, Stmt, Decl};
use crate::scanner::{Literal};
use crate::parser::BinOp::{EQUAL_EQUAL, BANG_EQUAL, PLUS, SLASH, MINUS, MULT, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, AND, OR};
use crate::source_ref::SourceRef;
use crate::environment::Env;


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

fn is_num(lit: Literal) -> Result<f64, RuntimeException> {
    if let Literal::NUMBER(num) = lit {
        Ok(num)
    } else {
        Err(RuntimeException::new(format!("Expected a number found {:?}", lit), SourceRef::new(0, 0, 0)))
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


struct Interpreter<'a> {
    env: &'a mut Env,
}

impl<'a> Interpreter<'a> {

    fn new(env: &'a mut Env) -> Interpreter { Interpreter { env } }

    fn interpret(&mut self, decls: Vec<Decl>) -> Result<Literal, RuntimeException> {
        let mut last = Literal::NIL;
        for decl in decls {
            last = self.execute_decl(decl)?
        }
        Ok(last)
    }

    fn execute_decl(&mut self, decl: Decl) -> Result<Literal, RuntimeException> {
        match decl {
            Decl::VarDecl(ident, value) => {
                if let Some(expr) = value {
                    let exp = self.execute_expr(expr)?;
                    self.env.declare(&ident, &exp);
                } else {
                    self.env.declare(&ident, &Literal::NIL);
                }
                Ok(Literal::NIL)

            },
            Decl::Stmt(stmt) => {
                match stmt {
                    Stmt::Expr(expr) => self.execute_expr(expr),
                    Stmt::Print(expr) => {
                        let res = self.execute_expr(expr)?;
                        println!("{}", res);
                        Ok(res)
                    }
                }
            }
        }
    }

    fn execute_expr(&mut self, expr: ExprTy) -> Result<Literal, RuntimeException> {
        match expr.expr {
            Expr::Binary(left, op, right) => {
                let left = self.execute_expr(left)?;
                let right = self.execute_expr(right)?;
                Ok(match op {
                    // NUM, STRING, BOOL, NIL
                    EQUAL_EQUAL => Literal::BOOL(is_equal(&left, &right, expr.context)?),
                    BANG_EQUAL => Literal::BOOL(!is_equal(&left, &right, expr.context)?),

                    // NUM
                    LESS => Literal::BOOL(is_num(left)? < is_num(right)?),
                    LESS_EQUAL => Literal::BOOL(is_num(left)? <= is_num(right)?),
                    SLASH => Literal::NUMBER(is_num(left)? / is_num(right)?),
                    MULT => Literal::NUMBER(is_num(left)? * is_num(right)?),
                    MINUS => Literal::NUMBER(is_num(left)? - is_num(right)?),
                    GREATER => Literal::BOOL(is_num(left)? > is_num(right)?),
                    GREATER_EQUAL => Literal::BOOL(is_num(left)? >= is_num(right)?),

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
            Expr::Variable(var) => self.env.get(&var, &expr.context),
            Expr::Assign(var, new_val) => {
                let value = self.execute_expr(new_val)?;
                self.env.assign(&var, &value, &expr.context);
                Ok(Literal::NIL)
            }
        }
    }
}

pub fn interpret(decls: Vec<Decl>, env: &mut Env) -> Result<Literal, RuntimeException> {
    let mut interp = Interpreter::new(env);
    interp.interpret(decls)
}