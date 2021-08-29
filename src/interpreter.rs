use crate::parser::{ExprTy, Expr, UnaryOp};
use crate::scanner::{Literal};
use crate::parser::BinOp::{EQUAL_EQUAL, BANG_EQUAL, PLUS, SLASH, MINUS, MULT, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, AND, OR};


#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub struct RuntimeException {
    // token: TokenInContext,
    msg: String,
}

impl RuntimeException {
    fn new(msg: String) -> RuntimeException {
        RuntimeException { msg }
    }
}

fn is_num(lit: Literal) -> Result<f64, RuntimeException> {
    if let Literal::NUMBER(num) = lit {
        Ok(num)
    } else {
        Err(RuntimeException::new(format!("Expected a number found {:?}", lit)))
    }
}

fn is_equal(left: &Literal, right: &Literal) -> Result<bool, RuntimeException> {
    match (left, right) {
        (Literal::STRING(left), Literal::STRING(right)) => Ok(left == right),
        (Literal::NUMBER(left), Literal::NUMBER(right)) => Ok(left == right),
        (Literal::BOOL(left), Literal::BOOL(right)) => Ok(left == right),
        (Literal::NIL, Literal::NIL) => Ok(true),
        _ => Err(RuntimeException::new(format!("Cannot compare {} and {}", left.tname(), right.tname())))
    }
}

pub fn interpret(expr: ExprTy) -> Result<Literal, RuntimeException> {
    match *expr {
        Expr::Binary(left, op, right) => {
            let left = interpret(left)?;
            let right = interpret(right)?;
            Ok(match op {
                // NUM, STRING, BOOL, NIL
                EQUAL_EQUAL => Literal::BOOL(is_equal(&left, &right)?),
                BANG_EQUAL => Literal::BOOL(!is_equal(&left, &right)?),

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
                        _ => return Err(RuntimeException::new(format!("Cannot + {} and {}", left.tname(), right.tname()))),
                    }
                }

                // BOOL ONLY
                AND => Literal::BOOL(left.truthy() && right.truthy()),
                OR => Literal::BOOL(left.truthy() || right.truthy()),
            })
        }
        Expr::Grouping(inner) => interpret(inner),
        Expr::Literal(lit) => Ok(lit),
        Expr::Unary(op, inner) => {
            match op {
                UnaryOp::BANG => {
                    if let Expr::Literal(Literal::BOOL(bool_l)) = *inner {
                        Ok(Literal::BOOL(!bool_l))
                    } else {
                        Err(RuntimeException::new(format!("Cannot apply ! to a non-bool {:?}", inner)))
                    }
                }
                UnaryOp::MINUS => {
                    if let Expr::Literal(Literal::NUMBER(num)) = *inner {
                        Ok(Literal::NUMBER(-num))
                    } else {
                        Err(RuntimeException::new(format!("Cannot apply - to a non-number {:?}", inner)))
                    }
                }
            }
        }
    }
}