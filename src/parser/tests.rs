use crate::source_ref::{SourceRef, Source};
use crate::scanner::{Literal};
use crate::parser::{ExprInContext, Expr, parse, ExprTy, UnaryOp, BinOp, Stmt, Tokens};
use std::rc::Rc;
use crate::scanner;
use crate::parser::parsing::Parser;

/// Used in tests
fn parse_expr(tokens: Tokens, source: Rc<String>) -> ExprTy {
    let mut parser = Parser::new(tokens, source);
    parser.expression().unwrap()
}

/// Turn a string into an expression to write simple tests
fn help(str: &str) -> ExprTy {
    let mut tokens = scanner(Rc::new(Source::new(str.to_string()))).unwrap();
    tokens.pop();
    parse_expr(tokens, Rc::new(String::from("")))
}


fn mk_expr_test(expr: Expr) -> ExprTy {
    Box::new(ExprInContext::new(expr, SourceRef::new(0, 0, 0, Rc::new(Source::new("".to_string())))))
}


#[test]
fn test_unary() {
    assert_eq!(help("1"), mk_expr_test(Expr::Literal(Literal::NUMBER(1.0))));
    assert_eq!(mk_expr_test(
        Expr::Unary(
            UnaryOp::MINUS,
            mk_expr_test(Expr::Literal(Literal::NUMBER(1.0))))
    ), help("-1"));
    assert_eq!(mk_expr_test(
        Expr::Unary(
            UnaryOp::MINUS,
            mk_expr_test(Expr::Unary(
                UnaryOp::MINUS,
                mk_expr_test(Expr::Literal(Literal::NUMBER(1.0)))))),
    ), help("--1"));
    assert_eq!(mk_expr_test(
        Expr::Unary(
            UnaryOp::MINUS,
            mk_expr_test(Expr::Unary(
                UnaryOp::MINUS,
                mk_expr_test(Expr::Unary(
                    UnaryOp::MINUS,
                    mk_expr_test(Expr::Literal(Literal::NUMBER(1.0)))))))),
    ), help("---1"));
    assert_ne!(mk_expr_test(
        Expr::Unary(
            UnaryOp::MINUS,
            mk_expr_test(Expr::Unary(
                UnaryOp::MINUS,
                mk_expr_test(Expr::Unary(
                    UnaryOp::MINUS,
                    mk_expr_test(Expr::Literal(Literal::NUMBER(1.0)))))))),
    ), help("----1")); // wrong # of minuses
}

fn num(fl: f64) -> ExprTy {
    mk_expr_test(Expr::Literal(Literal::NUMBER(fl)))
}

#[test]
fn test_precedence() {
    let mult = mk_expr_test(Expr::Binary(
        num(2.0),
        BinOp::MULT,
        num(8.0),
    ));
    let add = mk_expr_test(Expr::Binary(
        num(1.0),
        BinOp::PLUS,
        mult.clone(),
    ));
    assert_eq!(mult, help("2 * 8"));
    assert_eq!(add, help("1 + 2 * 8"));
    let lt = mk_expr_test(Expr::Binary(
        add.clone(),
        BinOp::LESS,
        num(13.3)));
    assert_eq!(lt.clone(), help("1 + 2 * 8 < 13.3"));
    let eqeq = mk_expr_test(
        Expr::Binary(
            num(29.0),
            BinOp::EQUAL_EQUAL,
            lt));
    assert_eq!(eqeq, help("29 == 1 + 2 * 8 < 13.3"));
    let not_five = mk_expr_test(
        Expr::Unary(
            UnaryOp::BANG,
            num(5.0)));
    assert_eq!(not_five.clone(), help("!5.0"));
    let add_not_five = mk_expr_test(
        Expr::Binary(
            not_five.clone(),
            BinOp::PLUS,
            num(4.0)));
    assert_eq!(add_not_five.clone(), help("!5.0 + 4"));
    let group = mk_expr_test(Expr::Grouping(mult.clone()));
    assert_eq!(
        group,
        help("(2 * 8)"));
    let group_group = mk_expr_test(Expr::Grouping(group.clone()));
    assert_eq!(group_group.clone(), help("((2*8))"));
}

#[test]
fn test_primaries() {
    let one = num(1.0);
    let hello = mk_expr_test(Expr::Literal(Literal::STRING("hello".into())));
    let fls = mk_expr_test(Expr::Literal(Literal::BOOL(false)));
    let tru = mk_expr_test(Expr::Literal(Literal::BOOL(true)));
    let nil = mk_expr_test(Expr::Literal(Literal::NIL));
    assert_eq!(one.clone(), help("1"));
    assert_eq!(hello.clone(), help("   \"hello\""));
    assert_eq!(fls.clone(), help("false"));
    assert_eq!(tru.clone(), help("true"));
    assert_eq!(nil.clone(), help("nil"));
}

#[test]
fn test_decl() {
    let src = Rc::new(Source::new(String::from("var varname = 1.0;")));
    let ast = Stmt::Variable(format!("varname"), Option::from(ExprTy::new(ExprInContext::new(
        Expr::Literal(Literal::NUMBER(1.0)),
        SourceRef::new(0, 14, 3, src.clone())))));
    let tokens = scanner(src.clone()).unwrap();
    assert_eq!(parse(tokens, Rc::new(src.src.clone())).unwrap(), vec![ast]);
}

#[test]
fn test_assign() {
    let src = Rc::new(Source::new("varname = 1.0;".to_string()));

    let context = SourceRef::new(10, 3, 0, src.clone());
    let context2 = SourceRef::new(0, 13, 0, src.clone());
    let expr = ExprInContext::new(Expr::Assign(format!("varname"),
                                               Box::new(
                                                   ExprInContext::new(
                                                       Expr::Literal(scanner::Literal::NUMBER(1.0)),
                                                       context
                                                   ))),
                                  context2);

    let ast = vec![Stmt::Expr(Box::new(expr))];

    let tokens = scanner(src.clone()).unwrap();
    assert_eq!(parse(tokens, Rc::new(src.src.clone())).unwrap()[0], ast[0]);
}