use crate::scanner::{Token, TokenInContext, Literal};
use crate::scanner;
use crate::scanner::Token::{MINUS, AND, OR, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, PLUS, SLASH, MULT, LITERAL, LPAREN, RPAREN, BANG_EQUAL, EQUAL_EQUAL};
use crate::source_ref::SourceRef;

type Tokens = Vec<TokenInContext>;

pub fn parse(tokens: Tokens, source: String) -> PResult {
    let mut parser = Parser::new(tokens, source);
    parser.parse()
}

struct Parser {
    tokens: Tokens,
    current: usize,
    source: String,
}

pub type ExprTy = Box<ExprInContext>;

#[derive(Clone, Debug, PartialOrd, PartialEq)]
pub struct ExprInContext {
    pub context: SourceRef,
    pub expr: Expr,
}

fn mk_expr(expr: Expr, context: SourceRef) -> ExprTy {
    Box::new(ExprInContext::new(expr, context))
}

impl ExprInContext {
    fn new(expr: Expr, context: SourceRef) -> ExprInContext {
        ExprInContext { expr, context }
    }
}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum Expr {
    Binary(ExprTy, BinOp, ExprTy),
    Grouping(ExprTy),
    Literal(scanner::Literal),
    Unary(UnaryOp, ExprTy),
}

#[allow(non_camel_case_types)]
#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum BinOp {
    EQUAL_EQUAL,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    PLUS,
    SLASH,
    MULT,
    MINUS,
    GREATER,
    GREATER_EQUAL,
    AND,
    OR,
}

#[allow(non_camel_case_types)]
#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum UnaryOp {
    MINUS,
    BANG,
}


impl BinOp {
    fn new(tk: Token) -> BinOp {
        match tk {
            PLUS => BinOp::PLUS,
            MULT => BinOp::MULT,
            EQUAL_EQUAL => BinOp::EQUAL_EQUAL,
            BANG_EQUAL => BinOp::BANG_EQUAL,
            MINUS => BinOp::MINUS,
            SLASH => BinOp::SLASH,
            GREATER => BinOp::GREATER,
            GREATER_EQUAL => BinOp::GREATER_EQUAL,
            LESS => BinOp::LESS,
            LESS_EQUAL => BinOp::LESS_EQUAL,
            AND => BinOp::AND,
            OR => BinOp::OR,
            _ => panic!("{:?} is not a valid binary op", tk)
        }
    }
}

impl UnaryOp {
    fn new(tk: Token) -> UnaryOp {
        match tk {
            Token::BANG => UnaryOp::BANG,
            Token::MINUS => UnaryOp::MINUS,
            _ => panic!("{:?} is not a valid unary op", tk)
        }
    }
}

type PResult = Result<ExprTy, String>;

impl Parser {
    fn new(tokens: Tokens, source: String) -> Parser {
        Parser { tokens, current: 0, source }
    }

    fn parse(&mut self) -> Result<ExprTy, String> {
        match self.expression() {
            Ok(prog) => Ok(prog),
            Err(err) => {
                Err(format!("[line {}] Error: {}", self.tokens[self.current].context.line, err))
            }
        }
    }

    fn check(&mut self, typ: Token) -> bool {
        if self.is_at_end() {
            false
        } else {
            typ == self.peek()
        }
    }

    fn consume(&mut self, typ: Token, _message: String) -> Result<TokenInContext, String> {
        if self.check(typ.clone()) { return Ok(self.advance()); }
        Err(format!("Consume failed, didn't find a {:?} as expected.", typ))
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().is_some() && self.previous().unwrap().token.type_equal(&Token::SEMICOLON) {
                return;
            }
            match self.peek().token {
                Token::CLASS | Token::FUN | Token::VAR | Token::FOR | Token::IF | Token::WHILE | Token::PRINT | Token::RETURN => {
                    return;
                }
                _ => {}
            }
            self.advance();
        }
    }

    fn matches(&mut self, tokens: Vec<Token>) -> bool {
        let tkn = match self.tokens.get(self.current) {
            None => return false,
            Some(t) => &t.token,
        };
        for expected in tokens.iter() {
            if expected.type_equal(tkn) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn previous(&self) -> Option<TokenInContext> {
        if self.current == 0 {
            return None;
        }
        Some(self.tokens[self.current - 1].clone())
    }

    fn peek(&self) -> TokenInContext {
        return self.tokens[self.current].clone();
    }

    fn is_at_end(&self) -> bool {
        Token::EOF == self.tokens[self.current]
    }

    fn advance(&mut self) -> TokenInContext {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().unwrap()
    }

    fn expression(&mut self) -> PResult {
        self.equality()
    }

    fn equality(&mut self) -> PResult {
        let mut expr = self.comparison()?;
        while self.matches(vec![BANG_EQUAL, EQUAL_EQUAL]) {
            let operator = BinOp::new(self.previous().unwrap().token);
            let right = self.comparison()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> PResult {
        let mut expr = self.term()?;
        while self.matches(vec![GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]) {
            let operator = BinOp::new(self.previous().unwrap().token);
            let right = self.term()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right),
                           context)
        }
        Ok(expr)
    }

    fn term(&mut self) -> PResult {
        let mut expr: ExprTy = self.factor()?;
        while self.matches(vec![Token::MINUS, PLUS]) {
            let operator = BinOp::new(self.previous().unwrap().token);
            let right = self.factor()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context)
        }
        Ok(expr)
    }

    fn factor(&mut self) -> PResult {
        let mut expr = self.unary()?;
        while self.matches(vec![SLASH, MULT]) {
            let operator = BinOp::new(self.previous().unwrap().token);
            let right = self.unary()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> PResult {
        if self.matches(vec![Token::BANG, Token::MINUS]) {
            let operator = UnaryOp::new(self.previous().unwrap().token);
            let right = self.unary()?;
            let context = self.previous().unwrap().context.merge(&right.context);
            return Ok(mk_expr(Expr::Unary(operator, right), context));
        }
        Ok(self.primary()?)
    }

    fn primary(&mut self) -> PResult {
        if self.matches(vec![LITERAL(Literal::BOOL(false))]) {
            if let LITERAL(Literal::BOOL(b)) = self.previous().unwrap().token {
                return Ok(mk_expr(Expr::Literal(Literal::BOOL(b)), self.previous().unwrap().context));
            }
            panic!("This path shouldn't happen. I need a better matches() func");
        }
        if self.matches(vec![LITERAL(Literal::BOOL(true))]) { return Ok(mk_expr(Expr::Literal(Literal::BOOL(true)), self.previous().unwrap().context)); }
        if self.matches(vec![LITERAL(Literal::NIL)]) { return Ok(mk_expr(Expr::Literal(Literal::NIL), self.previous().unwrap().context)); }
        if self.matches(vec![LITERAL(Literal::NUMBER(1.0))]) {
            // These ifs should always be true (based on the match above). This is an awkward intersection of javas
            // (language used in the book) Object base class and rust's type system.
            // I couldn't find a good way to write a generic match that return a specific variant
            // of the Token enum to avoid the second type check.
            if let Token::LITERAL(lit) = self.previous().unwrap().token {
                if let Literal::NUMBER(num) = lit {
                    return Ok(mk_expr(Expr::Literal(Literal::NUMBER(num)), self.previous().unwrap().context));
                }
            }
            panic!("This path shouldn't happen!");
        }
        if self.matches(vec![LPAREN]) {
            let expr = self.expression()?;
            self.consume(RPAREN, String::from("Expected ')' after expression."))?;
            return Ok(mk_expr(Expr::Grouping(expr), self.previous().unwrap().context));
        }
        if self.matches(vec![LITERAL(Literal::STRING("test".to_string()))]) {
            let str = self.previous().expect("Expected there to be a previous token b/c match passed");
            if let Token::LITERAL(lit) = str.token {
                if let Literal::STRING(str) = lit {
                    return Ok(mk_expr(Expr::Literal(Literal::STRING(str)), self.previous().unwrap().context));
                }
            }
            panic!("Also shouldn't happen. something wrong with matches function");
        }
        Err(format!("Failed to match any expression for {}", self.tokens[self.current].context.source(&self.source)))
    }
}

/// Turn a string into an expression to write simple tests
fn help(str: &str) -> ExprTy {
    let mut tokens = scanner(str.to_string()).unwrap();
    tokens.pop();
    parse(tokens, "".to_string()).unwrap()
}


fn mk_expr_test(expr: Expr) -> ExprTy {
    Box::new(ExprInContext::new(expr, SourceRef::new(0, 0, 0)))
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