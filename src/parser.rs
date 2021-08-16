use crate::scanner::{Token, TokenInContext, Literal};
use crate::scanner;
use crate::scanner::Token::{GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, MINUS, PLUS, SLASH, MULT, LITERAL, LPAREN, RPAREN, BANG, BANG_EQUAL, EQUAL_EQUAL};

type Tokens = Vec<TokenInContext>;

pub fn parse(tokens: Tokens) -> PResult {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser {
    tokens: Tokens,
    current: usize,
}

pub type ExprTy = Box<Expr>;

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum Expr {
    Binary(ExprTy, TokenInContext, ExprTy),
    Grouping(ExprTy),
    Literal(scanner::Literal),
    Unary(TokenInContext, ExprTy),
}

type PResult = Result<ExprTy, String>;

impl Parser {
    fn new(tokens: Tokens) -> Parser {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) -> Result<ExprTy, String> {
        match self.expression() {
            Ok(prog) => Ok(prog),
            Err(err) => {
                Err(format!("[line {}] `{}` -- Error: {}", self.tokens[self.current].line, self.tokens[self.current].lexeme, err))
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

    fn consume(&mut self, typ: Token, message: String) -> Result<TokenInContext, String> {
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
                    return
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
            let operator = self.previous().unwrap();
            let right = self.comparison()?;
            expr = ExprTy::new(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> PResult {
        let mut expr = self.term()?;
        while self.matches(vec![GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]) {
            let operator = self.previous().unwrap();
            let right = self.term()?;
            expr = ExprTy::new(Expr::Binary(expr, operator, right))
        }
        Ok(expr)
    }

    fn term(&mut self) -> PResult {
        let mut expr: ExprTy = self.factor()?;
        while self.matches(vec![MINUS, PLUS]) {
            let operator = self.previous().unwrap();
            let right = self.factor()?;
            expr = ExprTy::new(Expr::Binary(expr, operator, right))
        }
        Ok(expr)
    }

    fn factor(&mut self) -> PResult {
        let mut expr = self.unary()?;
        while self.matches(vec![SLASH, MULT]) {
            let operator = self.previous().unwrap();
            let right = self.unary()?;
            expr = ExprTy::new(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> PResult {
        if self.matches(vec![BANG, MINUS]) {
            let operator = self.previous().unwrap();
            let right = self.unary()?;
            return Ok(ExprTy::new(Expr::Unary(operator, right)));
        }
        Ok(self.primary()?)
    }

    fn primary(&mut self) -> PResult {
        if self.matches(vec![LITERAL(Literal::FALSE)]) { return Ok(ExprTy::new(Expr::Literal(Literal::FALSE))); }
        if self.matches(vec![LITERAL(Literal::TRUE)]) { return Ok(ExprTy::new(Expr::Literal(Literal::TRUE))); }
        if self.matches(vec![LITERAL(Literal::NIL)]) { return Ok(ExprTy::new(Expr::Literal(Literal::NIL))); }
        if self.matches(vec![LITERAL(Literal::NUMBER(1.0))]) {
            // These ifs should always be true (based on the match above). This is an awkward intersection of javas
            // (language used in the book) Object base class and rust's type system.
            // I couldn't find a good way to write a generic match that return a specific variant
            // of the Token enum to avoid the second type check.
            if let Token::LITERAL(lit) = self.previous().unwrap().token {
                if let Literal::NUMBER(num) = lit {
                    return Ok(ExprTy::new(Expr::Literal(Literal::NUMBER(num))));
                }
            }
            panic!("This path shouldn't happen!");
        }
        if self.matches(vec![LPAREN]) {
            let expr = self.expression()?;
            self.consume(RPAREN, String::from("Expected ')' after expression."))?;
            return Ok(ExprTy::new(Expr::Grouping(expr)));
        }
        if self.matches(vec![LITERAL(Literal::STRING("test".to_string()))]) {
            let str = self.previous().expect("Expected there to be a previous token b/c match passed");
            if let Token::LITERAL(lit) = str.token {
                if let Literal::STRING(str) = lit {
                    return Ok(ExprTy::new(Expr::Literal(Literal::STRING(str))));
                }
            }
            panic!("Also shouldn't happen. something wrong with matches function");
        }
        Err(format!("Failed to match any expression for {:?}", self.tokens[self.current]))
    }
}

/// Turn a string into an expression to write simple tests
fn help(str: &str) -> ExprTy {
    let mut tokens = scanner(str.to_string()).unwrap();
    tokens.pop();
    parse(tokens).unwrap()
}

#[test]
fn test_unary() {
    assert_eq!(help("1"), ExprTy::new(Expr::Literal(Literal::NUMBER(1.0))));
    assert_eq!(ExprTy::new(
        Expr::Unary(
            TokenInContext::simple(Token::MINUS),
            ExprTy::new(Expr::Literal(Literal::NUMBER(1.0))))
    ), help("-1"));
    assert_eq!(ExprTy::new(
        Expr::Unary(
            TokenInContext::simple(Token::MINUS),
            ExprTy::new(Expr::Unary(
                TokenInContext::simple(Token::MINUS),
                ExprTy::new(Expr::Literal(Literal::NUMBER(1.0)))))),
    ), help("--1"));
    assert_eq!(ExprTy::new(
        Expr::Unary(
            TokenInContext::simple(Token::MINUS),
            ExprTy::new(Expr::Unary(
                TokenInContext::simple(Token::MINUS),
                ExprTy::new(Expr::Unary(
                    TokenInContext::simple(Token::MINUS),
                    ExprTy::new(Expr::Literal(Literal::NUMBER(1.0)))))))),
    ), help("---1"));
    assert_ne!(ExprTy::new(
        Expr::Unary(
            TokenInContext::simple(Token::MINUS),
            ExprTy::new(Expr::Unary(
                TokenInContext::simple(Token::MINUS),
                ExprTy::new(Expr::Unary(
                    TokenInContext::simple(Token::MINUS),
                    ExprTy::new(Expr::Literal(Literal::NUMBER(1.0)))))))),
    ), help("----1")); // wrong # of minuses
}

fn num(fl: f64) -> ExprTy {
    ExprTy::new(Expr::Literal(Literal::NUMBER(fl)))
}

#[test]
fn test_precedence() {
    let mult = ExprTy::new(Expr::Binary(
        num(2.0),
        TokenInContext::simple(Token::MULT),
        num(8.0),
    ));
    let add = ExprTy::new(Expr::Binary(
        num(1.0),
        TokenInContext::simple(Token::PLUS),
        mult.clone(),
    ));
    assert_eq!(mult, help("2 * 8"));
    assert_eq!(add, help("1 + 2 * 8"));
    let lt = ExprTy::new(Expr::Binary(
        add.clone(),
        TokenInContext::simple(Token::LESS),
        num(13.3)));
    assert_eq!(lt.clone(), help("1 + 2 * 8 < 13.3"));
    let eqeq = ExprTy::new(
        Expr::Binary(
            num(29.0),
            TokenInContext::simple(Token::EQUAL_EQUAL),
            lt));
    assert_eq!(eqeq, help("29 == 1 + 2 * 8 < 13.3"));
    let not_five = ExprTy::new(
        Expr::Unary(
            TokenInContext::simple(Token::BANG),
            num(5.0)));
    assert_eq!(not_five.clone(), help("!5.0"));
    let add_not_five = ExprTy::new(
        Expr::Binary(
            not_five.clone(),
            TokenInContext::simple(Token::PLUS),
            num(4.0)));
    assert_eq!(add_not_five.clone(), help("!5.0 + 4"));
    let group = ExprTy::new(Expr::Grouping(mult.clone()));
    assert_eq!(
        group,
        help("(2 * 8)"));
    let group_group = ExprTy::new(Expr::Grouping(group.clone()));
    assert_eq!(group_group.clone(), help("((2*8))"));
}

#[test]
fn test_primaries() {
    let one = num(1.0);
    let hello = ExprTy::new(Expr::Literal(Literal::STRING("hello".into())));
    let fls = ExprTy::new(Expr::Literal(Literal::FALSE));
    let tru = ExprTy::new(Expr::Literal(Literal::TRUE));
    let nil = ExprTy::new(Expr::Literal(Literal::NIL));
    assert_eq!(one.clone(), help("1"));
    assert_eq!(hello.clone(), help("   \"hello\""));
    assert_eq!(fls.clone(), help("false"));
    assert_eq!(tru.clone(), help("true"));
    assert_eq!(nil.clone(), help("nil"));
}