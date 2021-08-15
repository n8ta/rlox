use crate::scanner::{TokenInContext, Literal};
use crate::scanner::Token;
use crate::scanner;
use crate::scanner::Token::{GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, MINUS, PLUS, SLASH, MULT, LITERAL, LPAREN, RPAREN, BANG, BANG_EQUAL, EQUAL_EQUAL};
use std::process::exit;

type Tokens = Vec<TokenInContext>;

pub fn parser(tokens: Tokens) -> ExprTy {
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

impl Parser {
    fn new(tokens: Tokens) -> Parser {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) -> ExprTy {
        self.expression()
    }

    fn check(&mut self, typ: Token) -> bool {
        if self.is_at_end() {
            false
        } else {
            typ == self.peek()
        }
    }

    fn consume(&mut self, typ: Token, message: String) -> TokenInContext {
        if self.check(typ) { return self.advance(); }
        eprintln!("Consume failed!\n{}", message);
        exit(-1);
    }

    fn matches(&mut self, tokens: Vec<Token>) -> bool {
        let tkn = match self.tokens.get(self.current) {
            None => return false,
            Some(t) => &t.token,
        };
        for expected in tokens.iter() {
            if expected == tkn {
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

    fn expression(&mut self) -> ExprTy {
        self.equality()
    }

    fn equality(&mut self) -> ExprTy {
        let mut expr = self.comparison();
        while self.matches(vec![BANG_EQUAL, EQUAL_EQUAL]) {
            let operator = self.previous().unwrap();
            let right = self.comparison();
            expr = Box::new(Expr::Binary(expr, operator, right));
        }
        expr
    }

    fn comparison(&mut self) -> ExprTy {
        let mut expr = self.term();
        while self.matches(vec![GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]) {
            let operator = self.previous().unwrap();
            let right = self.term();
            expr = Box::new(Expr::Binary(expr, operator, right))
        }
        expr
    }

    fn term(&mut self) -> ExprTy {
        let mut expr: ExprTy = self.factor();
        while self.matches(vec![MINUS, PLUS]) {
            let operator = self.previous().unwrap();
            let right = self.factor();
            expr = Box::new(Expr::Binary(expr, operator, right))
        }
        expr
    }

    fn factor(&mut self) -> ExprTy {
        let mut expr = self.unary();
        while self.matches(vec![SLASH, MULT]) {
            let operator = self.previous().unwrap();
            let right = self.unary();
            expr = Box::new(Expr::Unary(operator, right));
        }
        expr
    }

    fn unary(&mut self) -> ExprTy {
        if self.matches(vec![BANG, MINUS]) {
            let operator = self.previous().unwrap();
            let right = self.unary();
            return Box::new(Expr::Unary(operator, right));
        }
        return self.primary();
    }

    fn primary(&mut self) -> ExprTy {
        if self.matches(vec![LITERAL(Literal::FALSE)]) { return Box::new(Expr::Literal(Literal::FALSE)); }
        if self.matches(vec![LITERAL(Literal::FALSE)]) { return Box::new(Expr::Literal(Literal::TRUE)); }
        if self.matches(vec![LITERAL(Literal::NIL)]) { return Box::new(Expr::Literal(Literal::NIL)); }
        if self.matches(vec![LITERAL(Literal::NUMBER(1.0))]) {
            if let Token::LITERAL(lit) = self.previous().unwrap().token {
                if let Literal::NUMBER(num) = lit {
                    return Box::new(Expr::Literal(Literal::NUMBER(num)));
                }
            }
        }

        // if self.matches(vec![LITERAL(Literal::NUMBER(1.0)), LITERAL(Literal::STRING(String::from("str")))]) {
        // let prev = self.previous();
        // if let Token::LITERAL(lit) = prev.token.clone() {
        //     if let Literal::NUMBER(num) = lit {
        //         return Box::new(Expr::Literal(Literal::NUMBER(num)));
        //     }
        // }
        // eprintln!("Failed. Expected a number literal but found {:?}", prev);
        // exit(-1);
        // }
        if self.matches(vec![LPAREN]) {
            let expr = self.expression();
            self.consume(RPAREN, String::from("Expected ')' after expression."));
            return Box::new(Expr::Grouping(expr));
        }
        eprintln!("Failed to match anything!");
        exit(-1);
    }
}

/// Turn a string into an expression to write simple tests
fn help(str: &str) -> ExprTy {
    let mut tokens = scanner(str.to_string()).unwrap();
    tokens.pop();
    parser(tokens)
}

#[test]
fn test_ast() {
    assert_eq!(help("1"), Box::new(Expr::Literal(Literal::NUMBER(1.0))));
    assert_eq!(Box::new(
        Expr::Unary(
            TokenInContext::simple(Token::MINUS),
            Box::new(Expr::Literal(Literal::NUMBER(1.0))))
    ), help("-1"));
}