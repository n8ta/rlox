mod tests;

use crate::scanner::{Token, TokenInContext, Literal};
use crate::scanner;
use crate::scanner::Token::{MINUS, AND, OR, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, PLUS, SLASH, MULT, LITERAL, LPAREN, RPAREN, BANG_EQUAL, EQUAL_EQUAL, VAR, SEMICOLON};
use crate::source_ref::SourceRef;
use std::rc::Rc;
use crate::parser::Expr::Logical;
use colored::*;


#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct ParserError {
    pub msg: String,
    pub context: SourceRef,
}

impl ParserError {
    pub fn new(msg: String, context: SourceRef) -> ParserError { ParserError { msg, context } }
}

pub type Tokens = Vec<TokenInContext>;

pub fn parse(tokens: Tokens, source: Rc<String>) -> Result<Vec<Stmt>, ParserError> {
    let mut parser: Parser = Parser::new(tokens, source);
    parser.parse()
}


#[allow(dead_code)]
struct Parser {
    tokens: Tokens,
    current: usize,
    source: Rc<String>,
}

pub type ExprTy = Box<ExprInContext>;

#[derive(Clone, Debug, PartialOrd)]
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

impl PartialEq for ExprInContext {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum Stmt {
    Expr(ExprTy),
    Block(Vec<Stmt>),
    Print(ExprTy),
    Variable(String, Option<ExprTy>),
    If(ExprTy, Box<Stmt>, Option<Box<Stmt>>),
    While(ExprTy, Box<Stmt>),
}

#[derive(Clone, Debug, PartialOrd, PartialEq)]
pub enum LogicalOp {
    AND,
    OR,
}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum Expr {
    Binary(ExprTy, BinOp, ExprTy),
    Grouping(ExprTy),
    Literal(scanner::Literal),
    Unary(UnaryOp, ExprTy),
    Variable(String),
    Assign(String, ExprTy),
    Logical(ExprTy, LogicalOp, ExprTy),
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

impl LogicalOp {
    fn new(tk: Token) -> LogicalOp {
        match tk {
            AND => LogicalOp::AND,
            OR => LogicalOp::OR,
            _ => panic!("{:?} is not a valid logical op", tk)
        }
    }
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

type ExprResult = Result<ExprTy, ParserError>;

impl Parser {
    fn new(tokens: Tokens, source: Rc<String>) -> Parser {
        Parser { tokens, current: 0, source }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts: Vec<Stmt> = vec![];
        while !self.is_at_end() {
            stmts.push(self.declaration()?)
        }
        Ok(stmts)
    }

    fn check(&mut self, typ: Token) -> bool {
        if self.is_at_end() {
            false
        } else {
            typ.type_equal(&self.peek().token)
        }
    }

    fn consume(&mut self, typ: Token, message: &str) -> Result<TokenInContext, ParserError> {
        if self.check(typ.clone()) { return Ok(self.advance()); }
        Err(ParserError::new(
            format!("{} - didn't find a {:?} as expected. Found a {:?}",
                    message, typ, self.peek().token),
            self.tokens[self.current].context.clone()))
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

    fn err(&self, msg: String) -> ParserError {
        ParserError::new(msg, self.tokens[self.current].context.clone())
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(vec![VAR]) {
            match self.variable_declaration() {
                Ok(stmt) => Ok(stmt),
                Err(err) => {
                    self.synchronize();
                    Err(err)
                }
            }
        } else {
            match self.statement() {
                Ok(stmt) => Ok(stmt),
                Err(msg) => {
                    self.synchronize();
                    Err(msg)
                }
            }
        }
    }

    fn variable_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = self.consume(Token::IDENTIFIER(format!("")), "Expect variable name.").unwrap();
        let mut init: Option<ExprTy> = None;
        if self.matches(vec![Token::EQUAL]) {
            let exp = self.expression()?;
            init = Some(mk_expr(exp.expr, exp.context));
        }
        self.consume(SEMICOLON, "Expected ';' after variable declaration")?;
        if let Token::IDENTIFIER(str) = name.token {
            return Ok(Stmt::Variable(str.clone(), init));
        }
        Err(self.err(format!("FAILED didnt find a IDENT where expected")))
    }

    fn or(&mut self) -> ExprResult {
        let mut expr = self.and()?;
        while (self.matches(vec![Token::OR])) {
            let op = LogicalOp::new(self.previous().unwrap().token);
            let right = self.and()?;
            let cont = expr.context.merge(&right.context);
            expr = Box::new(ExprInContext::new(Expr::Logical(expr, op, right), cont));
        }
        Ok(expr)
    }

    fn and(&mut self) -> ExprResult {
        let mut expr = self.equality()?;
        while self.matches(vec![Token::AND]) {
            let op = LogicalOp::new(self.previous().unwrap().token);
            let right = self.equality()?;
            let context = expr.context.merge(&right.context);
            expr = Box::new(ExprInContext::new(Expr::Logical(expr, op, right), context));
        }
        Ok(expr)
    }

    fn assignment(&mut self) -> ExprResult {
        let expr = self.or()?;

        if self.matches(vec![Token::EQUAL]) {
            let _eq = self.previous().unwrap();
            let value: ExprTy = self.assignment()?;


            if let Expr::Variable(lit) = expr.expr {
                return Ok(mk_expr(Expr::Assign(lit, value.clone()),
                                  expr.context.merge(&value.context)));
            } else {
                return Err(self.err(format!("Invalid assignment target")));
            }
        }
        Ok(expr)
    }


    fn expression(&mut self) -> ExprResult {
        self.assignment()
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(vec![Token::PRINT]) {
            self.print_statement()
        } else if self.matches(vec![Token::WHILE]) {
            self.while_statement()
        } else if self.matches(vec![Token::LBRACE]) {
            self.block()
        } else if self.matches(vec![Token::IF]) {
            self.if_statement()
        } else if self.matches(vec![Token::FOR]) {
            self.for_statement()
        } else {
            self.expression_statement()
        }
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(Token::LPAREN, "Expected '(' after 'while'")?;
        let expr = self.expression()?;
        self.consume(Token::RPAREN, "Expected ')' after while condition")?;
        let body = self.statement()?;
        Ok(Stmt::While(expr, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(Token::LPAREN, "Expected '(' after 'if'")?;
        let test = self.expression()?;
        self.consume(Token::RPAREN, "Expected ')' after `if (... ")?;
        let if_branch = self.statement()?;
        let mut else_branch: Option<Box<Stmt>> = None;
        if self.matches(vec![Token::ELSE]) {
            else_branch = Some(Box::new(self.statement()?));
        }
        Ok(Stmt::If(test, Box::new(if_branch), else_branch))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(Token::LPAREN, "Expected a '(' after a for loop")?;
        let init: Option<Stmt> = if self.matches(vec![Token::SEMICOLON]) {
            None
        } else if self.matches(vec![Token::VAR]) {
            Some(self.variable_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };
        let mut condition: Option<ExprTy> = None;
        if !self.check(Token::SEMICOLON) {
            condition = Some(self.expression()?);
        }
        self.consume(Token::SEMICOLON, "Expect ';' after for loop condition.")?;
        let mut increment: Option<ExprTy> = None;
        if !self.check(Token::RPAREN) {
            increment = Some(self.expression()?);
        }
        self.consume(Token::RPAREN, "Expected ')' after for loop")?;
        let mut body = self.statement()?;
        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)]);
        }
        let src = self.tokens[self.current].context.clone();
        if condition.is_none() {
            condition = Some(Box::new(ExprInContext::new(Expr::Literal(Literal::BOOL(true)), src)));
        }
        body = Stmt::While(condition.unwrap(), Box::new(body));
        if let Some(init) = init {
            body = Stmt::Block(vec![init, body])
        }
        Ok(body)
    }

    fn block(&mut self) -> Result<Stmt, ParserError> {
        let mut stmts: Vec<Stmt> = vec![];
        while !self.check(Token::RBRACE) && !self.is_at_end() {
            stmts.push(self.declaration()?)
        }
        self.consume(Token::RBRACE, "Expected block to end with an '}'.")?;
        Ok(Stmt::Block(stmts))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let value: ExprTy = self.expression()?;
        self.consume(Token::SEMICOLON, "Expected ';' after print value.")?;
        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let value: ExprTy = self.expression()?;
        self.consume(Token::SEMICOLON, "Expected ';' after expression.")?;
        Ok(Stmt::Expr(value))
    }

    fn equality(&mut self) -> ExprResult {
        let mut expr = self.comparison()?;
        while self.matches(vec![BANG_EQUAL, EQUAL_EQUAL]) {
            let operator = BinOp::new(self.previous().unwrap().token);
            let right = self.comparison()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ExprResult {
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

    fn term(&mut self) -> ExprResult {
        let mut expr: ExprTy = self.factor()?;
        while self.matches(vec![Token::MINUS, PLUS]) {
            let operator = BinOp::new(self.previous().unwrap().token);
            let right = self.factor()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context)
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ExprResult {
        let mut expr = self.unary()?;
        while self.matches(vec![SLASH, MULT]) {
            let operator = BinOp::new(self.previous().unwrap().token);
            let right = self.unary()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ExprResult {
        if self.matches(vec![Token::BANG, Token::MINUS]) {
            let operator = UnaryOp::new(self.previous().unwrap().token);
            let right = self.unary()?;
            let context = self.previous().unwrap().context.merge(&right.context);
            return Ok(mk_expr(Expr::Unary(operator, right), context));
        }
        Ok(self.primary()?)
    }

    fn primary(&mut self) -> ExprResult {
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
            self.consume(RPAREN, "Expected ')' after expression.")?;
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

        if self.matches(vec![Token::IDENTIFIER(format!(""))]) {
            if let Token::IDENTIFIER(str) = self.previous().unwrap().token {
                return Ok(mk_expr(Expr::Variable(str.clone()), self.previous().unwrap().context));
            }
            panic!("Here be dragons");
        }
        // if self.matches(vec![Token::IDENTIFIER]) {
        //     return Ok(mk_expr(Expr::Variable(self.previous().unwrap().), self.previous().unwrap().context));
        // }
        Err(self.err(format!("Failed to match any expression for `{}`", self.tokens[self.current].context.source())))
    }
}

