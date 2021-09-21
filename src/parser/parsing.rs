use crate::scanner::{Token, TokenInContext, Literal};
use crate::scanner;
use crate::scanner::Token::{MINUS, AND, OR, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, PLUS, SLASH, MULT, LITERAL, LPAREN, RPAREN, BANG_EQUAL, EQUAL_EQUAL, VAR, SEMICOLON, IDENTIFIER, LBRACE, RBRACE};
use crate::source_ref::SourceRef;
use std::rc::Rc;
use crate::parser::Expr::Logical;
use colored::*;
use crate::parser::types::{Tokens, Stmt, ParserError, ExprTy, LogicalOp, ExprInContext, Expr, UnaryOp, ExprResult, BinOp, Func};

pub fn parse(tokens: Tokens, source: Rc<String>) -> Result<Vec<Stmt>, ParserError> {
    let mut parser: Parser = Parser::new(tokens, source);
    parser.parse()
}


fn mk_expr(expr: Expr, context: SourceRef) -> ExprTy {
    Box::new(ExprInContext::new(expr, context))
}


#[allow(dead_code)]
pub(crate) struct Parser {
    tokens: Tokens,
    current: usize,
    source: Rc<String>,
}

impl Parser {
    pub(crate) fn new(tokens: Tokens, source: Rc<String>) -> Parser {
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
        self.tokens[self.current].token.type_equal(&Token::EOF)
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
        } else if self.matches(vec![Token::FUN]) {
            self.function()
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

    fn function(&mut self) -> Result<Stmt, ParserError> {
        let name_in_context = self.consume(Token::IDENTIFIER(format!("")), "Expected a function name")?;
        let name = if let Token::IDENTIFIER(str) = name_in_context.token { str } else {
            return Err(self.err(format!("Expected a function name")));
        };
        self.consume(Token::LPAREN, "Expected '(' after function name")?;
        let mut params_untyped: Vec<TokenInContext> = vec![];
        if !self.check(RPAREN) {
            loop {
                if params_untyped.len() >= 255 {
                    return Err(self.err(format!("Cannot have more than 255 parameters.")));
                };
                params_untyped.push(self.consume(IDENTIFIER(format!("")), "Expected parameter name of functon arg")?);
            }
        }
        // TODO: Make less shit
        let mut params: Vec<(String, SourceRef)> = vec![];
        for param in params_untyped {
            if let Token::IDENTIFIER(str) = param.token {
                params.push((str, param.context))
            } else {
                panic!("This shouldn't happen");
            }
        }

        self.consume(RPAREN, "Expected a ')' after function parameters")?;
        self.consume(LBRACE, "Expected a '{' after a function declaration")?;
        let body: Vec<Stmt> = if let Stmt::Block(blk) = self.block()? { blk } else { panic!("block() didn't return a block"); };
        Ok(Stmt::Function(Func::new(name, params, body, name_in_context.context.clone())))

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


    pub fn expression(&mut self) -> ExprResult {
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
        let mut else_branch: Option<Box<Vec<Stmt>>> = None;
        if self.matches(vec![Token::ELSE]) {
            else_branch = Some(Box::new(vec![self.statement()?]));
        }
        Ok(Stmt::If(test, Box::new(vec![if_branch]), else_branch))
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
        // Ok(self.primary()?)
        Ok(self.call()?)
    }

    fn call(&mut self) -> ExprResult {
        let mut expr = self.primary()?;
        loop {
            if self.matches(vec![Token::LPAREN]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: ExprTy) -> ExprResult {
        let mut args: Vec<ExprTy> = vec![];
        let mut context = callee.context.clone();
        if !self.check(Token::RPAREN) {
            while self.matches(vec![Token::COMMA]) {
                if args.len() >= 255 {
                    return Err(self.err("Can't have more than 255 args".to_string()));
                }
                let expr = self.expression()?;
                context = context.merge(&expr.context);
                args.push(expr);
            }
        }
        self.consume(Token::RPAREN, "Expected a ')' after function arguments")?;
        Ok(Box::new(ExprInContext::new(Expr::Call(callee, args), context)))
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
            // of the Token enum to avoid the second type check. (Enum variants aren't types, very sad)
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
        Err(self.err(format!("Failed to match any expression for `{}`", self.tokens[self.current].context.source())))
    }
}