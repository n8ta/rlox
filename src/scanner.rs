use std::collections::HashMap;
use crate::source_ref::SourceRef;
use std::fmt::{Display, Formatter};
use crate::scanner::Token::IDENTIFIER;
use std::rc::Rc;

lazy_static! {
    static ref KEYWORDS: HashMap<String, Token> = {
        let mut map: HashMap<String, Token> = HashMap::default();
        map.insert(String::from("and"), Token::AND);
        map.insert(String::from("else"), Token::ELSE);
        map.insert(String::from("false"), Token::LITERAL(Literal::BOOL(false)));
        map.insert(String::from("for"), Token::FOR);
        map.insert(String::from("fun"), Token::FUN);
        map.insert(String::from("if"), Token::IF);
        map.insert(String::from("nil"), Token::LITERAL(Literal::NIL));
        map.insert(String::from("or"), Token::OR);
        map.insert(String::from("print"), Token::PRINT);
        map.insert(String::from("return"), Token::RETURN);
        map.insert(String::from("super"), Token::SUPER);
        map.insert(String::from("this"), Token::THIS);
        map.insert(String::from("true"), Token::LITERAL(Literal::BOOL(true)));
        map.insert(String::from("var"), Token::VAR);
        map.insert(String::from("while"), Token::WHILE);
        map
    };
}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum Literal {
    STRING(String),
    NUMBER(f64),
    BOOL(bool),
    NIL,
}

/// Equality is type equality not value equality
impl Literal {
    pub fn tname(&self) -> String {
        String::from(
            match self {
                Literal::STRING(_) => "STRING",
                Literal::NUMBER(_) => "NUMBER",
                Literal::BOOL(_) => "BOOL",
                Literal::NIL => "NIL",
            }
        )
    }
    pub fn truthy(&self) -> bool {
        match self {
            Literal::STRING(_) => true,
            Literal::NUMBER(_) => true,
            Literal::BOOL(bol) => *bol,
            Literal::NIL => false,
        }
    }
    pub fn type_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::STRING(_), Literal::STRING(_)) => true,
            (Literal::NUMBER(_), Literal::NUMBER(_)) => true,
            (Literal::BOOL(_), Literal::BOOL(_)) => true,
            (Literal::NIL, Literal::NIL) => true,
            _ => false,
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::STRING(str) => f.write_str(str),
            Literal::NUMBER(num) => f.write_str(&num.to_string()),
            Literal::BOOL(bol) => f.write_str(if *bol { "true" } else { "false" }),
            Literal::NIL => f.write_str("nil"),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Token {
    PLUS,
    MULT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    EQUAL,
    EQUAL_EQUAL,
    BANG_EQUAL,
    BANG,
    MINUS,
    LITERAL(Literal),
    IDENTIFIER(String),
    SEMICOLON,
    COMMA,
    DOT,
    SLASH,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    AND,
    ELSE,
    FOR,
    FUN,
    CLASS,
    IF,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    VAR,
    WHILE,
    EOF,
}

#[derive(Debug, Clone, PartialOrd)]
pub struct TokenInContext {
    pub token: Token,
    pub context: SourceRef,
}

impl Display for TokenInContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self.token))
    }
}

impl TokenInContext {
    pub fn new(token: Token, offset: usize, len: usize, line: usize, src: Rc<String>) -> TokenInContext {
        TokenInContext { token, context: SourceRef::new(offset, len, line, src) }
    }
}

type ScannerResult = Result<Vec<TokenInContext>, (String, usize)>;

impl Token {
    /// Compare two tokens by type only
    pub(crate) fn type_equal(&self, other: &Token) -> bool {
        match (self, other) {
            (Token::PLUS, Token::PLUS) => true,
            (Token::MULT, Token::MULT) => true,
            (Token::LPAREN, Token::LPAREN) => true,
            (Token::RPAREN, Token::RPAREN) => true,
            (Token::LBRACE, Token::LBRACE) => true,
            (Token::RBRACE, Token::RBRACE) => true,
            (Token::EQUAL, Token::EQUAL) => true,
            (Token::EQUAL_EQUAL, Token::EQUAL_EQUAL) => true,
            (Token::BANG_EQUAL, Token::BANG_EQUAL) => true,
            (Token::BANG, Token::BANG) => true,
            (Token::MINUS, Token::MINUS) => true,
            (Token::LITERAL(a), Token::LITERAL(b)) => a.type_equal(&b),
            (Token::IDENTIFIER(_), Token::IDENTIFIER(_)) => true,
            (Token::SEMICOLON, Token::SEMICOLON) => true,
            (Token::COMMA, Token::COMMA) => true,
            (Token::DOT, Token::DOT) => true,
            (Token::SLASH, Token::SLASH) => true,
            (Token::GREATER, Token::GREATER) => true,
            (Token::GREATER_EQUAL, Token::GREATER_EQUAL) => true,
            (Token::LESS, Token::LESS) => true,
            (Token::LESS_EQUAL, Token::LESS_EQUAL) => true,
            (Token::AND, Token::AND) => true,
            (Token::ELSE, Token::ELSE) => true,
            (Token::FOR, Token::FOR) => true,
            (Token::FUN, Token::FUN) => true,
            (Token::CLASS, Token::CLASS) => true,
            (Token::IF, Token::IF) => true,
            (Token::OR, Token::OR) => true,
            (Token::PRINT, Token::PRINT) => true,
            (Token::RETURN, Token::RETURN) => true,
            (Token::SUPER, Token::SUPER) => true,
            (Token::THIS, Token::THIS) => true,
            (Token::VAR, Token::VAR) => true,
            (Token::WHILE, Token::WHILE) => true,
            (Token::EOF, Token::EOF) => true,
            _ => false,
        }
    }
}

impl PartialEq<TokenInContext> for Token {
    fn eq(&self, other: &TokenInContext) -> bool {
        other.token == *self
    }
}

impl PartialEq for TokenInContext {
    fn eq(self: &TokenInContext, b: &TokenInContext) -> bool {
        return self.token == b.token;
    }
}

pub fn scanner(src: Rc<String>) -> ScannerResult {
    let mut scanner = Scanner::new(src);
    scanner.scan_tokens()
}

struct Scanner {
    src: Rc<String>,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<TokenInContext>,
}

impl Scanner {
    fn new(src: Rc<String>) -> Scanner { Scanner { src, start: 0, current: 0, line: 0, tokens: vec![] } }
    fn is_at_end(&self) -> bool {
        self.current >= self.src.chars().count()
    }
    fn advance(&mut self) -> char {
        let x = self.src.chars().nth(self.current).unwrap();
        self.current += 1;
        x
    }
    fn add_token(&mut self, tt: Token) {
        self.tokens.push(
            TokenInContext::new(
                tt,
                self.start,
                self.current - self.start,
                self.line,
            self.src.clone(),
            )
        );
    }
    fn string(&mut self) -> Result<(), String> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' { self.line += 1; }
            self.advance();
        }
        if self.is_at_end() {
            let partial_str = self.src.chars().skip(self.start).take(self.src.len() - self.start).collect::<String>();
            return Err(format!("Unterminated String: {}", partial_str));
        }
        self.advance();
        self.add_token(
            Token::LITERAL(
                Literal::STRING(
                    self.src.chars().skip(self.start + 1).take(self.current - self.start - 2).collect::<String>())));
        return Ok(());
    }
    fn number(&mut self) -> Result<(), String> {
        while self.peek().is_digit(10) { self.advance(); }
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
        }
        while self.peek().is_digit(10) { self.advance(); }

        let num = self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>();
        let float = match num.parse::<f64>() {
            Ok(float) => float,
            Err(_) => {
                return Err(String::from("Unable to parse f64 {}"));
            }
        };
        self.add_token(Token::LITERAL(Literal::NUMBER(float)));
        Ok(())
    }
    fn identifier(&mut self) -> Result<(), String> {
        while self.peek().is_alphanumeric() { self.advance(); }
        let ident = self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>();
        match KEYWORDS.get(&ident) {
            Some(k) => self.add_token(k.clone()),
            None => self.add_token(IDENTIFIER(ident)),
        };
        Ok(())
    }
    fn peek(&mut self) -> char {
        match self.src.chars().nth(self.current) {
            None => 0x0 as char,
            Some(c) => c,
        }
    }
    fn peek_next(&self) -> char {
        match self.src.chars().nth(self.current + 1) {
            None => 0x0 as char,
            Some(c) => c,
        }
    }
    fn scan_token(&mut self) -> Result<(), String> {
        let c = self.advance();
        match c {
            '(' => self.add_token(Token::LPAREN),
            ')' => self.add_token(Token::RPAREN),
            '{' => self.add_token(Token::LBRACE),
            '}' => self.add_token(Token::RBRACE),
            ',' => self.add_token(Token::COMMA),
            '.' => self.add_token(Token::DOT),
            '-' => self.add_token(Token::MINUS),
            '+' => self.add_token(Token::PLUS),
            ';' => self.add_token(Token::SEMICOLON),
            '*' => self.add_token(Token::MULT),
            '!' => {
                let tt = match self.matches('=') {
                    true => Token::BANG_EQUAL,
                    false => Token::BANG,
                };
                self.add_token(tt);
            }
            '=' => {
                let tt = match self.matches('=') {
                    true => Token::EQUAL_EQUAL,
                    false => Token::EQUAL
                };
                self.add_token(tt)
            }
            '<' => {
                let tt = match self.matches('=') {
                    true => Token::LESS_EQUAL,
                    false => Token::LESS
                };
                self.add_token(tt)
            }
            '>' => {
                let tt = match self.matches('=') {
                    true => Token::GREATER_EQUAL,
                    false => Token::GREATER
                };
                self.add_token(tt)
            }
            '/' => {
                if self.matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(Token::SLASH);
                }
            }
            '"' => self.string()?,
            '\r' => (),
            '\t' => (),
            ' ' => (),
            '\n' => self.line += 1,
            _ => {
                if c.is_digit(10) {
                    self.number()?;
                } else if c.is_alphabetic() {
                    self.identifier()?;
                } else {
                    return Err(format!("Unexpected token `{}`", c));
                }
            }
        }
        Ok(())
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false; }
        if self.src.chars().nth(self.current).unwrap() != expected { return false; }
        self.current += 1;
        true
    }

    fn scan_tokens(&mut self) -> ScannerResult {
        while !self.is_at_end() {
            if let Err(x) = self.scan_token() {
                return Err((x, self.line));
            }
            self.start = self.current;
        }
        self.tokens.push(TokenInContext::new(
            Token::EOF,
            self.current,
            self.current - self.start,
            self.line,
            self.src.clone(),
        ));
        Ok(self.tokens.clone())
    }
}

// Unwraps scanner result for easy tests
fn test_scanner(src: String) -> Vec<TokenInContext> {
    let mut scanner = Scanner::new(Rc::new(src));
    let mut tokens = scanner.scan_tokens().unwrap();
    tokens.pop(); // remove EOF
    tokens
}


#[test]
fn test_basic_lexemes() {
    type TT = Token;
    assert_eq!(vec![TT::EQUAL], test_scanner(String::from("=")));
    assert_eq!(vec![TT::BANG_EQUAL], test_scanner(String::from("!=")));
    assert_eq!(vec![TT::BANG_EQUAL, TT::EQUAL_EQUAL], test_scanner(String::from("!===")));
    assert_eq!(vec![TT::PLUS, TT::MINUS, TT::SLASH, TT::DOT, TT::MULT], test_scanner(String::from("+-/.*")));
    assert_eq!(vec![TT::LPAREN, TT::RPAREN], test_scanner(String::from("()")));
    assert_eq!(vec![TT::LESS, TT::LESS_EQUAL], test_scanner(String::from("<<=")));
    assert_eq!(vec![TT::GREATER, TT::GREATER_EQUAL], test_scanner(String::from(">>=")));
    assert_eq!(vec![TT::GREATER, TT::GREATER_EQUAL], test_scanner(String::from("\r\n\t>>=")));
}

#[test]
fn test_strings() {
    type TT = Token;
    assert_eq!(vec![TT::LITERAL(Literal::STRING(String::from("Cat"))), TT::LITERAL(Literal::STRING(String::from("HAT")))], test_scanner(String::from("\"Cat\" \"HAT\"")));
    assert_eq!(vec![TT::LITERAL(Literal::STRING(String::from("Helloßßßßß")))], test_scanner(String::from("\"Helloßßßßß\"")));
    assert_eq!(vec![TT::LITERAL(Literal::STRING(String::from("Hello"))), TT::GREATER], test_scanner(String::from("\"Hello\" >")));
    assert_eq!(vec![TT::LITERAL(Literal::STRING(String::from("")))], test_scanner(String::from("   \t\r\n\"\" ")));
}

#[test]
fn test_numbers() {
    type TT = Token;
    assert_eq!(vec![TT::LITERAL(Literal::NUMBER(123.0))], test_scanner(String::from("123")));
    assert_eq!(vec![TT::LITERAL(Literal::NUMBER(123.34))], test_scanner(String::from("123.34")));
    assert_eq!(vec![TT::MINUS, TT::LITERAL(Literal::NUMBER(123.0))], test_scanner(String::from("-123.0")));
    assert_ne!(vec![TT::MINUS, TT::LITERAL(Literal::NUMBER(124.0))], test_scanner(String::from("-123.0")));
    assert_ne!(vec![TT::LITERAL(Literal::NUMBER(124.0))], test_scanner(String::from("123.0")));
    assert_eq!(vec![TT::LITERAL(Literal::NUMBER(0.0))], test_scanner(String::from("0.0")));
}

#[test]
fn test_misc() {
    type TT = Token;
    assert_eq!(vec![TT::AND, TT::OR, TT::LITERAL(Literal::BOOL(true)), TT::LITERAL(Literal::BOOL(false))], test_scanner(String::from("and or true\n false")));
    assert_eq!(vec![TT::LITERAL(Literal::NIL), TT::PRINT, TT::RETURN, TT::WHILE], test_scanner(String::from("nil print return while")));
}

#[test]
fn test_identifier_and_assign() {
    type TT = Token;

    assert_eq!(vec![TT::VAR, TT::IDENTIFIER("Hello".to_string()), TT::SEMICOLON],
               test_scanner(format!("var Hello;")));

    let hello = TT::IDENTIFIER("Hello".to_string());
    assert_eq!(vec![TT::VAR, TT::IDENTIFIER("Hello".to_string()), TT::SEMICOLON, hello.clone(), TT::EQUAL, hello.clone(), TT::PLUS, TT::LITERAL(Literal::NUMBER(1.0)), TT::SEMICOLON],
               test_scanner(format!("var Hello;\nHello = Hello + 1;")));
}