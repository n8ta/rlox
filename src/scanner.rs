use std::process::exit;
use crate::scanner::TokenType::IDENTIFIER;

#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(non_camel_case_types)]
#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum TokenType {
    PLUS,
    MINUS,
    MULT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    EQUAL,
    EQUAL_EQUAL,
    BANG_EQUAL,
    BANG,
    STRING(String),
    NUMBER(f64),
    IDENTIFIER(String),
    SEMICOLON,
    COMMA,
    DOT,
    SLASH,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
}

#[derive(Debug, Clone)]
pub struct Token {
    token: TokenType,
    lexeme: String,
    line: usize,
}

impl Token {
    fn new(token: TokenType, lexeme: String, line: usize) -> Token {
        Token { token, lexeme, line }
    }
}

impl PartialEq<Token> for TokenType {
    fn eq(&self, other: &Token) -> bool {
        other.token == *self
    }
}

impl PartialEq for Token {
    fn eq(self: &Token, b: &Token) -> bool {
        return self.token == b.token;
    }
}

pub fn scanner(src: String) -> Vec<Token> {
    let mut scanner = Scanner::new(src);
    scanner.scan_tokens()
}

struct Scanner {
    src: String,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
}

impl Scanner {
    fn new(src: String) -> Scanner { Scanner { src, start: 0, current: 0, line: 0, tokens: vec![] } }
    fn is_at_end(&self) -> bool {
        self.current >= self.src.chars().count()
    }
    fn advance(&mut self) -> char {
        let x = self.src.chars().nth(self.current).unwrap();
        self.current += 1;
        x
    }
    fn add_token(&mut self, tt: TokenType) {
        self.tokens.push(
            Token::new(
                tt,
                self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>(),
                self.line)
        );
    }
    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' { self.line += 1; }
            self.advance();
        }
        if self.is_at_end() {
            eprintln!("Unterminated String\n");
            exit(-1);
        }
        self.advance();
        self.add_token(
            TokenType::STRING(
                self.src.chars().skip(self.start + 1).take(self.current - self.start - 2).collect::<String>()));
    }
    fn number(&mut self) {
        while self.peek().is_digit(10) { self.advance(); }
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
        }
        while self.peek().is_digit(10) { self.advance(); }
        self.add_token(TokenType::NUMBER(
            self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>()
                .parse::<f64>().unwrap()))
    }
    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() { self.advance(); }
        self.add_token(
            TokenType::IDENTIFIER(
                self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>()))
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
    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LPAREN),
            ')' => self.add_token(TokenType::RPAREN),
            '{' => self.add_token(TokenType::LBRACE),
            '}' => self.add_token(TokenType::RBRACE),
            ',' => self.add_token(TokenType::COMMA),
            '.' => self.add_token(TokenType::DOT),
            '-' => self.add_token(TokenType::MINUS),
            '+' => self.add_token(TokenType::PLUS),
            ';' => self.add_token(TokenType::SEMICOLON),
            '*' => self.add_token(TokenType::MULT),
            '!' => {
                let tt = match self.matches('=') {
                    true => TokenType::BANG_EQUAL,
                    false => TokenType::BANG,
                };
                self.add_token(tt);
            }
            '=' => {
                let tt = match self.matches('=') {
                    true => TokenType::EQUAL_EQUAL,
                    false => TokenType::EQUAL
                };
                self.add_token(tt)
            }
            '<' => {
                let tt = match self.matches('=') {
                    true => TokenType::LESS_EQUAL,
                    false => TokenType::LESS
                };
                self.add_token(tt)
            }
            '>' => {
                let tt = match self.matches('=') {
                    true => TokenType::GREATER_EQUAL,
                    false => TokenType::GREATER
                };
                self.add_token(tt)
            }
            '/' => {
                if self.matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::SLASH);
                }
            }
            '"' => self.string(),
            _ => {
                if c.is_digit(10) {
                    self.number();
                } else if c.is_alphabetic() {
                    self.identifier()
                } else {
                    eprintln!("Line {} unexpeted token `{}`", self.line, c);
                }
            }
        }
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false; }
        if self.src.chars().nth(self.current).unwrap() != expected { return false; }
        self.current += 1;
        true
    }

    fn scan_tokens(&mut self) -> Vec<Token> {
        let size = self.src.chars().count();
        while !self.is_at_end() {
            self.scan_token();
            self.start = self.current;
        }
        self.tokens.clone()
    }
}

#[test]
fn test_basic_lexemes() {
    type TT = TokenType;
    assert_eq!(vec![TT::EQUAL], scanner(String::from("=")));
    assert_eq!(vec![TT::BANG_EQUAL], scanner(String::from("!=")));
    assert_eq!(vec![TT::BANG_EQUAL, TT::EQUAL_EQUAL], scanner(String::from("!===")));
    assert_eq!(vec![TT::PLUS, TT::MINUS, TT::SLASH, TT::DOT, TT::MULT], scanner(String::from("+-/.*")));
    assert_eq!(vec![TT::LPAREN, TT::RPAREN], scanner(String::from("()")));
    assert_eq!(vec![TT::LESS, TT::LESS_EQUAL], scanner(String::from("<<=")));
    assert_eq!(vec![TT::GREATER, TT::GREATER_EQUAL], scanner(String::from(">>=")));
    assert_eq!(vec![TT::GREATER, TT::GREATER_EQUAL], scanner(String::from("\r\n\t>>=")));
}

#[test]
fn test_identifiers() {
    type TT = TokenType;
    assert_eq!(vec![TT::STRING(String::from("Cat")), TT::STRING(String::from("HAT"))], scanner(String::from("\"Cat\" \"HAT\"")));
    assert_eq!(vec![TT::STRING(String::from("Helloßßßßß"))], scanner(String::from("\"Helloßßßßß\"")));
    assert_eq!(vec![TT::STRING(String::from("Hello")), TT::GREATER], scanner(String::from("\"Hello\" >")));
    assert_eq!(vec![TT::STRING(String::from(""))], scanner(String::from("   \t\r\n\"\" ")));
}

#[test]
fn test_numbers() {
    type TT = TokenType;
    assert_eq!(vec![TT::NUMBER(123.0)], scanner(String::from("123")));
    assert_eq!(vec![TT::NUMBER(123.34)], scanner(String::from("123.34")));
    assert_eq!(vec![TT::MINUS, TT::NUMBER(123.0)], scanner(String::from("-123.0")));
    assert_eq!(vec![TT::NUMBER(0.0)], scanner(String::from("0.0")));
}