use std::collections::HashMap;
use std::num::ParseFloatError;

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenType> = {
        let mut map: HashMap<String, TokenType> = HashMap::default();
        map.insert(String::from("and"), TokenType::AND);
        map.insert(String::from("else"), TokenType::ELSE);
        map.insert(String::from("false"), TokenType::FALSE);
        map.insert(String::from("for"), TokenType::FOR);
        map.insert(String::from("fun"), TokenType::FUN);
        map.insert(String::from("if"), TokenType::IF);
        map.insert(String::from("nil"), TokenType::NIL);
        map.insert(String::from("or"), TokenType::OR);
        map.insert(String::from("print"), TokenType::PRINT);
        map.insert(String::from("return"), TokenType::RETURN);
        map.insert(String::from("super"), TokenType::SUPER);
        map.insert(String::from("this"), TokenType::THIS);
        map.insert(String::from("true"), TokenType::TRUE);
        map.insert(String::from("var"), TokenType::VAR);
        map.insert(String::from("while"), TokenType::WHILE);
        map
    };
}

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
    IDENTIFIER,
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
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
}

#[derive(Debug, Clone)]
pub struct Token {
    token: TokenType,
    lexeme: String,
    line: usize,
}

type ScannerResult = Result<Vec<Token>, (String, usize)>;

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

pub fn scanner(src: String) -> ScannerResult {
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
    fn string(&mut self) -> Result<(), String> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' { self.line += 1; }
            self.advance();
        }
        if self.is_at_end() {
            let partial_str = self.src.chars().skip(self.start).take(self.src.len()-self.start).collect::<String>();
            return Err(format!("Unterminated String: {}", partial_str));
        }
        self.advance();
        self.add_token(
            TokenType::STRING(
                self.src.chars().skip(self.start + 1).take(self.current - self.start - 2).collect::<String>()));
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
        self.add_token(TokenType::NUMBER(float));
        Ok(())
    }
    fn identifier(&mut self) -> Result<(), String> {
        while self.peek().is_alphanumeric() { self.advance(); }
        let ident = self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>();
        let keyword = match KEYWORDS.get(&ident) {
            None => {
                return Err(format!("Expected a reserved word, found: {}", ident));
            }
            Some(k) => k,
        };
        self.add_token(keyword.clone());
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
        Ok(self.tokens.clone())
    }
}

// Unwraps scanner result for easy tests
fn test_scanner(src: String) -> Vec<Token> {
    let mut scanner = Scanner::new(src);
    scanner.scan_tokens().unwrap()
}


#[test]
fn test_basic_lexemes() {
    type TT = TokenType;
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
    type TT = TokenType;
    assert_eq!(vec![TT::STRING(String::from("Cat")), TT::STRING(String::from("HAT"))], test_scanner(String::from("\"Cat\" \"HAT\"")));
    assert_eq!(vec![TT::STRING(String::from("Helloßßßßß"))], test_scanner(String::from("\"Helloßßßßß\"")));
    assert_eq!(vec![TT::STRING(String::from("Hello")), TT::GREATER], test_scanner(String::from("\"Hello\" >")));
    assert_eq!(vec![TT::STRING(String::from(""))], test_scanner(String::from("   \t\r\n\"\" ")));
}

#[test]
fn test_numbers() {
    type TT = TokenType;
    assert_eq!(vec![TT::NUMBER(123.0)], test_scanner(String::from("123")));
    assert_eq!(vec![TT::NUMBER(123.34)], test_scanner(String::from("123.34")));
    assert_eq!(vec![TT::MINUS, TT::NUMBER(123.0)], test_scanner(String::from("-123.0")));
    assert_eq!(vec![TT::NUMBER(0.0)], test_scanner(String::from("0.0")));
}

#[test]
fn test_identifiers() {
    type TT = TokenType;
    assert_eq!(vec![TT::AND, TT::OR, TT::TRUE, TT::FALSE], test_scanner(String::from("and or true\n false")));
    assert_eq!(vec![TT::NIL, TT::PRINT, TT::RETURN, TT::WHILE], test_scanner(String::from("nil print return while")));
}