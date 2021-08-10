#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(non_camel_case_types)]

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum TokenTypes {
    PLUS,
    MINUS,
    MULT,
    DIV,
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
    token: TokenTypes,
    lexeme: String,
    line: usize,
}

impl Token {
    fn new(token: TokenTypes, lexeme: String, line: usize) -> Token {
        Token { token, lexeme, line }
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
    fn add_token(&mut self, tt: TokenTypes) {
        self.tokens.push(
            Token::new(
                tt,
                self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>(),
                self.line)
        );
    }
    fn peek(&mut self) -> char {
        self.src.chars().nth(self.current).unwrap()
    }
    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenTypes::LPAREN),
            ')' => self.add_token(TokenTypes::RPAREN),
            '{' => self.add_token(TokenTypes::LBRACE),
            '}' => self.add_token(TokenTypes::RBRACE),
            ',' => self.add_token(TokenTypes::COMMA),
            '.' => self.add_token(TokenTypes::DOT),
            '-' => self.add_token(TokenTypes::MINUS),
            '+' => self.add_token(TokenTypes::PLUS),
            ';' => self.add_token(TokenTypes::SEMICOLON),
            '*' => self.add_token(TokenTypes::MULT),
            '!' => {
                let tt = match self.matches('=') {
                    true => TokenTypes::BANG_EQUAL,
                    false => TokenTypes::BANG,
                };
                self.add_token(tt);
            }
            '=' => {
                let tt = match self.matches('=') {
                    true => TokenTypes::EQUAL_EQUAL,
                    false => TokenTypes::EQUAL
                };
                self.add_token(tt)
            }
            '<' => {
                let tt = match self.matches('=') {
                    true => TokenTypes::LESS_EQUAL,
                    false => TokenTypes::LESS
                };
                self.add_token(tt)
            }
            '>' => {
                let tt = match self.matches('=') {
                    true => TokenTypes::GREATER_EQUAL,
                    false => TokenTypes::GREATER
                };
                self.add_token(tt)
            }
            '/' => {
                if self.matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenTypes::SLASH);
                }
            }
            ' ' => return,
            '\r' => return,
            '\t' => return,
            '\n' => self.line += 1,
            _ => {
                eprintln!("Line {} unexpeted token `{}`", self.line, c);
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
fn test() {
    assert_eq!(vec![TokenTypes::EQUAL], lex(String::from("=")))
}