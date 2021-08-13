use crate::scanner::Token;
use crate::ast::{Unary, Literal, Grouping, Binary};
use proc_macro::bridge::server::Group;

type Tokens = Vec<Token>;

pub fn parser(tokens: Tokens) {
    let mut Parser = Parser::new(tokens);
}

struct Parser {
    tokens: Tokens,
    current: usize,
}

impl Parser {
    fn new(tokens: Tokens) -> Parser {
        Parser { tokens, current: 0 }
    }

    fn literal(&mut self) -> Literal {
        todo!()
    }

    fn unary(&mut self) -> Unary {
        todo!()
    }

    fn binary(&mut self) -> Binary {
        todo!()
    }

    fn grouping(&mut self) -> Grouping {
        todo!()
    }

    fn equality(&mut self) -> Equality {

    }

}