use std::{env, io};
use std::process::exit;
use std::fs::read_to_string;
use std::io::{BufReader, BufRead};

#[macro_use(lazy_static)]
extern crate lazy_static;


mod scanner;
mod parser;
mod interpreter;
mod source_ref;

use scanner::scanner;
use parser::parse;
use crate::interpreter::interpret;

struct Lox {
    had_error: bool,
}

impl Lox {
    fn new() -> Lox {
        return Lox { had_error: false };
    }
    fn main(&mut self, args: Vec<String>) {
        if args.len() > 2 {
            eprintln!("Usage: rlox [script.lox]");
            exit(-1);
        } else if args.len() == 2 {
            self.run_file(&args[1]);
        } else {
            self.run_prompt();
        }
    }

    fn run_file(&mut self, filename: &str) {
        if let Ok(contents) = read_to_string(filename) {
            self.run(contents);
            if self.had_error {
                exit(-1);
            }
        } else {
            eprintln!("Couldn't read {}", filename);
            exit(-1);
        }
    }

    fn run_prompt(&mut self) {
        let mut reader = BufReader::new(io::stdin());
        loop {
            let mut line_contents = String::new();
            reader.read_line(&mut line_contents).unwrap();
            self.run(line_contents);
        }
    }

    fn run(&mut self, src: String) {
        let tokens = match scanner(src.clone()) {
            Ok(t) => t,
            Err(( message, line)) => {
                self.error(line, message);
                return;
            }
        };
        match parse(tokens, src) {
            Ok(ast) =>  {
                println!("AST: {:?}", ast);
                println!("=> {:?}", interpret(ast));
            },
            Err(err) => eprintln!("{}", err),
        }
    }

    fn error(&mut self, line: usize, message: String) {
        self.report(line, String::from(""), message);
    }

    fn report(&mut self, line: usize, src: String, message: String) {
        eprintln!("[line {}] Error {}: {}", line, src, message);
        self.had_error = true;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    lox.main(args);
}

