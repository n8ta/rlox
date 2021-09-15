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
mod environment;
mod e2e_tests;

use scanner::scanner;
use parser::parse;
use crate::interpreter::{interpret};
use crate::environment::Env;
use std::rc::Rc;
use std::cell::RefCell;
use crate::source_ref::Source;

struct Lox {
    had_error: bool,
    env: Rc<RefCell<Env>>,
    src: Rc<Source>,
}

impl Lox {
    fn new() -> Lox {
        return Lox {
            src: Rc::new(Source::new(String::new())),
            had_error: false,
            env: Rc::new(RefCell::new(Env::new(None))),
        };
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
            self.src = Rc::new(Source::new(contents));
            self.run(self.src.clone());
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
            let line = Rc::new(Source::new(line_contents));
            self.run(line);
        }
    }

    pub fn run(&mut self, src: Rc<Source>) {
        let tokens = match scanner(src.clone()) {
            Ok(t) => t,
            Err((message, line)) => {
                self.error(line, message);
                return;
            }
        };
        let ast = match parse(tokens, Rc::new(src.src.clone())) {
            Ok(ast) => ast,
            Err(err) => {
                eprintln!("{}", err);
                return;
            }
        };

        match interpret(ast, self.env.clone()) {
            Ok(_) => {}
            Err(err) =>
                eprintln!("[line:{}] Error: {}\n{}", err.context.line+1, &err.msg, err.context)
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

