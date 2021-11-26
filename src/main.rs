use std::{env, io};
use std::process::exit;
use std::fs::read_to_string;
use std::io::{BufReader, BufRead};

mod scanner;
mod parser;
mod source_ref;
mod e2e_tests;
mod resolver;
mod runtime;

use scanner::scanner;
use crate::parser::parse;
use crate::runtime::interpreter::{interpret, RuntimeException, LoxControlFlow};
use crate::runtime::environment::Env;
use std::rc::Rc;
use crate::source_ref::{Source, SourceRef};
use crate::runtime::{Callable, Value};
use std::time::{SystemTime, UNIX_EPOCH};
use crate::resolver::{resolve};


#[derive(Clone, Debug)]
struct ClockRuntimeFunc {}

impl Callable for ClockRuntimeFunc {
    fn arity(&self) -> u8 {
        0
    }
    fn call(&self, _args: Vec<Value>, context: SourceRef) -> Result<Value, RuntimeException> {
        let t = match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(t) => t,
            Err(_) => return Err(RuntimeException::new(format!("Unable to determine system time."), context))
        };
        Ok(Value::NUMBER(t.as_secs_f64() * 1000.0))
    }
    fn name(&self) -> &str { "clock" }
}

struct Lox {
    had_error: bool,
    env: Env,
    globals: Env,
    src: Rc<Source>,
}

impl Lox {
    fn new() -> Lox {
        let mut globals = Env::new(None);
        let clock = Value::FUNC(Rc::new(ClockRuntimeFunc {} ));
        globals.declare("clock", &clock);

        return Lox {
            src: Rc::new(Source::new(String::new())),
            had_error: false,
            globals: globals.clone(),
            env: Env::new(Some(globals))
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
        let mut ast = match parse(tokens, src.clone()) {
            Ok(ast) => ast,
            Err(err) => {
                eprintln!("[line:{}] Error: {}\n{}", err.context.line+1, &err.msg, err.context);
                return;
            }
        };

        match resolve(&mut ast) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("{}",err);
                return;
            }
        }

        match interpret(&ast, self.globals.clone(), self.globals.clone()) {
            Ok(_) => {}
            Err(err) => match err {
                LoxControlFlow::CFRuntime(err) => {
                    eprintln!("[line:{}] Error: {}\n{}", err.context.line+1, &err.msg, err.context)
                }
                LoxControlFlow::CFReturn(value, context) => {
                    eprintln!("[line:{}] Error: no function to return from! (value was {})\n{}", context.line, value, context)
                }
            }

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

