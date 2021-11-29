use crate::parser::{ParserFunc, Stmt, ExprTy, Expr};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::source_ref::{SourceRef};
use crate::{Callable};

pub type ResolverResult = Result<(), ResolverError>;

pub struct ResolverError {
    #[allow(dead_code)]
    message: String,
    #[allow(dead_code)]
    source: SourceRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Resolved {
    pub scope: usize,
    pub offset: usize,
}

#[derive(Copy, Clone, Debug)]
enum ClassType {
    None,
    Class,
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Resolver Error: {}", &self.message))?;
        f.write_str(&format!("{}", self.source))
    }
}


impl ResolverError {
    pub fn new(message: &str, source: &SourceRef) -> ResolverError { ResolverError { message: message.to_string(), source: source.clone() } }
}

pub fn resolve(prog: &mut Stmt) -> ResolverResult {
    let mut res = Resolver::new();
    res.resolve(prog)
}

#[derive(Debug, Clone)]
struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    current_class: ClassType,
}

impl Resolver {
    pub fn new() -> Resolver { Resolver { scopes: vec![], current_class: ClassType::None } }

    pub fn resolve(&mut self, program: &mut Stmt) -> ResolverResult {
        self.resolve_stmt(program)?;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
    fn declare(&mut self, name: &str) {
        // println!("Declare {} at scope {}", name, self.scopes.len() - 1);
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), false);
        }
    }
    fn define(&mut self, name: &str) {
        // println!("Define {} at scope {}", name, self.scopes.len() - 1);
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), true);
        }
    }

    fn resolve_local(&mut self, name: &str, resolved: &mut Option<Resolved>) {
        let mut res = Resolved {
            scope: 0,
            offset: 0
        };
        if self.scopes.len() == 0 {
            return;
        }
        for i in (0..(self.scopes.len())).rev() {
            if self.scopes[i].contains_key(name) {
                res.scope = (self.scopes.len() - 1) - i;
                res.offset = 0;
                resolved.insert(res);
                break;
            }
        }
        // println!("Resolving {} at dist {:?}", name, expr);
    }
    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> ResolverResult {
        match stmt {
            Stmt::Expr(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve(stmt)?;
                }
                self.end_scope();
            }
            Stmt::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Variable(name, init) => {
                self.declare(name);
                if let Some(expr) = init {
                    self.resolve_expr(expr)?;
                }
                self.define(name);
            }
            Stmt::If(test, if_branch, else_branch) => {
                self.resolve_expr(test)?;
                self.resolve(if_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve(else_branch)?;
                }
            }
            Stmt::While(test, body) => {
                self.resolve_expr(test)?;
                self.resolve_stmt(body)?;
            }
            Stmt::Function(func) => {
                self.declare(func.name());
                self.define(func.name());
                self.resolve_func(func)?;
            }
            Stmt::Return(expr, _context) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
            }
            Stmt::Class(class) => {
                let enclosing = self.current_class.clone();

                self.current_class = ClassType::Class;
                self.declare(class.name().clone());

                self.begin_scope();
                self.scopes.last_mut().unwrap().insert("this".to_string(), true);

                for m in class.inner.methods.borrow_mut().iter_mut() {
                    self.resolve_func(m)?;
                }
                self.end_scope();
                self.define(class.name());
                self.current_class = enclosing;
            }
        }
        Ok(())
    }
    fn resolve_func(&mut self, func: &mut ParserFunc) -> ResolverResult {
        self.begin_scope();
        for (name, _) in func.inner.args.iter() {
            self.declare(&name);
            self.define(&name);
        }
        let mut body_mut = func.inner.body.borrow_mut();
        self.resolve(&mut body_mut)?;
        self.end_scope();
        Ok(())
    }
    fn resolve_expr(&mut self, expr: &mut ExprTy) -> ResolverResult {
        match &mut expr.expr {
            Expr::This(resolved) => {
                if let ClassType::None = self.current_class {
                    return Err(ResolverError::new("Cannot use `this` outside a class", &expr.context));
                }
                self.resolve_local("this", resolved);
            }
            Expr::Binary(left, _op, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call(callee, args) => {
                self.resolve_expr(callee)?;
                for expr in args {
                    self.resolve_expr(expr)?;
                }
            }
            Expr::Grouping(group) => {
                self.resolve_expr(group)?;
            }
            Expr::Literal(_) => {}
            Expr::Unary(_op, expr) => {
                self.resolve_expr(expr)?;
            }
            Expr::Variable(var, resolved) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(bool) = scope.get(var) {
                        if !bool {
                            return Err(ResolverError::new("Can't read local variable in its own initializer.", &expr.context));
                        }
                    }
                }
                // println!("Resolve {}", var,);
                self.resolve_local(var, resolved);
            }
            Expr::Assign(name, value, resolved) => {
                self.resolve_expr(value)?;
                self.resolve_local(name, resolved);
            }
            Expr::Logical(left, _op, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Get(expr, _getter_name) => {
                self.resolve_expr(expr)?;
            }
            Expr::Set(left, _field, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
        }
        Ok(())
    }
}

// #[test]
// fn resolver_basic() {
//     use std::rc::Rc;
//     use crate::scanner::scanner;
//     use crate::parser::parse;
//     // var name = true;
//     // print name;
//     let src = "fun test() {\n
//         var x = 1;\n\
//         print x;
//     }\n".to_string();
//     let src = Rc::new(Source::new(src));
//     let mut stmts: Vec<Stmt> = parse(scanner(src.clone()).unwrap(), src.clone()).unwrap();
//     match resolve(&mut stmts) {
//         Err(_) => panic!("failed!"),
//         _ => {}
//     }
//
//     if let Stmt::Function(func) = &stmts[0] {
//         if let Stmt::Print(val) = &func.body[1] {
// //             println!("Checking {:?}", val);
//             assert!(val.scope.is_some(), "resolver should assign a scope to a stack variable");
//         } else {
//             assert!(false)
//         }
//     } else {
//         assert!(false)
//     }
// }
//
// #[test]
// fn resolver_recursive() {
//     use std::rc::Rc;
//     use crate::scanner::scanner;
//     use crate::parser::parse;
//     let src = "fun rec() {\nprint rec();\n}".to_string();
//     let src = Rc::new(Source::new(src));
//     let mut stmts: Vec<Stmt> = parse(scanner(src.clone()).unwrap(), src.clone()).unwrap();
//     if let Stmt::Function(func) = &stmts[0] {
//         if let Stmt::Print(expr) = &func.body[0] {
//             if let Expr::Call(callee, args) = &expr.expr {
//                 assert!(callee.scope.is_none(), "None because it should be in the global scope");
//             } else {
//                 assert!(false, "should be a call inside print");
//             }
//         } else {
//             assert!(false, "should be a print stmt");
//         }
//     } else {
//         assert!(false, "should be a function")
//     }
// }