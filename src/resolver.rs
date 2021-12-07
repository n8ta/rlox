use crate::parser::{ParserFunc, Stmt, ExprTy, Expr};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::source_ref::{SourceRef};
use crate::{StringInContext};

pub type ResolverResult = Result<(), ResolverError>;

pub struct ResolverError {
    #[allow(dead_code)]
    message: String,
    #[allow(dead_code)]
    source: SourceRef,
}


pub type ScopeSize = usize;

#[derive(Clone, PartialOrd, PartialEq, Debug, serde::Serialize)]
pub struct Resolved {
    #[serde(skip_serializing)]
    pub scope: usize,
    #[serde(skip_serializing)]
    pub offset: usize,
}

#[derive(Copy, Clone, Debug)]
enum ClassType {
    None,
    Class,
    SubClass,
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Resolver Error: {}\n", &self.message))?;
        f.write_str(&format!("{}", self.source))
    }
}


impl ResolverError {
    pub fn new(message: String, source: &SourceRef) -> ResolverError { ResolverError { message, source: source.clone() } }
}

pub fn resolve(prog: &mut Stmt) -> ResolverResult {
    let mut res = Resolver::new();
    res.resolve(prog)
}

#[derive(Debug, Clone)]
struct Resolver {
    scopes: Vec<HashMap<String, (usize, bool)>>,
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
    fn declare(&mut self, name: &StringInContext) {
        // // println!("Declare {} at scope {}", name, self.scopes.len() - 1);
        let last_size = self.last_size();
        if let Some(scope) = self.scopes.last_mut() {
            let size = match scope.get(&name.string) {
                None => last_size,
                Some((offset, _bool)) => *offset,
            };
            scope.insert(name.string.to_string(), (size, false));
        }
    }
    fn define(&mut self, name: &StringInContext) {
        // // println!("Define {} at scope {}", name, self.scopes.len() - 1);
        let last_size = self.last_size();
        if let Some(scope) = self.scopes.last_mut() {
            let (size, bool) = match scope.get(&name.string) {
                None => (last_size, true),
                Some((offset, _)) => (*offset, true),
            };
            scope.insert(name.string.to_string(), (size, bool));
        }
    }

    fn last_size(&self) -> usize {
        self.scopes.last().unwrap().len()
    }

    fn resolve_local(&mut self, name: &StringInContext, resolved: &mut Option<Resolved>) -> ResolverResult {
        if self.scopes.len() == 0 {
            return Ok(());
        }
        for i in (0..(self.scopes.len())).rev() {
            if let Some((offset, _defined)) = self.scopes[i].get(&name.string) {
                let res = Resolved {
                    scope: (self.scopes.len() - 1) - i,
                    offset: *offset,
                };
                // println!("Resolved {} at scope {} offset {}", name,  res.scope, res.offset);
                resolved.insert(res);
                break;
            }
        }
        if let None = resolved {
            if name.string != "clock" {
                return Err(ResolverError::new(format!("Variable {} was never defined", name.string), &name.context));
            }
        }
        Ok(())
        // // println!("Resolving {} at dist {:?}", name, expr);
    }
    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> ResolverResult {
        match stmt {
            Stmt::Expr(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Block(stmts, scope_size) => {
                self.begin_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve(stmt)?;
                }
                scope_size.insert(self.last_size());
                self.end_scope();
            }
            Stmt::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Variable(name, init, resolved, _context) => {
                self.declare(name);
                if let Some(expr) = init {
                    self.resolve_expr(expr)?;
                }
                self.define(name);
                self.resolve_local(name, resolved)?;
            }
            Stmt::If(test, if_branch, else_branch) => {
                self.resolve_expr(test)?;
                self.resolve(if_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve(else_branch)?;
                }
            }
            Stmt::While(test, body, scope_size) => {
                self.resolve_expr(test)?;

                // self.begin_scope();
                self.resolve_stmt(body)?;
                scope_size.insert(self.last_size());
                // self.end_scope();
            }
            Stmt::Function(func, resolved) => {
                self.declare(func.name());
                self.define(func.name());
                self.resolve_local(func.name(), resolved)?;
                self.resolve_func(func)?;
            }
            Stmt::Return(expr, _context) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
            }
            Stmt::Class(class, resolved, scope_size) => {
                // Make sure class isn't inheriting itself
                if let Some(super_class) = &class.inner.super_class {
                    if super_class.borrow().name == class.name().string {
                        return Err(ResolverError::new(format!("Class {} cannot inherit from itself", super_class.borrow().name), &class.context()));
                    }
                }

                let enclosing = self.current_class.clone();
                self.current_class = if let Some(_) = &class.inner.super_class {
                    ClassType::SubClass
                } else {
                    ClassType::Class
                };
                self.declare(&class.name().clone());

                if let Some(super_class_ref_cell) = &class.inner.super_class {
                    // resolved the superclass into a variable reference
                    let mut ref_mut = super_class_ref_cell.borrow_mut();
                    self.resolve_expr(&mut ref_mut.parent)?;

                    // open scope to be used only for the 'super' keyword
                    self.begin_scope();
                    self.scopes.last_mut().unwrap().insert("super".to_string(), (0, true));
                }

                // Open scope for 'this'
                self.begin_scope();
                let last = self.scopes.last_mut().unwrap();
                last.insert("this".to_string(), (last.len(), true));
                for m in class.inner.methods.borrow_mut().iter_mut() {
                    self.resolve_func(m)?;
                }
                // Close the scope for 'this'
                scope_size.insert(self.last_size());
                self.end_scope();


                // Close scope we opened for 'super' if we opened
                if let Some(_super) = &class.inner.super_class {
                    self.end_scope()
                }

                self.resolve_local(class.name(), resolved)?;
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

        let mut scope_size = func.inner.scope_size.borrow_mut();
        scope_size.insert(self.last_size());
        self.end_scope();
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut ExprTy) -> ResolverResult {
        match &mut expr.expr {
            Expr::Super(_method, resolved) => {
                if let ClassType::SubClass = self.current_class {
                    self.resolve_local(&StringInContext::new("super".to_string(), expr.context.clone()), resolved)?;
                } else {
                    return Err(ResolverError::new(format!("Cannot use super outside of a sub class"), &expr.context));
                }
            }
            Expr::This(resolved) => {
                if let ClassType::None = self.current_class {
                    return Err(ResolverError::new("Cannot use `this` outside a class".to_string(), &expr.context));
                }
                self.resolve_local(&StringInContext::new(format!("this"), expr.context.clone()), resolved)?;
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
            Expr::Variable(var) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some((_offset, defined)) = scope.get(&var.name.string) {
                        if !defined {
                            return Err(ResolverError::new("Can't read local variable in its own initializer.".to_string(), &expr.context));
                        }
                    }
                }
                self.resolve_local(&var.name, &mut var.resolved)?;
            }
            Expr::Assign(name, value, resolved) => {
                self.resolve_expr(value)?;
                self.resolve_local(name, resolved)?;
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
// // //             println!("Checking {:?}", val);
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