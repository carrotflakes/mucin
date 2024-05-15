use crate::{ast, string::Str};

pub(super) struct FunctionEnv {
    envs: Vec<Vec<Str>>,
}

impl FunctionEnv {
    pub fn from_function(function: &ast::Function) -> Vec<Str> {
        let mut env = Self {
            envs: vec![
                vec![],
                function.args.iter().map(|arg| arg.clone()).collect(),
            ],
        };
        env.expression(&function.body);
        env.envs.remove(0)
    }

    fn add_variable(&mut self, name: Str) {
        if name.starts_with("__") {
            // ignore internal variables
            return;
        }
        if !self.envs.iter().any(|env| env.contains(&name)) {
            self.envs.first_mut().unwrap().push(name);
        }
    }

    fn expression(&mut self, expression: &ast::Expression) {
        match expression {
            ast::Expression::Op { name: _, args } => {
                for arg in args {
                    self.expression(arg);
                }
            }
            ast::Expression::Call { callee, args } => {
                self.expression(callee);
                for append in args {
                    match append {
                        ast::VecAppend::Element(expr) => {
                            self.expression(expr);
                        }
                        ast::VecAppend::Spread(expr) => {
                            self.expression(expr);
                        }
                    }
                }
            }
            ast::Expression::Literal { value: _ } => {}
            ast::Expression::Vec { appends } => {
                for append in appends {
                    match append {
                        ast::VecAppend::Element(expr) => {
                            self.expression(expr);
                        }
                        ast::VecAppend::Spread(expr) => {
                            self.expression(expr);
                        }
                    }
                }
            }
            ast::Expression::Dict { appends } => {
                for append in appends {
                    match append {
                        ast::DictAppend::Field(_, expr) => {
                            self.expression(expr);
                        }
                        ast::DictAppend::Spread(expr) => {
                            self.expression(expr);
                        }
                    }
                }
            }
            ast::Expression::Struct {
                constructor,
                appends,
            } => {
                self.expression(constructor);
                for append in appends {
                    match append {
                        ast::DictAppend::Field(_, expr) => {
                            self.expression(expr);
                        }
                        ast::DictAppend::Spread(expr) => {
                            self.expression(expr);
                        }
                    }
                }
            }
            ast::Expression::Variable { name } => {
                self.add_variable(name.clone());
            }
            ast::Expression::If {
                condition,
                then,
                else_,
            } => {
                self.expression(condition);
                self.expression(then);
                if let Some(else_) = else_ {
                    self.expression(else_);
                }
            }
            ast::Expression::Loop { body } => {
                self.expression(body);
            }
            ast::Expression::Labeled { label: _, body } => {
                self.expression(body);
            }
            ast::Expression::Block(block) => {
                self.block(block);
            }
            ast::Expression::Match { expr, arms } => {
                self.expression(expr);
                for (pat, expr) in arms {
                    self.envs.push(vec![]);
                    self.pattern(pat);
                    self.expression(expr);
                    self.envs.pop();
                }
            }
            ast::Expression::Return { expr } => {
                if let Some(expr) = expr {
                    self.expression(expr);
                }
            }
            ast::Expression::Break { label: _, expr } => {
                if let Some(expr) = expr {
                    self.expression(expr);
                }
            }
            ast::Expression::Continue { label: _ } => {}
            ast::Expression::Closure(function) => {
                self.envs
                    .push(function.args.iter().map(|arg| arg.clone()).collect());
                self.expression(&function.body);
                self.envs.pop();
            }
            ast::Expression::StaticNativeFn { native_fn: _ } => {}
        }
    }

    fn block(&mut self, block: &ast::Block) {
        self.envs.push(vec![]);
        for statement in &block.statements {
            match statement {
                ast::Statement::Let {
                    name,
                    mutable: _,
                    expr,
                } => {
                    self.expression(expr);
                    self.envs.last_mut().unwrap().push(name.clone());
                }
                ast::Statement::Expression { expr } => {
                    self.expression(expr);
                }
                ast::Statement::Assign { name, expr, op: _ } => {
                    self.add_variable(name.clone());
                    self.expression(expr);
                }
                ast::Statement::FieldAssign {
                    dict,
                    field,
                    expr,
                    op: _,
                } => {
                    self.expression(dict);
                    self.expression(field);
                    self.expression(expr);
                }
                ast::Statement::Defer { expr } => self.expression(expr),
            }
        }
        if let Some(expr) = &block.expr {
            self.expression(expr);
        }
        self.envs.pop();
    }

    fn pattern(&mut self, pattern: &ast::Pattern) {
        match pattern {
            ast::Pattern::Wildcard => {}
            ast::Pattern::Variable { name, type_: _ } => {
                self.envs.last_mut().unwrap().push(name.clone());
            }
            ast::Pattern::Literal(_) => {}
            ast::Pattern::Vec {
                values,
                allow_tail: _,
            } => {
                for value in values {
                    self.pattern(value);
                }
            }
            ast::Pattern::Dict(fields) => {
                for (_, value) in fields {
                    self.pattern(value);
                }
            }
            ast::Pattern::Struct {
                constructor,
                fields,
            } => {
                self.envs.last_mut().unwrap().push(constructor.clone());
                for (_, value) in fields {
                    self.pattern(value);
                }
            }
        }
    }
}
