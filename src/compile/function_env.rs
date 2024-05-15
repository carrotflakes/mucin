use crate::{ast, string::Str};

pub(super) struct FunctionEnv {
    envs: Vec<Vec<(Str, bool)>>, // bool is not eternal
    in_closure: bool,
}

impl FunctionEnv {
    pub fn from_function(function: &ast::Function) -> Self {
        let mut env = Self {
            envs: vec![
                vec![],
                function
                    .args
                    .iter()
                    .map(|arg| (arg.clone(), false))
                    .collect(),
            ],
            in_closure: false,
        };
        env.expression(&function.body);
        env
    }

    pub fn from_block(block: &ast::Block) -> Self {
        let mut env = Self {
            envs: vec![vec![], vec![]],
            in_closure: false,
        };
        env.block(block);
        env
    }

    pub fn into_outer_env(mut self) -> Vec<Str> {
        self.envs
            .remove(0)
            .into_iter()
            .map(|(name, _)| name)
            .collect()
    }

    pub fn has_eternal_variable(&self) -> bool {
        let current_env = self.envs.last().unwrap();
        current_env.iter().any(|(_, eternal)| *eternal)
    }

    fn add_variable(&mut self, name: Str) {
        if name.starts_with("__") {
            // ignore internal variables
            return;
        }
        if let Some(s) = self
            .envs
            .iter_mut()
            .rev()
            .find_map(|env| env.iter_mut().find(|(n, _)| *n == name))
        {
            if self.in_closure {
                // Mark the variable as eternal
                s.1 = true;
            }
        } else {
            self.envs.first_mut().unwrap().push((name, false));
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
                self.envs.push(vec![]);
                self.block(block);
                self.envs.pop();
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
                let in_closuer = self.in_closure;
                self.in_closure = true;
                self.envs.push(
                    function
                        .args
                        .iter()
                        .map(|arg| (arg.clone(), false))
                        .collect(),
                );
                self.expression(&function.body);
                self.envs.pop();
                self.in_closure = in_closuer;
            }
            ast::Expression::Env(expr) => {
                self.envs.push(vec![]);
                self.expression(expr);
                self.envs.pop();
            }
            ast::Expression::StaticNativeFn { native_fn: _ } => {}
        }
    }

    fn block(&mut self, block: &ast::Block) {
        for statement in &block.statements {
            match statement {
                ast::Statement::Let {
                    name,
                    mutable: _,
                    expr,
                } => {
                    self.expression(expr);
                    self.envs.last_mut().unwrap().push((name.clone(), false));
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
    }

    fn pattern(&mut self, pattern: &ast::Pattern) {
        match pattern {
            ast::Pattern::Wildcard => {}
            ast::Pattern::Variable { name, type_: _ } => {
                self.envs.last_mut().unwrap().push((name.clone(), false));
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
                self.envs
                    .last_mut()
                    .unwrap()
                    .push((constructor.clone(), false));
                for (_, value) in fields {
                    self.pattern(value);
                }
            }
        }
    }
}
