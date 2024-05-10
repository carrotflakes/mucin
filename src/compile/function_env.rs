use crate::{model, string::Str};

pub(super) struct FunctionEnv {
    outer_variables: Vec<Str>,
    inner_variables: Vec<Str>,
}

impl FunctionEnv {
    pub fn from_function(function: &model::Function) -> Vec<Str> {
        let mut env = Self {
            outer_variables: Vec::new(),
            inner_variables: function.args.iter().map(|arg| arg.clone()).collect(),
        };
        env.expression(&function.body);
        env.outer_variables
    }

    fn add_variable(&mut self, name: Str) {
        if name.starts_with("__") {
            // ignore internal variables
            return;
        }
        if self.inner_variables.contains(&name) || self.outer_variables.contains(&name) {
            return;
        }
        self.outer_variables.push(name);
    }

    fn block(&mut self, block: &model::Block) {
        for statement in &block.statements {
            match statement {
                model::Statement::Let {
                    name,
                    mutable: _,
                    expr,
                } => {
                    self.expression(expr);
                    self.inner_variables.push(name.clone());
                }
                model::Statement::Expression { expr } => {
                    self.expression(expr);
                }
                model::Statement::Assign { name, expr, op: _ } => {
                    self.add_variable(name.clone());
                    self.expression(expr);
                }
                model::Statement::FieldAssign {
                    dict,
                    field,
                    expr,
                    op: _,
                } => {
                    self.expression(dict);
                    self.expression(field);
                    self.expression(expr);
                }
            }
        }
        if let Some(expr) = &block.expr {
            self.expression(expr);
        }
    }

    fn expression(&mut self, expression: &model::Expression) {
        match expression {
            model::Expression::Op { name: _, args } => {
                for arg in args {
                    self.expression(arg);
                }
            }
            model::Expression::Call { callee, args } => {
                self.expression(callee);
                for append in args {
                    match append {
                        model::VecAppend::Element(expr) => {
                            self.expression(expr);
                        }
                        model::VecAppend::Spread(expr) => {
                            self.expression(expr);
                        }
                    }
                }
            }
            model::Expression::Literal { value: _ } => {}
            model::Expression::Vec { appends } => {
                for append in appends {
                    match append {
                        model::VecAppend::Element(expr) => {
                            self.expression(expr);
                        }
                        model::VecAppend::Spread(expr) => {
                            self.expression(expr);
                        }
                    }
                }
            }
            model::Expression::Dict { appends } => {
                for append in appends {
                    match append {
                        model::DictAppend::Field(_, expr) => {
                            self.expression(expr);
                        }
                        model::DictAppend::Spread(expr) => {
                            self.expression(expr);
                        }
                    }
                }
            }
            model::Expression::Struct {
                constructor,
                appends,
            } => {
                self.expression(constructor);
                for append in appends {
                    match append {
                        model::DictAppend::Field(_, expr) => {
                            self.expression(expr);
                        }
                        model::DictAppend::Spread(expr) => {
                            self.expression(expr);
                        }
                    }
                }
            }
            model::Expression::Tuple { exprs } => {
                for expr in exprs {
                    self.expression(expr);
                }
            }
            model::Expression::Variable { name } => {
                self.add_variable(name.clone());
            }
            model::Expression::If {
                condition,
                then,
                else_,
            } => {
                self.expression(condition);
                let len = self.inner_variables.len();
                self.expression(then);
                self.inner_variables.truncate(len);
                if let Some(else_) = else_ {
                    self.expression(else_);
                    self.inner_variables.truncate(len);
                }
            }
            model::Expression::Loop { body } => {
                self.expression(body);
            }
            model::Expression::Labeled { label: _, body } => {
                let len = self.inner_variables.len();
                self.expression(body);
                self.inner_variables.truncate(len);
            }
            model::Expression::Block(block) => {
                let len = self.inner_variables.len();
                self.block(block);
                self.inner_variables.truncate(len);
            }
            model::Expression::Match { expr, arms } => {
                self.expression(expr);
                let len = self.inner_variables.len();
                for (pat, expr) in arms {
                    self.pattern(pat);
                    self.expression(expr);
                    self.inner_variables.truncate(len);
                }
            }
            model::Expression::Return { expr } => {
                if let Some(expr) = expr {
                    self.expression(expr);
                }
            }
            model::Expression::Break { label: _, expr } => {
                if let Some(expr) = expr {
                    self.expression(expr);
                }
            }
            model::Expression::Continue { label: _ } => {}
            model::Expression::Closure(function) => {
                let len = self.inner_variables.len();
                self.inner_variables.extend(function.args.iter().cloned());
                self.expression(&function.body);
                self.inner_variables.truncate(len);
            }
            model::Expression::StaticNativeFn { native_fn: _ } => {}
        }
    }

    fn pattern(&mut self, pattern: &model::Pattern) {
        match pattern {
            model::Pattern::Wildcard => {}
            model::Pattern::Variable { name, type_: _ } => {
                self.inner_variables.push(name.clone());
            }
            model::Pattern::Literal(_) => {}
            model::Pattern::Vec {
                values,
                allow_tail: _,
            } => {
                for value in values {
                    self.pattern(value);
                }
            }
            model::Pattern::Dict(fields) => {
                for (_, value) in fields {
                    self.pattern(value);
                }
            }
            model::Pattern::Struct {
                constructor,
                fields,
            } => {
                self.inner_variables.push(constructor.clone());
                for (_, value) in fields {
                    self.pattern(value);
                }
            }
        }
    }
}
