use gc_arena::{lock::RefLock, Mutation};

use crate::{
    native_fns::{NF_FIELD_ASSIGN, NF_INDEX, NF_STRUCT_TYPE_METHODS},
    string::{intern, Str},
    value::{Closure, Dict},
};

use self::pattern_match::convert_match;

use super::*;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    String(String),
    Return,
    Break,
}

pub struct Builder<'gc> {
    mc: &'gc Mutation<'gc>,
    method_call_fn: Option<Closure<'gc>>,
    envs: Vec<Vec<(Str, bool)>>,
    instructions: Vec<Instruction<'gc>>,
    stack_size: usize,
    labels: Vec<Label>,
    break_indexes: Vec<(Str, usize)>,
    instructions_offset: usize,
}

#[derive(Debug, Clone)]
struct Label {
    name: Str,
    continue_index: usize,
    stack_size: usize,
}

impl<'gc> Builder<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>) -> Self {
        Self {
            mc,
            method_call_fn: None,
            envs: vec![],
            instructions: vec![],
            stack_size: 0,
            labels: vec![],
            break_indexes: vec![],
            instructions_offset: 0,
        }
    }

    pub fn use_method_call_fn(mut self, method_call_fn: Option<Closure<'gc>>) -> Self {
        self.method_call_fn = method_call_fn;
        self
    }

    pub fn wrap_env(mut self, env: Vec<(Str, bool)>) -> Self {
        self.envs.push(env);
        self
    }

    pub fn wrap_envs(mut self, envs: Vec<Vec<(Str, bool)>>) -> Self {
        self.envs.extend(envs);
        self
    }

    fn branch(&self) -> Self {
        Self {
            mc: self.mc,
            method_call_fn: self.method_call_fn.clone(),
            envs: self.envs.clone(),
            instructions: vec![],
            stack_size: self.stack_size,
            labels: self.labels.clone(),
            break_indexes: vec![],
            instructions_offset: self.instructions_offset + self.instructions.len(),
        }
    }

    pub fn into_instructions(self) -> Vec<Instruction<'gc>> {
        self.instructions
    }

    pub fn build_program(
        &mut self,
        definitions: &[model::Definition],
    ) -> std::result::Result<Vec<(Str, bool)>, String> {
        let mut names = Vec::new();
        for definition in definitions {
            match definition {
                model::Definition::Function(function) => {
                    names.push((function.name.clone(), true));
                }
                model::Definition::Method { .. } => {}
                model::Definition::Variable {
                    name,
                    mutable,
                    expr: _,
                } => {
                    names.push((name.clone(), *mutable));
                }
                model::Definition::Struct { name, fields: _ } => {
                    names.push((name.clone(), false));
                }
                model::Definition::Module(mod_name) => {
                    names.push((mod_name.clone(), false));
                }
            }
        }
        self.envs.push(names.clone());

        for definition in definitions {
            match definition {
                model::Definition::Function(function) => {
                    let f = Gc::new(self.mc, self.build_function(function)?);
                    let index = self.resolve_variable(&function.name).unwrap();
                    self.instructions.extend([
                        Instruction::MakeClosure(f),
                        Instruction::Store(index.0 as u16, index.1 as u16),
                    ]);
                }
                model::Definition::Method { receiver, function } => {
                    let f = Gc::new(self.mc, self.build_function(function)?);
                    self.build_expression(receiver).unwrap();
                    self.instructions
                        .push(Instruction::CallNative(&NF_STRUCT_TYPE_METHODS));
                    self.instructions
                        .push(Instruction::Push(Box::new(Value::String(
                            function.name.clone(),
                        ))));
                    self.instructions.push(Instruction::MakeClosure(f));
                    self.instructions
                        .push(Instruction::CallNative(&NF_FIELD_ASSIGN));
                    self.instructions.push(Instruction::Pop);
                }
                model::Definition::Variable {
                    name,
                    mutable: _,
                    expr,
                } => {
                    match self.build_expression(expr) {
                        Ok(()) => {}
                        Err(Error::Return) => {
                            return Err("unexpected return in variable definition".to_owned());
                        }
                        Err(Error::Break) => {
                            return Err("unexpected break in variable definition".to_owned());
                        }
                        Err(Error::String(err)) => return Err(err),
                    }
                    let index = self.resolve_variable(name).unwrap();
                    self.instructions
                        .push(Instruction::Store(index.0 as u16, index.1 as u16));
                }
                model::Definition::Struct { name, fields } => {
                    let index = self.resolve_variable(name).unwrap();
                    self.instructions.extend([
                        Instruction::Push(Box::new(Value::StructType(Gc::new(
                            self.mc,
                            StructType {
                                name: name.clone(),
                                fields: fields.clone().into_boxed_slice(),
                                methods: Gc::new(self.mc, RefLock::new(Dict::default())),
                            },
                        )))),
                        Instruction::Store(index.0 as u16, index.1 as u16),
                    ]);
                }
                model::Definition::Module(name) => {
                    let index = self.resolve_variable(name).unwrap();
                    self.instructions.extend([
                        Instruction::Push(Box::new(Value::Unit)),
                        Instruction::Store(index.0 as u16, index.1 as u16),
                    ]);
                }
            }
        }
        self.instructions.push(Instruction::PushUnit);
        self.instructions.push(Instruction::Return);

        Ok(names)
    }

    #[must_use]
    pub fn build_function(
        &mut self,
        function: &model::Function,
    ) -> std::result::Result<Function<'gc>, String> {
        let variables = super::function_env::FunctionEnv::from_function(function);

        let mut capture_envs: Vec<_> = variables
            .iter()
            .map(|name| {
                self.resolve_variable(name)
                    .map(|(i1, _, _)| i1)
                    .ok_or_else(|| format!("variable not found: {}", name))
            })
            .collect::<std::result::Result<_, _>>()?;
        capture_envs.sort_unstable();
        capture_envs.dedup();
        let mut builder = Builder {
            mc: self.mc,
            method_call_fn: self.method_call_fn.clone(),
            envs: capture_envs.iter().map(|i| self.envs[*i].clone()).collect(),
            instructions: Vec::new(),
            stack_size: 0,
            labels: vec![],
            break_indexes: vec![],
            instructions_offset: 0,
        };

        builder.envs.push(
            function
                .args
                .iter()
                .map(|arg| (arg.clone(), true))
                .collect(),
        );

        match builder.build_expression(&function.body) {
            Ok(()) => {
                builder.instructions.push(Instruction::Return);
            }
            Err(Error::Return) => {}
            Err(Error::Break) => panic!("unexpected break in function definition"),
            Err(Error::String(err)) => return Err(err),
        }

        let frame = builder.envs.pop().unwrap();
        let frame = Gc::new(
            self.mc,
            StructType {
                name: function.name.clone(),
                fields: frame.into_boxed_slice(),
                methods: Gc::new(self.mc, RefLock::new(Dict::default())),
            },
        );

        Ok(Function {
            name: function.name.clone(),
            arity: function.args.len(),
            frame,
            body: builder.instructions,
            capture_envs,
        })
    }

    #[must_use]
    fn build_block(&mut self, block: &model::Block) -> Result<()> {
        let ss = self.stack_size;

        for statement in &block.statements {
            match statement {
                model::Statement::Let {
                    name,
                    mutable,
                    expr,
                } => {
                    self.build_expression(expr)?;
                    self.add_var(name.clone(), *mutable);
                    let index = self.resolve_variable(name).unwrap();
                    self.instructions
                        .push(Instruction::Store(index.0 as u16, index.1 as u16));
                    self.stack_size -= 1;
                }
                model::Statement::Expression { expr } => {
                    self.build_expression(expr)?;
                    self.instructions.push(Instruction::Pop);
                    self.stack_size -= 1;
                }
                model::Statement::Assign { name, expr, op } => {
                    let index = self.resolve_variable(name).unwrap();
                    if !index.2 {
                        return Err(Error::String(format!("variable is not mutable: {}", name)));
                    }

                    if let Some(op) = op {
                        // Assign with operation
                        self.instructions
                            .push(Instruction::Load(index.0 as u16, index.1 as u16));
                        self.stack_size += 1;
                        self.build_expression(expr)?;
                        match op.as_str() {
                            "__add" => self.instructions.push(Instruction::Add),
                            "__sub" => self.instructions.push(Instruction::Sub),
                            "__mul" => self.instructions.push(Instruction::Mul),
                            "__div" => self.instructions.push(Instruction::Div),
                            "__rem" => self.instructions.push(Instruction::Rem),
                            _ => unreachable!(),
                        }
                        self.stack_size -= 1;
                    } else {
                        self.build_expression(expr)?;
                    }

                    self.instructions
                        .push(Instruction::Store(index.0 as u16, index.1 as u16));
                    self.stack_size -= 1;
                }
                model::Statement::FieldAssign {
                    dict,
                    field,
                    expr,
                    op,
                } => {
                    self.build_expression(dict)?;

                    if let Some(op) = op {
                        // Assign with operation
                        self.instructions.push(Instruction::Dup);
                        self.stack_size += 1;
                        self.build_expression(field)?;
                        self.instructions.push(Instruction::Dup);
                        let index = self.add_var(intern(""), false);
                        self.instructions.push(Instruction::Store(index.0, index.1));
                        self.instructions.push(Instruction::CallNative(&NF_INDEX));
                        self.stack_size -= 1;

                        self.build_expression(expr)?;
                        match op.as_str() {
                            "__add" => self.instructions.push(Instruction::Add),
                            "__sub" => self.instructions.push(Instruction::Sub),
                            "__mul" => self.instructions.push(Instruction::Mul),
                            "__div" => self.instructions.push(Instruction::Div),
                            "__rem" => self.instructions.push(Instruction::Rem),
                            _ => unreachable!(),
                        }

                        self.instructions.push(Instruction::Load(index.0, index.1));
                        self.instructions.push(Instruction::Swap);
                    } else {
                        self.build_expression(field)?;
                        self.build_expression(expr)?;
                    }

                    self.instructions
                        .push(Instruction::CallNative(&NF_FIELD_ASSIGN));
                    self.instructions.push(Instruction::Pop);
                    self.stack_size -= 3;
                }
            }

            assert_eq!(ss, self.stack_size);
        }

        if let Some(expr) = &block.expr {
            self.build_expression(expr)
        } else {
            self.instructions.push(Instruction::PushUnit);
            self.stack_size += 1;
            Ok(())
        }
    }

    #[must_use]
    fn build_expression(&mut self, expression: &model::Expression) -> Result<()> {
        match expression {
            model::Expression::Op { name, args } => {
                if let Some((inst, arity, flip)) = match name.as_str() {
                    "__add" => Some((Instruction::Add, 2, false)),
                    "__sub" => Some((Instruction::Sub, 2, false)),
                    "__mul" => Some((Instruction::Mul, 2, false)),
                    "__div" => Some((Instruction::Div, 2, false)),
                    "__rem" => Some((Instruction::Rem, 2, false)),
                    "__neg" => Some((Instruction::Neg, 1, false)),
                    "__eq" => Some((Instruction::Eq, 2, false)),
                    "__ne" => Some((Instruction::Ne, 2, false)),
                    "__gt" => Some((Instruction::Gt, 2, false)),
                    "__ge" => Some((Instruction::Ge, 2, false)),
                    "__lt" => Some((Instruction::Gt, 2, true)),
                    "__le" => Some((Instruction::Ge, 2, true)),
                    "__not" => Some((Instruction::Not, 1, false)),
                    _ => None,
                } {
                    assert_eq!(args.len(), arity);
                    if flip {
                        for arg in args.iter().rev() {
                            self.build_expression(arg)?;
                        }
                    } else {
                        for arg in args {
                            self.build_expression(arg)?;
                        }
                    }
                    self.instructions.push(inst);
                    self.stack_size -= arity - 1;
                    return Ok(());
                }

                if name.as_str() == "__or" || name.as_str() == "__and" {
                    self.build_expression(&args[0])?;
                    let stack_size = self.stack_size;

                    self.instructions.push(Instruction::Dup);
                    let jump_index = self.instructions.len();
                    self.instructions.push(Instruction::PushUnit);
                    self.instructions.push(Instruction::Pop);

                    let env_len = self.envs.last().unwrap().len();
                    match self.build_expression(&args[1]) {
                        Ok(()) => {}
                        Err(Error::Break) => {}
                        Err(Error::Return) => {}
                        err @ Err(Error::String(_)) => return err,
                    }

                    self.instructions[jump_index] = if name.as_str() == "__or" {
                        Instruction::JumpIf(self.instructions_offset + self.instructions.len())
                    } else {
                        Instruction::JumpIfNot(self.instructions_offset + self.instructions.len())
                    };

                    self.stack_size = stack_size;
                    self.envs.last_mut().unwrap()[env_len..].fill_with(|| (intern(""), false));

                    return Ok(());
                }

                for arg in args {
                    self.build_expression(arg)?;
                }

                if name.as_str() == "__index" {
                    self.instructions.push(Instruction::CallNative(&NF_INDEX));
                    self.stack_size -= args.len() - 1;
                    return Ok(());
                }

                unreachable!("unknown operator: {}", name);
            }
            model::Expression::Call { callee, args } => {
                let ss = self.stack_size;
                if let model::Expression::Op { name, args: args2 } = callee.as_ref() {
                    if name.as_str() == "__index" {
                        if let Some(method_call_fn) = self.method_call_fn.clone() {
                            self.build_vec(args)?;
                            self.build_expression(&args2[0])?; // struct
                            self.build_expression(&args2[1])?; // key
                            self.instructions
                                .push(Instruction::Push(Box::new(Value::Closure(method_call_fn))));
                            self.instructions.push(Instruction::Call(3));
                            self.stack_size -= 2;
                            assert_eq!(ss + 1, self.stack_size);
                            return Ok(());
                        }
                    }
                }

                if let Some(args) = args
                    .iter()
                    .map(|arg| {
                        if let model::VecAppend::Element(e) = arg {
                            Some(e)
                        } else {
                            None
                        }
                    })
                    .collect::<Option<Vec<_>>>()
                {
                    for arg in &args {
                        self.build_expression(arg)?;
                    }
                    self.build_expression(callee)?;
                    self.instructions.push(Instruction::Call(args.len()));
                    self.stack_size -= args.len();
                } else {
                    self.build_vec_appends(args)?;
                    self.build_expression(callee)?;
                    self.instructions
                        .push(Instruction::CallWithUnpack(args.len()));
                    self.stack_size -= args.len();
                }
                assert_eq!(ss + 1, self.stack_size);
            }
            model::Expression::Literal { value } => {
                self.instructions
                    .push(Instruction::Push(Box::new(build_value(value))));
                self.stack_size += 1;
            }
            model::Expression::Vec { appends } => {
                self.build_vec(appends)?;
            }
            model::Expression::Dict { appends } => {
                for append in appends {
                    match append {
                        model::DictAppend::Field(name, expr) => {
                            self.instructions
                                .push(Instruction::Push(Box::new(Value::String(name.clone()))));
                            self.stack_size += 1;
                            self.build_expression(expr)?;
                            self.instructions.push(Instruction::MakePair);
                            self.stack_size -= 1;
                        }
                        model::DictAppend::Spread(expr) => {
                            self.build_expression(expr)?;
                        }
                    }
                }
                self.instructions.push(Instruction::MakeDict(appends.len()));
                self.stack_size += 1;
                self.stack_size -= appends.len();
            }
            model::Expression::Struct {
                constructor,
                appends,
            } => {
                for append in appends {
                    match append {
                        model::DictAppend::Field(name, expr) => {
                            self.instructions
                                .push(Instruction::Push(Box::new(Value::String(name.clone()))));
                            self.stack_size += 1;
                            self.build_expression(expr)?;
                            self.instructions.push(Instruction::MakePair);
                            self.stack_size -= 1;
                        }
                        model::DictAppend::Spread(expr) => {
                            self.build_expression(expr)?;
                        }
                    }
                }
                self.build_expression(constructor)?;
                self.instructions
                    .push(Instruction::MakeStruct(appends.len()));
                self.stack_size -= appends.len();
            }
            model::Expression::Variable { name } => match self.resolve_variable(name) {
                Some((i1, i2, _)) => {
                    self.instructions
                        .push(Instruction::Load(i1 as u16, i2 as u16));
                    self.stack_size += 1;
                }
                None => return Err(Error::String(format!("variable not found: {}", name))),
            },
            model::Expression::If {
                condition,
                then,
                else_,
            } => {
                let ss = self.stack_size;

                let else_body = model::Expression::Literal {
                    value: model::Literal::Unit,
                };
                let else_ = else_.as_deref().unwrap_or(&else_body);

                self.build_expression(condition)?;

                let env_len = self.envs.last().unwrap().len();

                let mut builder = self.branch();
                builder.stack_size -= 1;
                builder.instructions_offset += 1;
                let res1 = builder.build_expression(then);
                if let Err(Error::String(_)) = res1 {
                    return res1;
                }

                let then_env_len = builder.envs.last().unwrap().len();
                builder.envs.last_mut().unwrap().truncate(env_len);

                self.instructions.push(Instruction::JumpIfNot(
                    self.instructions_offset
                        + self.instructions.len()
                        + builder.instructions.len()
                        + 2,
                ));

                self.break_indexes.extend(
                    builder
                        .break_indexes
                        .iter()
                        .map(|(name, i)| (name.clone(), i + self.instructions.len())),
                );
                self.instructions.extend(builder.instructions);

                let mut builder = self.branch();
                builder.stack_size -= 1;
                builder.instructions_offset += 1;
                let res2 = builder.build_expression(&else_);
                if let Err(Error::String(_)) = res2 {
                    return res2;
                }

                let else_env_len = builder.envs.last().unwrap().len();
                builder.envs.last_mut().unwrap().truncate(env_len);

                self.instructions.push(Instruction::Jump(
                    self.instructions_offset
                        + self.instructions.len()
                        + builder.instructions.len()
                        + 1,
                ));
                self.break_indexes.extend(
                    builder
                        .break_indexes
                        .iter()
                        .map(|(name, i)| (name.clone(), i + self.instructions.len())),
                );
                self.instructions.extend(builder.instructions);

                self.envs
                    .last_mut()
                    .unwrap()
                    .resize_with(then_env_len.max(else_env_len), || (intern(""), false));

                res1.or(res2)?;

                assert_eq!(ss + 1, self.stack_size);
            }
            model::Expression::Loop { body } => {
                assert!(!self.labels.is_empty(), "loop outside of labeled");

                self.build_expression(body)?;
                self.build_expression(&model::Expression::Continue { label: intern("") })?;
            }
            model::Expression::Labeled { label, body } => {
                let ss = self.stack_size;
                let continue_index = self.instructions_offset + self.instructions.len();
                self.labels.push(Label {
                    name: label.clone(),
                    continue_index,
                    stack_size: self.stack_size,
                });
                let env_len = self.envs.last().unwrap().len();

                match self.build_expression(body) {
                    Ok(()) => {
                        assert_eq!(ss, self.stack_size - 1);
                    }
                    Err(Error::Break) => {}
                    err @ Err(Error::Return) => return err,
                    err @ Err(Error::String(_)) => return err,
                }

                self.labels.pop();
                let index_to_break = self.instructions_offset + self.instructions.len();
                for (name, break_index) in self.break_indexes.iter() {
                    if !(name.is_empty() || name == label) {
                        continue;
                    }

                    assert!(matches!(
                        self.instructions[*break_index],
                        Instruction::Jump(0)
                    ));
                    self.instructions[*break_index] = Instruction::Jump(index_to_break);
                }
                self.break_indexes.retain(|(name, _)| name != label);

                self.stack_size = ss + 1;
                self.envs.last_mut().unwrap()[env_len..].fill_with(|| (intern(""), false));
            }
            model::Expression::Match { expr, arms } => {
                let expr = convert_match(expr.as_ref().clone(), arms.clone());
                // dbg!(&expr);
                self.build_expression(&expr)?;
            }
            model::Expression::Block(block) => {
                let env_len = self.envs.last().unwrap().len();
                self.build_block(block)?;
                self.envs.last_mut().unwrap()[env_len..].fill_with(|| (intern(""), false));
            }
            model::Expression::Return { expr } => {
                if let Some(expr) = expr {
                    self.build_expression(expr)?;
                } else {
                    self.instructions.push(Instruction::PushUnit);
                    self.stack_size += 1;
                }
                if self.stack_size > 1 {
                    self.instructions
                        .push(Instruction::Rewind(self.stack_size, true));
                    self.stack_size = 1;
                }
                self.instructions.push(Instruction::Return);
                return Err(Error::Return);
            }
            model::Expression::Break { label, expr } => {
                if let Some(expr) = expr {
                    self.build_expression(expr)?;
                } else {
                    self.instructions.push(Instruction::PushUnit);
                    self.stack_size += 1;
                };

                let Some(label) = self
                    .labels
                    .iter()
                    .rev()
                    .find(|cp| label.is_empty() || &cp.name == label)
                else {
                    return Err(Error::String("break outside of labeled".to_owned()));
                };
                let rewind = self.stack_size - label.stack_size;
                if rewind > 0 {
                    self.instructions.push(Instruction::Rewind(rewind, true));
                    self.stack_size = label.stack_size + 1;
                }
                self.break_indexes
                    .push((label.name.clone(), self.instructions.len()));
                self.instructions.push(Instruction::Jump(0));
                return Err(Error::Break);
            }
            model::Expression::Continue { label } => {
                let Some(label) = self
                    .labels
                    .iter()
                    .rev()
                    .find(|cp| label.is_empty() || &cp.name == label)
                else {
                    return Err(Error::String("continue outside of labeled".to_owned()));
                };
                let rewind = self.stack_size - label.stack_size;
                if rewind > 0 {
                    self.instructions.push(Instruction::Rewind(rewind, false));
                    self.stack_size = label.stack_size;
                }
                self.instructions
                    .push(Instruction::Jump(label.continue_index));
                return Err(Error::Break);
            }
            model::Expression::Closure(function) => {
                let function = self.build_function(function).map_err(Error::String)?;
                self.instructions
                    .push(Instruction::MakeClosure(Gc::new(self.mc, function)));
                self.stack_size += 1;
            }
            model::Expression::StaticNativeFn { native_fn } => {
                self.instructions
                    .push(Instruction::Push(Box::new(Value::NativeFn(native_fn))));
                self.stack_size += 1;
            }
        }
        Ok(())
    }

    #[must_use]
    fn build_vec(&mut self, appends: &Vec<model::VecAppend>) -> Result<()> {
        if let Some(exprs) = appends
            .iter()
            .map(|append| {
                if let model::VecAppend::Element(e) = append {
                    Some(e)
                } else {
                    None
                }
            })
            .collect::<Option<Vec<_>>>()
        {
            for expr in &exprs {
                self.build_expression(expr)?;
            }
            self.instructions.push(Instruction::MakeVec(exprs.len()));
        } else {
            self.build_vec_appends(appends)?;
            self.instructions
                .push(Instruction::MakeVecWithUnpack(appends.len()));
        }
        self.stack_size += 1;
        self.stack_size -= appends.len();
        Ok(())
    }

    #[must_use]
    fn build_vec_appends(&mut self, appends: &[model::VecAppend]) -> Result<()> {
        Ok(for append in appends {
            match append {
                model::VecAppend::Element(ee) => {
                    self.build_expression(ee)?;
                }
                model::VecAppend::Spread(ve) => {
                    self.build_expression(ve)?;
                    self.instructions.push(Instruction::PushUnit);
                    self.instructions.push(Instruction::MakePair);
                }
            }
        })
    }

    fn add_var(&mut self, name: Str, mutable: bool) -> (u16, u16) {
        let index = self.envs.len() - 1;
        let index2 = self.envs.last().unwrap().len();
        self.envs.last_mut().unwrap().push((name, mutable));
        (index as u16, index2 as u16)
    }

    fn resolve_variable(&self, name: &str) -> Option<(usize, usize, bool)> {
        self.envs.iter().enumerate().rev().find_map(|(i1, env)| {
            env.iter()
                .enumerate()
                .rev()
                .find(|(_, n)| n.0.as_str() == name)
                .map(|(i2, (_, mutable))| (i1, i2, *mutable))
        })
    }
}

fn build_value<'gc>(value: &model::Literal) -> Value<'gc> {
    match value {
        model::Literal::Unit => Value::Unit,
        model::Literal::Null => Value::Null,
        model::Literal::Int(value) => Value::Int(*value),
        model::Literal::Float(value) => Value::Float(*value),
        model::Literal::String(value) => Value::String(value.clone()),
        model::Literal::Bool(value) => Value::Bool(*value),
    }
}
