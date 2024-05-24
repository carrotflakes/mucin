use gc_arena::{lock::RefLock, Mutation};

use crate::{
    native_fns::{NF_FIELD_ASSIGN, NF_INDEX, NF_STRUCT_TYPE_METHODS},
    string::{intern, unique_str, Str},
    value::{Closure, Dict},
};

use self::{function_env::FunctionEnv, pattern_match::convert_match};

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
    blocks: Vec<Block<'gc>>,
    current: usize,
    envs: Envs,
    labels: Vec<(Str, BlockId, BlockId)>,
}

#[derive(Debug, Clone, PartialEq)]
struct BlockId(usize);

#[derive(Collect, Debug)]
#[collect(no_drop)]
struct Block<'gc> {
    #[collect(require_static)]
    name: BlockId,
    insts: Vec<Inst<'gc>>,
    stack_size: Option<usize>,
}

#[derive(Collect, Debug)]
#[collect(no_drop)]
enum Inst<'gc> {
    Instruction(Instruction<'gc>),
    Jump {
        #[collect(require_static)]
        jump_type: JumpType,
        #[collect(require_static)]
        block: BlockId,
        stack_size: Option<usize>,
        merged: bool,
    },
    Return {
        stack_size: Option<usize>,
    },
}

#[derive(Debug)]
enum JumpType {
    Jump,
    JumpIf,
    JumpIfNot,
}

#[derive(Debug, Clone)]
struct Envs(Vec<Vec<(Str, bool)>>);

impl<'gc> Builder<'gc> {
    pub fn new(
        mc: &'gc Mutation<'gc>,
        envs: Vec<Vec<(Str, bool)>>,
        method_call_fn: Option<Closure<'gc>>,
    ) -> Self {
        Self {
            mc,
            method_call_fn,
            blocks: vec![Block {
                name: BlockId(0),
                insts: vec![],
                stack_size: Some(0),
            }],
            current: 0,
            envs: Envs(envs),
            labels: vec![],
        }
    }

    fn instructions(&mut self) -> Vec<Instruction<'gc>> {
        // Flatten blocks
        let mut blocks = vec![];
        while !self.blocks.is_empty() {
            let block = self.blocks.remove(0);
            blocks.push(block);

            while let Some(Inst::Jump { block, .. }) = blocks.last().unwrap().insts.last() {
                if let Some(i) = self.blocks.iter().position(|b| b.name == *block) {
                    if let Some(Inst::Jump { merged, .. }) =
                        blocks.last_mut().unwrap().insts.last_mut()
                    {
                        *merged = true;
                    }
                    blocks.push(self.blocks.remove(i));
                } else {
                    break;
                }
            }
        }

        // Resolve stack size
        let mut queue = vec![0];
        let mut closed = vec![0];
        while let Some(i) = queue.pop() {
            let mut ss = blocks[i].stack_size.unwrap() as i32;
            for j in 0..blocks[i].insts.len() {
                match &mut blocks[i].insts[j] {
                    Inst::Instruction(inst) => ss += inst.stack_size(),
                    Inst::Jump {
                        block,
                        stack_size,
                        jump_type,
                        ..
                    } => {
                        match jump_type {
                            JumpType::Jump => {}
                            JumpType::JumpIf | JumpType::JumpIfNot => ss -= 1,
                        }
                        *stack_size = Some(ss as usize);
                        let block = block.clone();
                        let k = blocks.iter().position(|b| b.name == block).unwrap();
                        if !closed.contains(&k) {
                            queue.push(k);
                            closed.push(k);
                        }
                        if let Some(ss_) = &mut blocks[k].stack_size {
                            if (ss as usize) < *ss_ {
                                *ss_ = ss as usize;
                                if !queue.contains(&k) {
                                    queue.push(k);
                                }
                            }
                        } else {
                            blocks[k].stack_size = Some(ss as usize);
                        }
                    }
                    Inst::Return { stack_size } => {
                        assert!(stack_size.is_none());
                        *stack_size = Some(ss as usize);
                    }
                }
            }
        }

        // Resolve jumps
        let mut insts = vec![];
        let mut block_entries = vec![];
        for block in &blocks {
            block_entries.push(insts.len());
            for inst in &block.insts {
                match inst {
                    Inst::Instruction(inst) => insts.push(inst.clone()),
                    Inst::Jump {
                        jump_type,
                        block,
                        stack_size,
                        merged,
                    } => {
                        if *merged {
                            continue;
                        }
                        let rewind = stack_size.unwrap()
                            - blocks
                                .iter()
                                .find(|b| &b.name == block)
                                .unwrap()
                                .stack_size
                                .unwrap();
                        if rewind > 0 {
                            insts.push(Instruction::Rewind(rewind, false));
                        }
                        let i = blocks.iter().position(|b| &b.name == block).unwrap();
                        match jump_type {
                            JumpType::Jump => insts.push(Instruction::Jump(i)),
                            JumpType::JumpIf => insts.push(Instruction::JumpIf(i)),
                            JumpType::JumpIfNot => insts.push(Instruction::JumpIfNot(i)),
                        }
                    }
                    Inst::Return { stack_size } => {
                        if let Some(rewind) = stack_size {
                            if *rewind > 1 {
                                insts.push(Instruction::Rewind(*rewind, true));
                            }
                        }
                        insts.push(Instruction::Return);
                    }
                }
            }
        }
        for inst in &mut insts {
            match inst {
                Instruction::Jump(i) => *i = block_entries[*i],
                Instruction::JumpIf(i) => *i = block_entries[*i],
                Instruction::JumpIfNot(i) => *i = block_entries[*i],
                _ => {}
            }
        }

        insts
    }

    pub fn build_program(
        &mut self,
        definitions: &[ast::Definition],
    ) -> std::result::Result<(Vec<(Str, bool)>, Vec<Instruction<'gc>>), String> {
        let mut names = Vec::new();
        for definition in definitions {
            match definition {
                ast::Definition::Function(function) => {
                    names.push((function.name.clone(), true));
                }
                ast::Definition::Method { .. } => {}
                ast::Definition::Variable {
                    name,
                    mutable,
                    expr: _,
                } => {
                    names.push((name.clone(), *mutable));
                }
                ast::Definition::Struct { name, fields: _ } => {
                    names.push((name.clone(), false));
                }
                ast::Definition::Module(mod_name) => {
                    names.push((mod_name.clone(), false));
                }
            }
        }
        self.envs.0.push(names.clone());

        for definition in definitions {
            match definition {
                ast::Definition::Function(function) => {
                    let f = Gc::new(self.mc, self.build_function(function)?);
                    let index = self.resolve_variable(&function.name).unwrap();
                    self.push_inst(Instruction::MakeClosure(f));
                    self.push_inst(Instruction::Store(index.0 as u16, index.1 as u16));
                }
                ast::Definition::Method { receiver, function } => {
                    let f = Gc::new(self.mc, self.build_function(function)?);
                    self.build_expression(receiver).unwrap();
                    self.push_inst(Instruction::CallNative(&NF_STRUCT_TYPE_METHODS));
                    self.push_inst(Instruction::Push(Box::new(Value::String(
                        function.name.clone(),
                    ))));
                    self.push_inst(Instruction::MakeClosure(f));
                    self.push_inst(Instruction::CallNative(&NF_FIELD_ASSIGN));
                    self.push_inst(Instruction::Pop);
                }
                ast::Definition::Variable {
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
                    self.push_inst(Instruction::Store(index.0 as u16, index.1 as u16));
                }
                ast::Definition::Struct { name, fields } => {
                    let index = self.resolve_variable(name).unwrap();
                    self.push_inst(Instruction::Push(Box::new(Value::StructType(Gc::new(
                        self.mc,
                        StructType {
                            name: name.clone(),
                            fields: fields.clone().into_boxed_slice(),
                            methods: Gc::new(self.mc, RefLock::new(Dict::default())),
                        },
                    )))));
                    self.push_inst(Instruction::Store(index.0 as u16, index.1 as u16));
                }
                ast::Definition::Module(name) => {
                    let index = self.resolve_variable(name).unwrap();
                    self.push_inst(Instruction::Push(Box::new(Value::Unit)));
                    self.push_inst(Instruction::Store(index.0 as u16, index.1 as u16));
                }
            }
        }
        self.push_inst(Instruction::PushUnit);
        self.push_inst(Instruction::Return);

        Ok((names, self.instructions()))
    }

    #[must_use]
    pub fn build_function(
        &mut self,
        function: &ast::Function,
    ) -> std::result::Result<Function<'gc>, String> {
        let variables = super::function_env::FunctionEnv::from_function(function).into_outer_env();

        let mut capture_envs: Vec<_> = variables
            .iter()
            .map(|name| {
                self.envs
                    .resolve_variable(name)
                    .map(|(i1, _, _)| i1)
                    .ok_or_else(|| format!("variable not found: {}", name))
            })
            .collect::<std::result::Result<_, _>>()?;
        capture_envs.sort_unstable();
        capture_envs.dedup();

        let mut builder = Builder {
            mc: self.mc,
            method_call_fn: self.method_call_fn.clone(),
            envs: Envs(
                capture_envs
                    .iter()
                    .map(|i| self.envs.0[*i].clone())
                    .collect(),
            ),
            blocks: vec![Block {
                name: BlockId(0),
                insts: vec![],
                stack_size: Some(0),
            }],
            current: 0,
            labels: vec![],
        };

        builder.envs.0.push(
            function
                .args
                .iter()
                .map(|arg| (arg.clone(), true))
                .collect(),
        );

        match builder.build_expression(&function.body) {
            Ok(()) => {
                builder.push_inst(Instruction::Return);
            }
            Err(Error::Break) => return Err("unexpected break in function definition".to_owned()),
            Err(Error::Return) => {}
            Err(Error::String(err)) => return Err(err),
        };

        let frame = builder.envs.0.pop().unwrap();
        let frame = Gc::new(
            self.mc,
            StructType {
                name: function.name.clone(),
                fields: frame.into_boxed_slice(),
                methods: Gc::new(self.mc, RefLock::new(Dict::default())),
            },
        );

        let body = builder.instructions();

        Ok(Function {
            name: function.name.clone(),
            arity: function.args.len(),
            frame,
            body,
            capture_envs,
        })
    }

    #[must_use]
    fn build_expression(&mut self, expression: &ast::Expression) -> Result<()> {
        match expression {
            ast::Expression::Op { name, args } => {
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
                    self.push_inst(inst);
                    return Ok(());
                }

                match name.as_str() {
                    "__or" | "__and" => {
                        self.build_expression(&args[0])?;

                        self.push_inst(Instruction::Dup);

                        let then_bname = self.new_block();
                        let after_bname = self.new_block();

                        if name.as_str() == "__or" {
                            self.push_jump(JumpType::JumpIf, after_bname.clone());
                        } else {
                            self.push_jump(JumpType::JumpIfNot, after_bname.clone());
                        }
                        self.push_jump(JumpType::Jump, then_bname.clone());

                        self.switch(then_bname);
                        self.push_inst(Instruction::Pop);
                        self.build_expression(&args[1])?;
                        self.push_jump(JumpType::Jump, after_bname.clone());
                        self.switch(after_bname);

                        return Ok(());
                    }
                    "__index" => {
                        for arg in args {
                            self.build_expression(arg)?;
                        }
                        self.push_inst(Instruction::CallNative(&NF_INDEX));
                        return Ok(());
                    }
                    _ => {
                        unreachable!("unknown operator: {}", name);
                    }
                }
            }
            ast::Expression::Call { callee, args } => {
                if let ast::Expression::Op { name, args: args2 } = callee.as_ref() {
                    if name.as_str() == "__index" {
                        if let Some(method_call_fn) = self.method_call_fn.clone() {
                            self.build_vec(args)?;
                            self.build_expression(&args2[0])?; // struct
                            self.build_expression(&args2[1])?; // key
                            self.push_inst(Instruction::Push(Box::new(Value::Closure(
                                method_call_fn,
                            ))));
                            self.push_inst(Instruction::Call(3));
                            return Ok(());
                        }
                    }
                }

                if let Some(args) = args
                    .iter()
                    .map(|arg| {
                        if let ast::VecAppend::Element(e) = arg {
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
                    self.push_inst(Instruction::Call(args.len()));
                } else {
                    self.build_vec_appends(args)?;
                    self.build_expression(callee)?;
                    self.push_inst(Instruction::CallWithUnpack(args.len()));
                }
            }
            ast::Expression::Literal { value } => {
                self.push_inst(Instruction::Push(Box::new(build_value(value))));
            }
            ast::Expression::Vec { appends } => {
                self.build_vec(appends)?;
            }
            ast::Expression::Dict { appends } => {
                self.build_dict_appends(appends)?;
                self.push_inst(Instruction::MakeDict(appends.len()));
            }
            ast::Expression::Struct {
                constructor,
                appends,
            } => {
                self.build_dict_appends(appends)?;
                self.build_expression(constructor)?;
                self.push_inst(Instruction::MakeStruct(appends.len()));
            }
            ast::Expression::Variable { name } => match self.envs.resolve_variable(name) {
                Some((i1, i2, _)) => {
                    self.push_inst(Instruction::Load(i1 as u16, i2 as u16));
                }
                None => return Err(Error::String(format!("variable not found: {}", name))),
            },
            ast::Expression::If {
                condition,
                then,
                else_,
            } => {
                self.build_expression(condition)?;

                let else_body = ast::Expression::Literal {
                    value: ast::Literal::Unit,
                };
                let else_ = else_.as_deref().unwrap_or(&else_body);

                let then_bname = self.new_block();
                let else_bname = self.new_block();
                let after_bname = self.new_block();

                self.push_jump(JumpType::JumpIfNot, else_bname.clone());
                self.push_jump(JumpType::Jump, then_bname.clone());
                self.switch(else_bname);

                let res1 = self.build_expression(else_);
                if let Err(Error::String(_)) = res1 {
                    return res1;
                }
                if res1.is_ok() {
                    self.push_jump(JumpType::Jump, after_bname.clone());
                }

                self.switch(then_bname);

                let res2 = self.build_expression(then);
                if let Err(Error::String(_)) = res2 {
                    return res2;
                }
                if res2.is_ok() {
                    self.push_jump(JumpType::Jump, after_bname.clone());
                }

                res1.or(res2)?;

                self.switch(after_bname);
            }
            ast::Expression::Loop { body } => {
                self.build_expression(body)?;
                self.build_expression(&ast::Expression::Continue { label: intern("") })?;
            }
            ast::Expression::Labeled { label, body } => {
                self.build_labeled(label, body)?;
            }
            ast::Expression::Match { expr, arms } => {
                let expr = convert_match(expr.as_ref().clone(), arms.clone());
                self.build_expression(&expr)?;
            }
            ast::Expression::For {
                variable,
                iterable,
                body,
            } => {
                let expr = convert_if_expr(iterable, variable, body);

                self.build_expression(&expr)?;
            }
            ast::Expression::Block(block) => {
                let env_required = FunctionEnv::from_block(block).has_eternal_variable();
                if env_required {
                    let insts_len = self.current().insts.len();
                    self.push_inst(Instruction::PushUnit);

                    let envs_len = self.envs.0.len();
                    self.envs.0.push(vec![]);

                    self.build_block(block)?;

                    let env = self.envs.0.remove(envs_len);

                    self.current().insts[insts_len] =
                        Inst::Instruction(Instruction::PushEnv(Gc::new(
                            self.mc,
                            StructType {
                                name: intern(""),
                                fields: env.into_boxed_slice(),
                                methods: Gc::new(self.mc, RefLock::new(Dict::default())),
                            },
                        )));
                    self.push_inst(Instruction::TruncateEnv(envs_len));

                    return Ok(());
                }

                let env_len = self.envs.0.last().unwrap().len();

                self.build_block(block)?;

                self.envs.0.last_mut().unwrap()[env_len..].fill_with(|| (intern(""), false));
            }
            ast::Expression::Return { expr } => {
                if let Some(expr) = expr {
                    self.build_expression(expr)?;
                } else {
                    self.push_inst(Instruction::PushUnit);
                }

                self.current().insts.push(Inst::Return { stack_size: None });
                return Err(Error::Return);
            }
            ast::Expression::Break { label, expr } => {
                let Some(label) = self.get_label(label) else {
                    return Err(Error::String("break outside of labeled".to_owned()));
                };
                let bname = label.2.clone();

                if let Some(expr) = expr {
                    self.build_expression(expr)?;
                } else {
                    self.push_inst(Instruction::PushUnit);
                }

                self.push_jump(JumpType::Jump, bname);
                return Err(Error::Break);
            }
            ast::Expression::Continue { label } => {
                let Some(label) = self.get_label(label) else {
                    return Err(Error::String("continue outside of labeled".to_owned()));
                };

                self.push_jump(JumpType::Jump, label.1.clone());
                return Err(Error::Break);
            }
            ast::Expression::Closure(function) => {
                let function = self.build_function(function).map_err(Error::String)?;
                self.push_inst(Instruction::MakeClosure(Gc::new(self.mc, function)));
            }
            ast::Expression::Env(expr) => {
                self.build_expression(expr)?;
                // let index = self.instructions.len();
                // self.push_inst(Instruction::PushUnit);
                // let envs_len = self.envs.len();
                // self.envs.push(vec![]);

                // self.build_expression(expr)?;

                // let env = self.envs.pop().unwrap();
                // assert_eq!(envs_len, self.envs.len());
                // self.instructions[index] = Instruction::PushEnv(Gc::new(
                //     self.mc,
                //     StructType {
                //         name: intern(""),
                //         fields: env.into_boxed_slice(),
                //         methods: Gc::new(self.mc, RefLock::new(Dict::default())),
                //     },
                // ));
                // self.push_inst(Instruction::TruncateEnv(self.envs.len()));
            }
            ast::Expression::StaticNativeFn { native_fn } => {
                self.push_inst(Instruction::Push(Box::new(Value::NativeFn(native_fn))));
            }
        }
        Ok(())
    }

    #[must_use]
    fn build_labeled(
        &mut self,
        label: &std::sync::Arc<String>,
        body: &ast::Expression,
    ) -> Result<()> {
        let body_bname = self.new_block();
        let after_bname = self.new_block();
        self.push_jump(JumpType::Jump, body_bname.clone());

        let labels_len = self.labels.len();
        self.labels
            .push((label.clone(), body_bname.clone(), after_bname.clone()));
        self.switch(body_bname.clone());
        match self.build_expression(body) {
            Ok(()) => {}
            Err(Error::Break) => {}
            Err(Error::Return) => {}
            Err(e) => return Err(e),
        };
        self.push_jump(JumpType::Jump, after_bname.clone());
        self.labels.truncate(labels_len);
        self.switch(after_bname);

        Ok(())
    }

    #[must_use]
    fn build_block(&mut self, block: &ast::Block) -> Result<()> {
        for statement in &block.statements {
            match statement {
                ast::Statement::Let {
                    name,
                    mutable,
                    expr,
                } => {
                    self.build_expression(expr)?;
                    self.envs.add_var(name.clone(), *mutable);
                    let index = self.envs.resolve_variable(name).unwrap();
                    self.push_inst(Instruction::Store(index.0 as u16, index.1 as u16));
                }
                ast::Statement::Expression { expr } => {
                    self.build_expression(expr)?;
                    self.push_inst(Instruction::Pop);
                }
                ast::Statement::Assign { name, expr, op } => {
                    let index = self.envs.resolve_variable(name).unwrap();
                    if !index.2 {
                        return Err(Error::String(format!("variable is not mutable: {}", name)));
                    }

                    if let Some(op) = op {
                        // Assign with operation
                        self.push_inst(Instruction::Load(index.0 as u16, index.1 as u16));
                        self.build_expression(expr)?;
                        match op.as_str() {
                            "__add" => self.push_inst(Instruction::Add),
                            "__sub" => self.push_inst(Instruction::Sub),
                            "__mul" => self.push_inst(Instruction::Mul),
                            "__div" => self.push_inst(Instruction::Div),
                            "__rem" => self.push_inst(Instruction::Rem),
                            _ => unreachable!(),
                        }
                    } else {
                        self.build_expression(expr)?;
                    }

                    self.push_inst(Instruction::Store(index.0 as u16, index.1 as u16));
                }
                ast::Statement::FieldAssign {
                    dict,
                    field,
                    expr,
                    op,
                } => {
                    self.build_expression(dict)?;

                    if let Some(op) = op {
                        // Assign with operation
                        self.push_inst(Instruction::Dup);
                        self.build_expression(field)?;
                        if let ast::Expression::Literal { value: _ } = field {
                            self.push_inst(Instruction::CallNative(&NF_INDEX));
                            self.build_expression(field)?;
                        } else {
                            self.push_inst(Instruction::Dup);
                            let index = self.envs.add_var(intern(""), false);
                            self.push_inst(Instruction::Store(index.0, index.1));
                            self.push_inst(Instruction::CallNative(&NF_INDEX));
                            self.push_inst(Instruction::Load(index.0, index.1));
                        }
                        self.push_inst(Instruction::Swap);

                        self.build_expression(expr)?;
                        match op.as_str() {
                            "__add" => self.push_inst(Instruction::Add),
                            "__sub" => self.push_inst(Instruction::Sub),
                            "__mul" => self.push_inst(Instruction::Mul),
                            "__div" => self.push_inst(Instruction::Div),
                            "__rem" => self.push_inst(Instruction::Rem),
                            _ => unreachable!(),
                        }
                    } else {
                        self.build_expression(field)?;
                        self.build_expression(expr)?;
                    }

                    self.push_inst(Instruction::CallNative(&NF_FIELD_ASSIGN));
                    self.push_inst(Instruction::Pop);
                }
                ast::Statement::Defer { expr: _ } => {
                    todo!()
                }
            }
        }

        if let Some(expr) = &block.expr {
            self.build_expression(expr)
        } else {
            self.push_inst(Instruction::PushUnit);
            Ok(())
        }
    }

    #[must_use]
    fn build_dict_appends(&mut self, appends: &Vec<ast::DictAppend>) -> Result<()> {
        Ok(for append in appends {
            match append {
                ast::DictAppend::Field(name, expr) => {
                    self.push_inst(Instruction::Push(Box::new(Value::String(name.clone()))));
                    self.build_expression(expr)?;
                    self.push_inst(Instruction::MakePair);
                }
                ast::DictAppend::Spread(expr) => {
                    self.build_expression(expr)?;
                }
            }
        })
    }

    #[must_use]
    fn build_vec(&mut self, appends: &Vec<ast::VecAppend>) -> Result<()> {
        if let Some(exprs) = appends
            .iter()
            .map(|append| {
                if let ast::VecAppend::Element(e) = append {
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
            self.push_inst(Instruction::MakeVec(exprs.len()));
        } else {
            self.build_vec_appends(appends)?;
            self.push_inst(Instruction::MakeVecWithUnpack(appends.len()));
        }
        Ok(())
    }

    #[must_use]
    fn build_vec_appends(&mut self, appends: &[ast::VecAppend]) -> Result<()> {
        for append in appends {
            match append {
                ast::VecAppend::Element(ee) => {
                    self.build_expression(ee)?;
                }
                ast::VecAppend::Spread(ve) => {
                    self.build_expression(ve)?;
                    self.push_inst(Instruction::PushUnit);
                    self.push_inst(Instruction::MakePair);
                }
            }
        }
        Ok(())
    }

    fn push_inst(&mut self, inst: Instruction<'gc>) {
        self.current().insts.push(Inst::Instruction(inst));
    }

    fn push_jump(&mut self, jump_type: JumpType, block: BlockId) {
        self.blocks[self.current].insts.push(Inst::Jump {
            jump_type,
            block,
            stack_size: None,
            merged: false,
        });
    }

    fn current(&mut self) -> &mut Block<'gc> {
        &mut self.blocks[self.current]
    }

    fn new_block(&mut self) -> BlockId {
        let name = BlockId(self.blocks.len());
        self.blocks.push(Block {
            name: name.clone(),
            insts: vec![],
            stack_size: None,
        });
        name
    }

    fn switch(&mut self, name: BlockId) {
        self.current = self.blocks.iter().position(|b| b.name == name).unwrap();
    }

    fn get_label(&self, name: &Str) -> Option<&(Str, BlockId, BlockId)> {
        if name.is_empty() {
            return self.labels.last();
        }
        self.labels.iter().rev().find(|(n, _, _)| n == name)
    }

    fn resolve_variable(&self, name: &str) -> Option<(usize, usize, bool)> {
        self.envs.resolve_variable(name)
    }
}

impl Envs {
    fn add_var(&mut self, name: Str, mutable: bool) -> (u16, u16) {
        let index = self.0.len() - 1;
        let index2 = self.0.last().unwrap().len();
        self.0.last_mut().unwrap().push((name, mutable));
        (index as u16, index2 as u16)
    }

    fn resolve_variable(&self, name: &str) -> Option<(usize, usize, bool)> {
        self.0.iter().enumerate().rev().find_map(|(i1, env)| {
            env.iter().enumerate().rev().find_map(|(i2, (n, mutable))| {
                if n.as_str() == name {
                    Some((i1, i2, *mutable))
                } else {
                    None
                }
            })
        })
    }
}

fn convert_if_expr(
    iterable: &Box<ast::Expression>,
    variable: &Str,
    body: &Box<ast::Expression>,
) -> ast::Expression {
    let it = unique_str("#for_it");

    ast::Expression::Block(ast::Block {
        statements: vec![
            ast::Statement::Let {
                name: it.clone(),
                mutable: false,
                expr: iterable.as_ref().clone(),
            },
            ast::Statement::Let {
                name: variable.clone(),
                mutable: true,
                expr: ast::Expression::Literal {
                    value: ast::Literal::Unit,
                },
            },
        ],
        expr: Some(Box::new(ast::Expression::Labeled {
            label: intern(""),
            body: Box::new(ast::Expression::Loop {
                body: Box::new(ast::Expression::If {
                    condition: Box::new(ast::Expression::Block(ast::Block {
                        statements: vec![ast::Statement::Assign {
                            name: variable.clone(),
                            expr: ast::Expression::Call {
                                callee: Box::new(ast::Expression::Variable { name: it.clone() }),
                                args: vec![],
                            },
                            op: None,
                        }],
                        expr: Some(Box::new(ast::Expression::Variable {
                            name: variable.clone(),
                        })),
                    })),
                    then: body.clone(),
                    else_: Some(Box::new(ast::Expression::Break {
                        label: intern(""),
                        expr: None,
                    })),
                }),
            }),
        })),
    })
}

fn build_value<'gc>(value: &ast::Literal) -> Value<'gc> {
    match value {
        ast::Literal::Unit => Value::Unit,
        ast::Literal::Null => Value::Null,
        ast::Literal::Int(value) => Value::Int(*value),
        ast::Literal::Float(value) => Value::Float(*value),
        ast::Literal::String(value) => Value::String(value.clone()),
        ast::Literal::Bool(value) => Value::Bool(*value),
    }
}

impl<'gc> Instruction<'gc> {
    fn stack_size(&self) -> i32 {
        match self {
            Instruction::Push(_) => 1,
            Instruction::PushEnv(_) => 0,
            Instruction::PushUnit => 1,
            Instruction::Pop => -1,
            Instruction::Dup => 1,
            Instruction::Swap => 0,
            Instruction::Load(_, _) => 1,
            Instruction::Store(_, _) => -1,
            Instruction::TruncateEnv(_) => 0,
            Instruction::Rewind(len, keep) => (if *keep { 1 } else { 0 }) - *len as i32,
            Instruction::Return => 0,
            Instruction::Add => -1,
            Instruction::Sub => -1,
            Instruction::Mul => -1,
            Instruction::Div => -1,
            Instruction::Rem => -1,
            Instruction::Neg => 0,
            Instruction::Eq => -1,
            Instruction::Ne => -1,
            Instruction::Gt => -1,
            Instruction::Ge => -1,
            Instruction::Not => 0,
            Instruction::Jump(_) => 0,
            Instruction::JumpIf(_) => -1,
            Instruction::JumpIfNot(_) => -1,
            Instruction::Call(len) => -(*len as i32),
            Instruction::CallWithUnpack(len) => -(*len as i32),
            Instruction::CallNative(f) => 1 - (f.arity as i32),
            Instruction::MakeVec(len) => 1 - (*len as i32),
            Instruction::MakeVecWithUnpack(len) => 1 - (*len as i32),
            Instruction::MakeDict(len) => 1 - (*len as i32),
            Instruction::MakePair => -1,
            Instruction::MakeStruct(len) => 1 - (*len as i32),
            Instruction::MakeClosure(_) => 1,
        }
    }
}
