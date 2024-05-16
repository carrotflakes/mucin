use gc_arena::{lock::RefLock, Gc, Mutation};

use crate::{
    compile::{Function, Instruction},
    string::intern,
    value::{Closure, Dict, Struct, StructType, Value},
};

pub type Env<'gc> = Gc<'gc, RefLock<Struct<'gc>>>;

#[derive(Clone)]
pub struct Vm<'gc> {
    pub mc: &'gc Mutation<'gc>,
    pub frames: Vec<Frame<'gc>>,
    pub values: Vec<Value<'gc>>,
    pub strict_arity: bool,
}

#[derive(Clone)]
pub struct Frame<'gc> {
    function: Gc<'gc, Function<'gc>>,
    pc: usize,
    envs: Vec<Env<'gc>>,
}

impl<'gc> Vm<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>) -> Self {
        Self {
            mc,
            frames: Vec::with_capacity(256),
            values: Vec::with_capacity(1024),
            strict_arity: false,
        }
    }

    pub fn make_env(
        &mut self,
        mut envs: Vec<Env<'gc>>,
        struct_type: Gc<'gc, StructType<'gc>>,
        initializer: Vec<Instruction<'gc>>,
    ) -> Result<Env<'gc>, String> {
        envs.push(Struct::from_type(self.mc, struct_type));
        self.run_function(
            Gc::new(
                self.mc,
                Function {
                    name: intern(""),
                    arity: 0,
                    frame: Gc::new(
                        self.mc,
                        StructType {
                            name: intern(""),
                            fields: Box::new([]),
                            methods: Gc::new(self.mc, RefLock::new(Dict::default())),
                        },
                    ),
                    body: initializer,
                    capture_envs: (0..envs.len()).collect(),
                },
            ),
            envs.clone(),
        )?;
        Ok(envs.pop().unwrap())
    }

    pub fn run_closure(&mut self, closure: Closure<'gc>) -> Result<Value<'gc>, String> {
        assert_eq!(closure.function.arity, 0);

        let mut envs = closure.capture.to_vec();
        envs.push(Struct::from_type(self.mc, closure.function.frame));

        self.run_function(closure.function, envs)
    }

    pub fn run_closure_with_args(
        &mut self,
        closure: Closure<'gc>,
        mut args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>, String> {
        assert_eq!(closure.function.arity, args.len());

        let env = Struct::from_type(self.mc, closure.function.frame);
        env.borrow_mut(self.mc).values[..args.len()].swap_with_slice(&mut args);
        let mut envs = closure.capture.to_vec();
        envs.push(env);

        self.run_function(closure.function, envs)
    }

    fn run_function(
        &mut self,
        function: Gc<'gc, Function<'gc>>,
        envs: Vec<Env<'gc>>,
    ) -> Result<Value<'gc>, String> {
        self.frames.push(Frame {
            function,
            pc: 0,
            envs,
        });
        match self.run_loop() {
            Ok(v) => Ok(v),
            Err(err) => Err([err]
                .into_iter()
                .chain(
                    self.frames
                        .iter()
                        .map(|f| format!("{:?} [{}]", &f.function.name, f.pc)),
                )
                .collect::<Vec<_>>()
                .join("\n")),
        }
    }

    fn run_loop(&mut self) -> Result<Value<'gc>, String> {
        while let Some(Frame { function, pc, envs }) = self.frames.last_mut() {
            #[cfg(debug_assertions)]
            if *pc >= function.body.len() {
                println!("function: {:?}", function);
                panic!("unexpected end of function");
            }

            let instruction = &function.body[*pc];
            *pc += 1;
            match instruction {
                Instruction::Push(value) => self.values.push(value.as_ref().clone()),
                Instruction::PushUnit => self.values.push(Value::Unit),
                Instruction::Pop => {
                    self.values.pop();
                }
                Instruction::Dup => {
                    self.values.push(self.values.last().unwrap().clone());
                }
                Instruction::Swap => {
                    let len = self.values.len();
                    self.values.swap(len - 1, len - 2);
                }
                Instruction::CallNative(nf) => {
                    let len = self.values.len() - nf.arity;
                    let value = (nf.function)(self.mc, &self.values[len..])?;
                    self.values.truncate(len);
                    self.values.push(value);
                }
                Instruction::Call(arity) => {
                    let arity = *arity;
                    let f = self.values.pop().unwrap();
                    self.call(f, arity)?;
                }
                Instruction::CallWithUnpack(len) => {
                    let len = *len;
                    let f = self.values.pop().unwrap();
                    let base_len = self.values.len() - len;
                    for v in self.values.split_off(base_len) {
                        match v {
                            Value::Pair(e) => match e[0] {
                                Value::Vec(v) => self.values.extend(v.borrow().clone()),
                                _ => unreachable!(),
                            },
                            e => self.values.push(e),
                        }
                    }
                    self.call(f, self.values.len() - base_len)?;
                }

                Instruction::PushEnv(env) => {
                    envs.push(Struct::from_type(self.mc, env.clone()));
                }
                Instruction::TruncateEnv(len) => {
                    envs.truncate(*len);
                }

                Instruction::Return => {
                    self.frames.pop();
                }
                Instruction::Rewind(rewind, keep_top) => {
                    let len = self.values.len();
                    let last = if *keep_top { 1 } else { 0 };
                    self.values.drain(len - *rewind..len - last).count();
                }
                Instruction::Load(i1, i2) => {
                    self.values
                        .push(envs[*i1 as usize].borrow().values[*i2 as usize].clone());
                }
                Instruction::Store(i1, i2) => {
                    let value = self.values.pop().unwrap();
                    envs[*i1 as usize].borrow_mut(self.mc).values[*i2 as usize] = value;
                }

                Instruction::Add => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.add(&right)?);
                }
                Instruction::Sub => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.sub(&right)?);
                }
                Instruction::Mul => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.mul(&right)?);
                }
                Instruction::Div => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.div(&right)?);
                }
                Instruction::Rem => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.rem(&right)?);
                }
                Instruction::Neg => {
                    let value = self.values.pop().unwrap();
                    self.values.push(value.neg()?);
                }
                Instruction::Not => {
                    let value = self.values.pop().unwrap();
                    self.values.push(value.not());
                }
                Instruction::Eq => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.eq(&right));
                }
                Instruction::Ne => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.eq(&right).not());
                }
                Instruction::Gt => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.gt(&right)?);
                }
                Instruction::Ge => {
                    let right = self.values.pop().unwrap();
                    let left = self.values.pop().unwrap();
                    self.values.push(left.ge(&right)?);
                }
                Instruction::Jump(pc) => {
                    self.frames.last_mut().unwrap().pc = *pc;
                }
                Instruction::JumpIf(pc) => {
                    let condition = self.values.pop().unwrap();
                    if condition.is_truthy() {
                        self.frames.last_mut().unwrap().pc = *pc;
                    }
                }
                Instruction::JumpIfNot(pc) => {
                    let condition = self.values.pop().unwrap();
                    if !condition.is_truthy() {
                        self.frames.last_mut().unwrap().pc = *pc;
                    }
                }

                Instruction::MakeClosure(function) => {
                    let capture = function.capture_envs.iter().map(|i| envs[*i]).collect();
                    self.values.push(Value::Closure(Closure {
                        function: *function,
                        capture,
                    }));
                }
                Instruction::MakeVec(len) => {
                    let vec = self.values.split_off(self.values.len() - *len);
                    self.values
                        .push(Value::Vec(Gc::new(self.mc, RefLock::new(vec))));
                }
                Instruction::MakeVecWithUnpack(len) => {
                    let mut vec = Vec::with_capacity(*len);
                    for v in self.values.split_off(self.values.len() - *len) {
                        match v {
                            Value::Pair(e) => match e[0] {
                                Value::Vec(v) => vec.extend(v.borrow().clone()),
                                _ => unreachable!(),
                            },
                            e => vec.push(e),
                        }
                    }
                    self.values
                        .push(Value::Vec(Gc::new(self.mc, RefLock::new(vec))));
                }
                Instruction::MakePair => {
                    let values = self.values.split_off(self.values.len() - 2);
                    self.values.push(Value::Pair(values.into_boxed_slice()));
                    // let mut values = [self.values.pop().unwrap(),self.values.pop().unwrap()];
                    // values.reverse();
                    // self.values.push(Value::Pair(Box::new(values)));
                }
                Instruction::MakeDict(len) => {
                    let mut fields = vec![];
                    let appends = self.values.split_off(self.values.len() - *len);
                    for append in appends.into_iter().rev() {
                        match append {
                            Value::Pair(kv) => {
                                fields.push((kv[0].as_string().unwrap().clone(), kv[1].clone()));
                            }
                            Value::Dict(dict_) => {
                                fields.extend(dict_.borrow().0.clone());
                            }
                            _ => panic!("unexpected value"),
                        }
                    }
                    fields.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));
                    fields.dedup_by(|(k1, _), (k2, _)| k1 == k2);
                    self.values
                        .push(Value::Dict(Gc::new(self.mc, RefLock::new(Dict(fields)))));
                }
                Instruction::MakeStruct(len) => {
                    let constructor = self.values.pop().unwrap();
                    let Value::StructType(struct_type) = constructor else {
                        return Err(format!("expected struct type, got {:?}", constructor));
                    };

                    let mut fields = vec![];
                    let appends = self.values.split_off(self.values.len() - *len);
                    for append in appends.into_iter().rev() {
                        match append {
                            Value::Pair(kv) => {
                                fields.push((kv[0].as_string().unwrap().clone(), kv[1].clone()));
                            }
                            Value::Dict(dict_) => {
                                fields.extend(dict_.borrow().0.clone());
                            }
                            _ => panic!("unexpected value"),
                        }
                    }

                    let values = struct_type
                        .fields
                        .iter()
                        .map(|(name, _)| {
                            fields
                                .iter()
                                .find(|(k, _)| k == name)
                                .map(|(_, v)| v.clone())
                                .unwrap_or(Value::Unit)
                        })
                        .collect();
                    self.values.push(Value::Struct(Gc::new(
                        self.mc,
                        RefLock::new(Struct {
                            struct_type,
                            values,
                        }),
                    )));
                }
            }
        }
        assert_eq!(self.values.len(), 1, "unexpected values: {:?}", self.values);
        Ok(self.values.pop().unwrap())
    }

    pub fn call(&mut self, f: Value<'gc>, args_len: usize) -> Result<(), String> {
        match f {
            Value::NativeFn(nf) => {
                self.resize_args("nativeFn", nf.arity, args_len);
                let len = self.values.len() - nf.arity;
                let value = (nf.function)(self.mc, &self.values[len..])?;
                self.values.truncate(len);
                self.values.push(value);
            }
            Value::Closure(closure) => {
                let arity = closure.function.arity;
                self.resize_args(&closure.function.name, arity, args_len);
                let mut env =
                    vec![Value::Unit; closure.function.frame.fields.len()].into_boxed_slice();
                for i in 0..arity {
                    env[arity - i - 1] = self.values.pop().unwrap();
                }
                let mut envs = Vec::with_capacity(closure.capture.len() + 1); // TODO: We can expect the capacity
                envs.extend(closure.capture.iter());
                envs.push(Gc::new(
                    self.mc,
                    RefLock::new(Struct {
                        struct_type: closure.function.frame,
                        values: env,
                    }),
                ));
                self.frames.push(Frame {
                    function: closure.function.clone(),
                    pc: 0,
                    envs,
                });
            }
            Value::VmFn(vf) => {
                self.resize_args("vmFn", vf.arity, args_len);
                let f = vf.function;
                return f(self);
            }
            callee => return Err(format!("expected function, got {:?}", callee)),
        }

        Ok(())
    }

    fn resize_args(&mut self, name: &str, expect: usize, got: usize) {
        if self.strict_arity {
            assert_eq!(
                got, expect,
                "function {}: expected {} arguments, got {}",
                name, expect, got
            );
        }

        self.values
            .resize(self.values.len() + expect - got, Value::Unit);
    }
}

#[test]
fn test_stack_trace() {
    let src = r#"
fn main() a()
fn a() b()
fn b() panic("error")
"#;
    let mut runtime = crate::runtime::Runtime::new();
    runtime.push_env_from_src(src).unwrap();
    gilder::assert_golden!(runtime.call_fn("main", |_| vec![], |_, _| {}).unwrap_err());
}
