mod builder;
mod function_env;
mod pattern_match;

use crate::{
    model,
    string::Str,
    value::{StructType, Value},
};

pub use builder::Builder;
use gc_arena::{Collect, Gc, Mutation};

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct Function<'gc> {
    pub name: Str,
    pub arity: usize,
    pub frame: Gc<'gc, StructType<'gc>>,
    pub body: Vec<Instruction<'gc>>,
    pub capture_envs: Vec<usize>,
}

#[derive(Debug)]
pub struct NativeFn {
    pub arity: usize,
    pub function: for<'gc> fn(&Mutation<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, String>,
}

#[derive(Collect)]
#[collect(no_drop)]
pub enum Instruction<'gc> {
    Push(Box<Value<'gc>>), // TODO: devide into PushInt...
    PushUnit,
    Pop,
    Dup,
    Swap,
    Return,
    Load(u16, u16),
    Store(u16, u16),
    /// Rewind the stack n times keeping the top value
    Rewind(usize, bool),

    CallNative(&'static NativeFn),
    Call(usize),
    CallWithUnpack(usize),

    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Rem,
    Not,
    Eq,
    Ne,
    Gt,
    Ge,
    Jump(usize),
    JumpIf(usize),
    JumpIfNot(usize),

    MakeClosure(Gc<'gc, Function<'gc>>),
    MakePair,
    MakeVec(usize),
    MakeVecWithUnpack(usize),
    MakeDict(usize),
    MakeStruct(usize),
}

impl<'gc> std::fmt::Debug for Instruction<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Push(value) => write!(f, "push {:?}", value),
            Instruction::PushUnit => write!(f, "push_unit"),
            Instruction::Pop => write!(f, "pop"),
            Instruction::Dup => write!(f, "dup"),
            Instruction::Swap => write!(f, "swap"),
            Instruction::Return => write!(f, "return"),
            Instruction::Rewind(rewind, keep_top) => {
                write!(f, "rewind {} {}", rewind, keep_top)
            }
            Instruction::Load(i1, i2) => write!(f, "load {} {}", i1, i2),
            Instruction::Store(i1, i2) => write!(f, "store {} {}", i1, i2),

            Instruction::CallNative(_) => write!(f, "call_native"),
            Instruction::Call(arity) => write!(f, "call {}", arity),
            Instruction::CallWithUnpack(arity) => write!(f, "call_with_unpack {}", arity),

            Instruction::Add => write!(f, "add"),
            Instruction::Sub => write!(f, "sub"),
            Instruction::Mul => write!(f, "mul"),
            Instruction::Div => write!(f, "div"),
            Instruction::Rem => write!(f, "rem"),
            Instruction::Neg => write!(f, "neg"),
            Instruction::Not => write!(f, "not"),
            Instruction::Eq => write!(f, "eq"),
            Instruction::Ne => write!(f, "ne"),
            Instruction::Gt => write!(f, "gt"),
            Instruction::Ge => write!(f, "ge"),
            Instruction::Jump(offset) => write!(f, "jump {}", offset),
            Instruction::JumpIf(offset) => write!(f, "jump_if {}", offset),
            Instruction::JumpIfNot(offset) => write!(f, "jump_if_not {}", offset),

            Instruction::MakeClosure(c) => write!(f, "make_closure {:#?}", c),
            Instruction::MakeVec(size) => write!(f, "make_vec {}", size),
            Instruction::MakeVecWithUnpack(size) => write!(f, "make_vec_with_unpack {}", size),
            Instruction::MakePair => write!(f, "make_pair"),
            Instruction::MakeDict(size) => write!(f, "make_dict {}", size),
            Instruction::MakeStruct(size) => write!(f, "make_struct {}", size),
        }
    }
}
