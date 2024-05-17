pub mod dict;
pub mod r#struct;

use std::sync::{Arc, Mutex};

use gc_arena::{lock::RefLock, Collect, Gc, Mutation};

use crate::{compile::Function, string::Str};

pub use dict::Dict;
pub use r#struct::{Struct, StructType};

type Error = String;

#[derive(Collect, Clone)]
#[collect(no_drop)]
pub enum Value<'gc> {
    Unit,
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Str),

    // NOTE: Box<[Value<'gc>; 2]> is slow
    Pair(Box<[Value<'gc>]>),
    Vec(Gc<'gc, RefLock<Vec<Value<'gc>>>>),
    Dict(Gc<'gc, RefLock<Dict<'gc>>>),
    StructType(Gc<'gc, StructType<'gc>>),
    Struct(Gc<'gc, RefLock<Struct<'gc>>>),

    Any(#[collect(require_static)] Arc<dyn std::any::Any>),
    AnyMut(#[collect(require_static)] Arc<Mutex<dyn std::any::Any>>),

    Closure(Closure<'gc>),
    NativeFn(&'static NativeFn),
    VmFn(&'static VmFn),
}

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
pub struct Closure<'gc> {
    pub function: Gc<'gc, Function<'gc>>,
    pub capture: Box<[Gc<'gc, RefLock<Struct<'gc>>>]>,
}

#[derive(Debug)]
pub struct NativeFn {
    pub arity: usize,
    pub function: for<'gc> fn(&Mutation<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, String>,
}

#[derive(Debug)]
pub struct VmFn {
    pub arity: usize,
    pub function: for<'gc> fn(&mut crate::vm::Vm<'gc>) -> Result<(), String>,
}

impl<'gc> Value<'gc> {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Int(value) => Some(*value as f64),
            Value::Float(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(value) => Some(value.as_str()),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&Str> {
        match self {
            Value::String(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_vec(&self) -> Option<Gc<'gc, RefLock<Vec<Value<'gc>>>>> {
        match self {
            Value::Vec(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_dict(&self) -> Option<Gc<'gc, RefLock<Dict<'gc>>>> {
        match self {
            Value::Dict(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_closure(&self) -> Option<&Closure<'gc>> {
        match self {
            Value::Closure(closure) => Some(closure),
            _ => None,
        }
    }

    pub fn as_any(&self) -> Option<&Arc<dyn std::any::Any>> {
        match self {
            Value::Any(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_any_mut(&self) -> Option<&Arc<Mutex<dyn std::any::Any>>> {
        match self {
            Value::AnyMut(value) => Some(value),
            _ => None,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(value) => *value,
            Value::Null => false,
            Value::Unit => false,
            _ => true,
        }
    }

    pub fn add(&self, other: &Value<'gc>) -> Result<Value<'gc>, Error> {
        Ok(match (self, other) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left + right),
            (Value::Int(left), Value::Float(right)) => Value::Float(*left as f64 + right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left + *right as f64),
            (Value::String(left), Value::String(right)) => {
                Value::String(Str::from(format!("{}{}", left, right)))
            }
            _ => {
                return Err(format!(
                    "unsupported types for add, {:?} and {:?}",
                    self, other
                ))
            }
        })
    }

    pub fn sub(&self, other: &Value<'gc>) -> Result<Value<'gc>, Error> {
        Ok(match (self, other) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left - right),
            (Value::Int(left), Value::Float(right)) => Value::Float(*left as f64 - right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left - *right as f64),
            _ => {
                return Err(format!(
                    "unsupported types for sub, {:?} and {:?}",
                    self, other
                ))
            }
        })
    }

    pub fn mul(&self, other: &Value<'gc>) -> Result<Value<'gc>, Error> {
        Ok(match (self, other) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left * right),
            (Value::Int(left), Value::Float(right)) => Value::Float(*left as f64 * right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left * *right as f64),
            _ => {
                return Err(format!(
                    "unsupported types for mul, {:?} and {:?}",
                    self, other
                ))
            }
        })
    }

    pub fn div(&self, other: &Value<'gc>) -> Result<Value<'gc>, Error> {
        Ok(match (self, other) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left / right),
            (Value::Int(left), Value::Float(right)) => Value::Float(*left as f64 / right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left / *right as f64),
            _ => {
                return Err(format!(
                    "unsupported types for div, {:?} and {:?}",
                    self, other
                ))
            }
        })
    }

    pub fn rem(&self, other: &Value<'gc>) -> Result<Value<'gc>, Error> {
        Ok(match (self, other) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left % right),
            (Value::Int(left), Value::Float(right)) => Value::Float(*left as f64 % right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left % right),
            (Value::Float(left), Value::Int(right)) => Value::Float(left % *right as f64),
            _ => {
                return Err(format!(
                    "unsupported types for rem, {:?} and {:?}",
                    self, other
                ))
            }
        })
    }

    pub fn neg(&self) -> Result<Value<'gc>, Error> {
        Ok(match self {
            Value::Int(value) => Value::Int(-value),
            Value::Float(value) => Value::Float(-value),
            _ => return Err(format!("unsupported types for neg, {:?}", self)),
        })
    }

    pub fn not(&self) -> Value<'gc> {
        Value::Bool(!self.is_truthy())
    }

    pub fn eq(&self, other: &Value<'gc>) -> Value<'gc> {
        Value::Bool(self == other)
    }

    pub fn gt(&self, other: &Value<'gc>) -> Result<Value<'gc>, Error> {
        Ok(match (self, other) {
            (Value::Int(left), Value::Int(right)) => Value::Bool(left > right),
            (Value::Int(left), Value::Float(right)) => Value::Bool(*left as f64 > *right),
            (Value::Float(left), Value::Float(right)) => Value::Bool(left > right),
            (Value::Float(left), Value::Int(right)) => Value::Bool(*left > *right as f64),
            _ => {
                return Err(format!(
                    "unsupported types for gt, {:?} and {:?}",
                    self, other
                ))
            }
        })
    }

    pub fn ge(&self, other: &Value<'gc>) -> Result<Value<'gc>, Error> {
        Ok(match (self, other) {
            (Value::Int(left), Value::Int(right)) => Value::Bool(left >= right),
            (Value::Int(left), Value::Float(right)) => Value::Bool(*left as f64 >= *right),
            (Value::Float(left), Value::Float(right)) => Value::Bool(left >= right),
            (Value::Float(left), Value::Int(right)) => Value::Bool(*left >= *right as f64),
            _ => {
                return Err(format!(
                    "unsupported types for ge, {:?} and {:?}",
                    self, other
                ))
            }
        })
    }
}

impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Null, Value::Null) => true,
            (Value::Bool(left), Value::Bool(right)) => left == right,
            (Value::Int(left), Value::Int(right)) => left == right,
            (Value::Float(left), Value::Float(right)) => left == right,
            (Value::String(left), Value::String(right)) => left == right,

            (Value::Vec(left), Value::Vec(right)) => Gc::ptr_eq(*left, *right),
            (Value::Dict(left), Value::Dict(right)) => Gc::ptr_eq(*left, *right),
            (Value::Pair(left), Value::Pair(right)) => left == right,
            (Value::StructType(left), Value::StructType(right)) => Gc::ptr_eq(*left, *right),
            (Value::Struct(left), Value::Struct(right)) => Gc::ptr_eq(*left, *right),
            (Value::Any(left), Value::Any(right)) => Arc::ptr_eq(left, right),
            (Value::AnyMut(left), Value::AnyMut(right)) => Arc::ptr_eq(left, right),

            (Value::Closure(left), Value::Closure(right)) => {
                Gc::ptr_eq(left.function, right.function)
            }
            (Value::NativeFn(left), Value::NativeFn(right)) => left.function == right.function,
            (Value::VmFn(left), Value::VmFn(right)) => left.function == right.function,
            _ => false,
        }
    }
}

impl<'gc> std::fmt::Debug for Value<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Null => write!(f, "null"),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Int(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{:?}", value),
            Value::Pair(value) => write!(f, "{:?}", value),
            Value::Vec(vec) => write!(f, "{:?}", vec.borrow()),
            Value::Dict(dict) => write!(f, "{}", dict.borrow()),
            Value::StructType(struct_type) => write!(f, "{:?}", &struct_type.fields),
            Value::Struct(struct_) => write!(f, "{:?}", &struct_.as_ref().borrow().values),
            Value::Any(_) => write!(f, "<Any>"),
            Value::AnyMut(_) => write!(f, "<AnyMut>"),
            Value::Closure(closure) => write!(f, "<Closure {:?}>", closure),
            Value::NativeFn(_) => write!(f, "<NativeFn>"),
            Value::VmFn(_) => write!(f, "<VmFn>"),
        }
    }
}

impl<'gc> std::fmt::Display for Value<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Null => write!(f, "null"),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Int(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Pair(values) => {
                let f: &mut std::fmt::Formatter<'_> = f;
                write!(f, "(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, ")")
            }
            Value::Vec(vec) => {
                write!(f, "[")?;
                for (i, value) in vec.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Value::Dict(dict) => write!(f, "{}", dict.borrow()),
            Value::StructType(struct_type) => write!(f, "{}", struct_type),
            Value::Struct(struct_) => {
                let struct_ = struct_.as_ref().borrow();
                write!(f, "{}", struct_)
            }
            Value::Any(_) => write!(f, "<Any>"),
            Value::AnyMut(_) => write!(f, "<AnyMut>"),
            Value::Closure(c) => {
                if c.function.name.is_empty() {
                    write!(f, "<Closure>")
                } else {
                    write!(f, "<Closure {}>", &c.function.name)
                }
            }
            Value::NativeFn(_) => write!(f, "<NativeFn>"),
            Value::VmFn(_) => write!(f, "<VmFn>"),
        }
    }
}
