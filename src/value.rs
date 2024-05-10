use std::sync::{Arc, Mutex};

use gc_arena::{lock::RefLock, Collect, Gc, Mutation};

use crate::{
    compile::{Function, NativeFn},
    string::Str,
};

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

    Tuple(Box<[Value<'gc>]>),
    Vec(Gc<'gc, RefLock<Vec<Value<'gc>>>>),
    Dict(Gc<'gc, RefLock<Dict<'gc>>>),
    StructType(Gc<'gc, StructType<'gc>>),
    Struct(Gc<'gc, RefLock<Struct<'gc>>>),

    Any(#[collect(require_static)] Arc<Mutex<dyn std::any::Any>>),

    Closure(Closure<'gc>),
    NativeFn(&'static NativeFn),
}

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
pub struct Closure<'gc> {
    pub function: Gc<'gc, Function<'gc>>,
    pub capture: Box<[Gc<'gc, RefLock<Struct<'gc>>>]>,
}

#[derive(Collect, Debug, Clone, Default)]
#[collect(no_drop)]
pub struct Dict<'gc>(pub Vec<(Str, Value<'gc>)>);

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
pub struct StructType<'gc> {
    pub name: Str,
    pub fields: Box<[(Str, bool)]>,
    pub methods: Gc<'gc, RefLock<Dict<'gc>>>,
}

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
pub struct Struct<'gc> {
    pub struct_type: Gc<'gc, StructType<'gc>>,
    pub values: Box<[Value<'gc>]>,
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
            Value::Float(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_int_or_float(&self) -> Option<f64> {
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

    pub fn as_any(&self) -> Option<&Arc<Mutex<dyn std::any::Any>>> {
        match self {
            Value::Any(value) => Some(value),
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
            (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
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
            (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
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
            (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
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
            (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
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
            (Value::Float(left), Value::Float(right)) => Value::Float(left % right),
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
            (Value::Float(left), Value::Float(right)) => Value::Bool(left > right),
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
            (Value::Float(left), Value::Float(right)) => Value::Bool(left >= right),
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
            (Value::Tuple(left), Value::Tuple(right)) => left == right,
            (Value::StructType(left), Value::StructType(right)) => Gc::ptr_eq(*left, *right),
            (Value::Struct(left), Value::Struct(right)) => Gc::ptr_eq(*left, *right),
            (Value::Any(left), Value::Any(right)) => Arc::ptr_eq(left, right),

            (Value::NativeFn(left), Value::NativeFn(right)) => left.function == right.function,
            (Value::Closure(left), Value::Closure(right)) => {
                Gc::ptr_eq(left.function, right.function)
            }
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
            Value::Tuple(value) => write!(f, "{:?}", value),
            Value::Vec(vec) => write!(f, "{:?}", vec.borrow()),
            Value::Dict(dict) => write!(f, "{:?}", dict.borrow()),
            Value::StructType(struct_type) => write!(f, "{:?}", &struct_type.fields),
            Value::Struct(struct_) => write!(f, "{:?}", &struct_.as_ref().borrow().values),
            Value::Any(_) => write!(f, "<any>"),
            Value::Closure(closure) => write!(f, "<closure {:?}>", closure),
            Value::NativeFn(_) => write!(f, "<native_fn>"),
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
            Value::Tuple(values) => {
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
            Value::Any(_) => write!(f, "<any>"),
            Value::Closure(c) => {
                if c.function.name.is_empty() {
                    write!(f, "<closure>")
                } else {
                    write!(f, "<closure {}>", &c.function.name)
                }
            }
            Value::NativeFn(_) => write!(f, "<native_fn>"),
        }
    }
}

impl<'gc> Dict<'gc> {
    pub fn get(&self, key: &Str) -> Option<&Value<'gc>> {
        self.0
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    pub fn get_by_str(&self, key: &str) -> Option<&Value<'gc>> {
        self.0
            .iter()
            .find_map(|(k, v)| if k.as_str() == key { Some(v) } else { None })
    }

    pub fn set(&mut self, key: Str, value: Value<'gc>) {
        if let Some(e) = self.0.iter_mut().find(|(k, _)| k == &key) {
            e.1 = value;
        } else {
            self.0.push((key, value));
        }
    }

    pub fn entries(&self) -> impl Iterator<Item = (&Str, &Value<'gc>)> {
        self.0.iter().map(|(k, v)| (k, v))
    }
}

impl<'gc> std::fmt::Display for Dict<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}

impl<'gc> Struct<'gc> {
    pub fn from_type(
        mc: &Mutation<'gc>,
        struct_type: Gc<'gc, StructType<'gc>>,
    ) -> Gc<'gc, RefLock<Self>> {
        Gc::new(
            mc,
            RefLock::new(Struct {
                struct_type,
                values: struct_type.fields.iter().map(|_| Value::Unit).collect(),
            }),
        )
    }

    pub fn get(&self, key: &Str) -> Option<&Value<'gc>> {
        self.values
            .iter()
            .zip(self.struct_type.fields.iter())
            .find_map(|(value, field)| if &field.0 == key { Some(value) } else { None })
    }

    pub fn get_by_str(&self, key: &str) -> Option<&Value<'gc>> {
        self.values
            .iter()
            .zip(self.struct_type.fields.iter())
            .find_map(|(value, field)| {
                if field.0.as_str() == key {
                    Some(value)
                } else {
                    None
                }
            })
    }

    pub fn set(&mut self, key: Str, value: Value<'gc>) {
        if let Some(i) = self
            .struct_type
            .fields
            .iter()
            .position(|field| field.0 == key)
        {
            self.values[i] = value;
        }
    }
}

impl std::fmt::Display for StructType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        write!(f, "{{")?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field.0)?;
        }
        write!(f, "}}")
    }
}

impl std::fmt::Display for Struct<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.struct_type.name)?;
        write!(f, "{{")?;
        for (i, value) in self.values.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", &self.struct_type.fields[i].0, value)?;
        }
        write!(f, "}}")
    }
}
