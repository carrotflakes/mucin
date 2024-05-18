use gc_arena::{lock::RefLock, Collect, Gc, Mutation};

use crate::string::Str;

use super::{Dict, Value};

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

    pub fn set(&mut self, key: Str, value: Value<'gc>) -> Result<(), String> {
        if let Some(i) = self
            .struct_type
            .fields
            .iter()
            .position(|field| field.0 == key)
        {
            if !self.struct_type.fields[i].1 {
                return Err(format!("Cannot set immutable field {}", key));
            }

            self.values[i] = value;
        }
        Ok(())
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
