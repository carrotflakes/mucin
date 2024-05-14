use gc_arena::{lock::RefLock, Gc, Mutation};

use crate::{
    compile::NativeFn,
    string::{intern, Str},
    value::{Dict, Value},
};

pub fn types<'gc>(mc: &Mutation<'gc>) -> Vec<(Str, Value<'gc>)> {
    [
        "unit", "null", "int", "float", "string", "bool", "pair", "vec", "dict", "any", "closure",
        "nativeFn",
    ]
    .into_iter()
    .map(|name| {
        (
            intern(name),
            Value::Dict(Gc::new(
                mc,
                RefLock::new(Dict(vec![(intern("name"), Value::String(intern(name)))])),
            )),
        )
    })
    .collect()
}

pub fn std<'gc>() -> Vec<(Str, Value<'gc>)> {
    [
        ("typeof", &NF_TYPEOF),
        ("structType", &NF_STRUCT_TYPE),
        ("structTypeMethods", &NF_STRUCT_TYPE_METHODS),
        (
            "panic",
            &NativeFn {
                arity: 1,
                function: |_mc: &Mutation<'_>, args: &[Value<'_>]| -> Result<Value<'_>, String> {
                    Err(format!("panic: {}", args[0]))
                },
            },
        ),
        (
            "print",
            &NativeFn {
                arity: 1,
                function: |_mc: &Mutation<'_>, args: &[Value<'_>]| -> Result<Value<'_>, String> {
                    println!("{}", args[0]);
                    Ok(args[0].clone())
                },
            },
        ),
        (
            "toString",
            &NativeFn {
                arity: 1,
                function: |_mc: &Mutation<'_>, args: &[Value<'_>]| -> Result<Value<'_>, String> {
                    Ok(Value::String(intern(&format!("{}", args[0]))))
                },
            },
        ),
        (
            "newVec",
            &NativeFn {
                arity: 0,
                function: |mc: &Mutation<'_>, _: &[Value<'_>]| -> Result<Value<'_>, String> {
                    Ok(Value::Vec(Gc::new(mc, RefLock::new(Vec::<Value>::new()))))
                },
            },
        ),
        (
            "vecGet",
            &NativeFn {
                arity: 2,
                function: |_mc: &Mutation<'_>, args: &[Value<'_>]| -> Result<Value<'_>, String> {
                    match &args[0] {
                        Value::Vec(vec) => {
                            let index = match args[1] {
                                Value::Int(index) => index as usize,
                                _ => return Err("expected int".to_string()),
                            };
                            Ok(vec.borrow()[index].clone())
                        }
                        _ => return Err("expected vec".to_string()),
                    }
                },
            },
        ),
        (
            "vecSet",
            &NativeFn {
                arity: 3,
                function: |mc: &Mutation<'_>, args: &[Value<'_>]| -> Result<Value<'_>, String> {
                    let vec = match &args[0] {
                        Value::Vec(vec) => vec,
                        _ => return Err("expected vec".to_string()),
                    };
                    let index = match args[1] {
                        Value::Int(index) => index as usize,
                        _ => return Err("expected int".to_string()),
                    };
                    let value = args[2].clone();
                    vec.borrow_mut(mc)[index] = value;
                    Ok(Value::Unit)
                },
            },
        ),
        (
            "vecLen",
            &NativeFn {
                arity: 1,
                function: |_mc: &Mutation<'_>, args: &[Value<'_>]| -> Result<Value<'_>, String> {
                    match &args[0] {
                        Value::Vec(vec) => Ok(Value::Int(vec.borrow().len() as i64)),
                        _ => Err("expected vec".to_string()),
                    }
                },
            },
        ),
        ("vecPush", &NF_VEC_PUSH),
        ("vecPop", &NF_VEC_POP),
        ("vecInsert", &NF_VEC_INSERT),
        ("vecRemove", &NF_VEC_REMOVE),
        // (
        //     "import",
        //     1,
        //     |mc: &Mutation<'gc>, args: &[Value<'gc>]| -> Result<Value<'gc>, String> {
        //         let Some((global_variable_names, env)) =
        //             ENV_REF.with(|env_ref| env_ref.borrow().clone())
        //         else {
        //             return Err("import not initialized".to_string());
        //         };

        //         let path = match &args[0] {
        //             Value::String(path) => path,
        //             _ => return Err("expected string".to_string()),
        //         };
        //         let src = std::fs::read_to_string(path.as_str()).map_err(|e| e.to_string())?;

        //         let defs = crate::parser::parse(&src)?;

        //         let (names, env_initializers) = crate::compile::Builder::new(mc)
        //             .wrap_env(global_variable_names)
        //             .build_program(&defs)?;

        //         let env = Vm::new_env(mc, vec![env.clone()], env_initializers)?;
        //         let dict = Value::Dict(Gc::new(
        //             mc,
        //             RefLock::new(Dict(
        //                 names
        //                     .into_iter()
        //                     .zip(env.borrow().iter().cloned())
        //                     .collect(),
        //             )),
        //         ));
        //         Ok(dict)
        //     },
        // ),
    ]
    .into_iter()
    .map(|(name, nf)| (intern(name), Value::NativeFn(nf)))
    .collect()
}

// thread_local! {
//     pub static ENV_REF: std::cell::RefCell<Option<(Vec<Str>, Env<'static>)>> = std::cell::RefCell::new(None);
// }

// pub fn prepare_import(global_variable_names: Vec<Str>, env: Env) {
//     ENV_REF.with(|env_ref| {
//         *env_ref.borrow_mut() = Some((global_variable_names, env));
//     });
// }

pub fn nf_typeof<'gc>(_mc: &Mutation<'gc>, args: &[Value<'gc>]) -> Result<Value<'gc>, String> {
    Ok(match &args[0] {
        Value::Unit => Value::String(intern("unit")),
        Value::Null => Value::String(intern("null")),
        Value::Int(_) => Value::String(intern("int")),
        Value::Float(_) => Value::String(intern("float")),
        Value::String(_) => Value::String(intern("string")),
        Value::Bool(_) => Value::String(intern("bool")),
        Value::Pair(_) => Value::String(intern("pair")),
        Value::Vec(_) => Value::String(intern("vec")),
        Value::Dict(_) => Value::String(intern("dict")),
        Value::StructType(_) => Value::String(intern("structType")),
        Value::Struct(_) => Value::String(intern("struct")),
        Value::Any(_) => Value::String(intern("any")),
        Value::AnyMut(_) => Value::String(intern("anyMut")),
        Value::Closure(_) => Value::String(intern("closure")),
        Value::NativeFn(_) => Value::String(intern("nativeFn")),
        Value::VmFn(_) => Value::String(intern("vmFn")),
    })
}

pub fn nf_struct_type<'gc>(_mc: &Mutation<'gc>, args: &[Value<'gc>]) -> Result<Value<'gc>, String> {
    match &args[0] {
        Value::Struct(s) => Ok(Value::StructType(s.borrow().struct_type)),
        // TODO: implement for other types
        _ => Ok(Value::Null),
    }
}

fn nf_struct_type_methods<'gc>(
    _mc: &Mutation<'gc>,
    args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
    match &args[0] {
        Value::StructType(st) => Ok(Value::Dict(st.methods)),
        _ => Err("expected struct type".to_owned()),
    }
}

fn nf_index<'gc>(_: &Mutation, values: &[Value<'gc>]) -> Result<Value<'gc>, String> {
    Ok(match &values[0] {
        Value::Vec(v) => match &values[1] {
            Value::Int(i) => v.borrow().get(*i as usize).cloned().unwrap_or(Value::Unit),
            Value::String(s) if s.as_str() == "len" => Value::Int(v.borrow().len() as i64),
            _ => return Err(format!("indexing with non-integer value: {:?}", values[1])),
        },
        Value::Dict(d) => match &values[1] {
            Value::String(s) => d
                .borrow()
                .0
                .iter()
                .find(|(k, _)| k == s)
                .map(|(_, v)| v.clone())
                .unwrap_or(Value::Unit),
            _ => return Err(format!("indexing with non-string value: {:?}", values[1])),
        },
        Value::Struct(s) => match &values[1] {
            Value::String(k) => s
                .borrow()
                .get_by_str(k.as_str())
                .cloned()
                .unwrap_or(Value::Unit),
            _ => return Err(format!("indexing with non-string value: {:?}", values[1])),
        },
        Value::StructType(st) => match &values[1] {
            Value::String(k) => st.methods.borrow().get(k).cloned().unwrap_or(Value::Unit),
            _ => return Err(format!("indexing with non-string value: {:?}", values[1])),
        },
        _ => {
            return Err(format!(
                "indexing with non-indexable value: {:?}",
                values[0]
            ))
        }
    })
}

fn nf_field_assign<'gc>(mc: &Mutation<'gc>, values: &[Value<'gc>]) -> Result<Value<'gc>, String> {
    match &values[0] {
        Value::Dict(dict) => {
            let key = match &values[1] {
                Value::String(s) => s,
                _ => return Err("expected string".to_owned()),
            };
            let mut dict = dict.borrow_mut(mc);
            let value = values[2].clone();
            dict.set(key.clone(), value);
            Ok(Value::Unit)
        }
        Value::Vec(vec) => match &values[1] {
            Value::Int(index) => {
                let index = *index as usize;

                let mut vec = vec.borrow_mut(mc);
                let value = values[2].clone();
                vec[index] = value;
                Ok(Value::Unit)
            }
            Value::String(s) if s.as_str() == "len" => {
                let mut vec = vec.borrow_mut(mc);

                if let Value::Int(len) = values[2] {
                    if len < 0 {
                        return Err("negative length".to_owned());
                    }
                    vec.resize(len as usize, Value::Unit);
                    Ok(Value::Unit)
                } else {
                    Err(format!("expected int, got: {:?}", values[2]))
                }
            }
            _ => return Err("expected int or 'len'".to_owned()),
        },
        Value::Struct(s) => match &values[1] {
            Value::String(k) => {
                let mut s = s.borrow_mut(mc);
                let value = values[2].clone();
                s.set(k.clone(), value);
                Ok(Value::Unit)
            }
            _ => Err(format!("expected string, got: {:?}", values[1])),
        },
        _ => Err(format!("field assign to non-dict: {:?}", values[0])),
    }
}

pub static NF_TYPEOF: NativeFn = NativeFn {
    arity: 1,
    function: nf_typeof,
};

pub static NF_STRUCT_TYPE: NativeFn = NativeFn {
    arity: 1,
    function: nf_struct_type,
};

pub static NF_STRUCT_TYPE_METHODS: NativeFn = NativeFn {
    arity: 1,
    function: nf_struct_type_methods,
};

pub static NF_INDEX: NativeFn = NativeFn {
    arity: 2,
    function: nf_index,
};

pub static NF_FIELD_ASSIGN: NativeFn = NativeFn {
    arity: 3,
    function: nf_field_assign,
};

fn nf_vec_push<'gc>(mc: &Mutation<'gc>, values: &[Value<'gc>]) -> Result<Value<'gc>, String> {
    match &values[0] {
        Value::Vec(vec) => {
            let value = values[1].clone();
            vec.borrow_mut(mc).push(value);
            Ok(Value::Unit)
        }
        _ => Err("expected vec".to_owned()),
    }
}

fn nf_vec_pop<'gc>(mc: &Mutation<'gc>, values: &[Value<'gc>]) -> Result<Value<'gc>, String> {
    match &values[0] {
        Value::Vec(vec) => {
            let value = vec.borrow_mut(mc).pop();
            Ok(value.unwrap_or(Value::Null))
        }
        _ => Err("expected vec".to_owned()),
    }
}

fn nf_vec_insert<'gc>(mc: &Mutation<'gc>, values: &[Value<'gc>]) -> Result<Value<'gc>, String> {
    match &values[0] {
        Value::Vec(vec) => {
            let index = match &values[1] {
                Value::Int(index) => *index as usize,
                _ => return Err("expected int".to_owned()),
            };
            let value = values[2].clone();
            vec.borrow_mut(mc).insert(index, value);
            Ok(Value::Unit)
        }
        _ => Err("expected vec".to_owned()),
    }
}

fn nf_vec_remove<'gc>(mc: &Mutation<'gc>, values: &[Value<'gc>]) -> Result<Value<'gc>, String> {
    match &values[0] {
        Value::Vec(vec) => {
            let index = match &values[1] {
                Value::Int(index) => *index as usize,
                _ => return Err("expected int".to_owned()),
            };
            let value = vec.borrow_mut(mc).remove(index);
            Ok(value)
        }
        _ => Err("expected vec".to_owned()),
    }
}

pub static NF_VEC_PUSH: NativeFn = NativeFn {
    arity: 2,
    function: nf_vec_push,
};

pub static NF_VEC_POP: NativeFn = NativeFn {
    arity: 1,
    function: nf_vec_pop,
};

pub static NF_VEC_INSERT: NativeFn = NativeFn {
    arity: 3,
    function: nf_vec_insert,
};

pub static NF_VEC_REMOVE: NativeFn = NativeFn {
    arity: 2,
    function: nf_vec_remove,
};
