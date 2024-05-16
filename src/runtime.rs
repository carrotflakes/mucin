use std::sync::Arc;

use gc_arena::{lock::RefLock, Arena, Collect, Gc, Mutation, Rootable};

use crate::{
    compile, continuation, native_fns, parser,
    string::{compact_str_pool, intern},
    value::{Closure, Dict, Struct, StructType, Value},
    vm::{Env, Vm},
};

pub struct Runtime(Arena<Rootable![RuntimeInner<'_>]>);

impl Runtime {
    pub fn new() -> Self {
        Self(Arena::new(|mc| {
            let mut inner = RuntimeInner::new(mc);

            inner.push_env(mc, |_| {
                let mut v: Vec<_> = native_fns::std()
                    .into_iter()
                    .map(|(k, v)| ((k, false), v))
                    .collect();
                v.push((
                    (intern("calljp"), false),
                    Value::VmFn(&continuation::VF_CALLJP),
                ));
                v.push(((intern("jump"), false), Value::VmFn(&continuation::VF_JUMP)));
                v
            });
            let env = inner.push_env_from_src(mc, SRC).unwrap();
            inner.method_call_fn = env
                .borrow()
                .get_by_str("callMethod")
                .unwrap()
                .as_closure()
                .cloned();

            inner
        }))
    }

    pub fn arena(&self) -> &Arena<Rootable![RuntimeInner<'_>]> {
        &self.0
    }

    pub fn push_env(
        &mut self,
        new_env: impl for<'a> FnMut(&Mutation<'a>) -> Vec<((Arc<String>, bool), Value<'a>)>,
    ) {
        self.0.mutate_root(|mc, inner| {
            inner.push_env(mc, new_env);
        })
    }

    pub fn push_env_from_src(&mut self, src: &str) -> Result<(), String> {
        self.0.mutate_root(|mc, inner| {
            inner.push_env_from_src(mc, src)?;
            Ok(())
        })
    }

    pub fn push_env_from_files(
        &mut self,
        loader: impl FnMut(&str) -> Result<String, String>,
        root_path: String,
    ) -> Result<(), String> {
        self.0.mutate_root(|mc, inner| {
            let mut rep = crate::multi_file_builder::Repository::load(mc, loader, root_path)
                .map_err(|e| match e {
                    crate::multi_file_builder::Error::ParseError(e) => e,
                    crate::multi_file_builder::Error::LoadError(e) => e,
                })?;
            let env = rep.build(&inner.envs, inner.method_call_fn.clone())?;
            inner.envs.push(env);
            Ok(())
        })
    }

    pub fn pop_env(&mut self) {
        self.0.mutate_root(|_, inner| {
            inner.pop();
        });
        self.gc();
    }

    pub fn call_fn<T>(
        &mut self,
        name: &str,
        args_builder: impl for<'a> FnOnce(&Mutation<'a>) -> Vec<Value<'a>>,
        callback: impl for<'a> FnOnce(&Mutation<'a>, &Value<'a>) -> T,
    ) -> Result<T, String> {
        let res = self.0.mutate_root(|mc, inner| {
            let args = args_builder(mc);

            let closure = inner
                .get_value(name)
                .ok_or("function not found")?
                .as_closure()
                .ok_or("not a function")?
                .clone();
            let res = Vm::new(mc).run_closure_with_args(closure, args)?;
            let res = callback(mc, &res);
            Ok(res)
        });
        self.gc();
        res
    }

    pub fn get_value<T>(
        &mut self,
        name: &str,
        callback: impl for<'a> FnOnce(&Mutation<'a>, &Value<'a>) -> T,
    ) -> Result<T, String> {
        let res = self.0.mutate_root(|mc, inner| {
            let value = inner.get_value(name).ok_or("value not found")?;
            Ok(callback(mc, &value))
        });
        self.gc();
        res
    }

    pub fn gc(&mut self) {
        self.0.collect_all();
        compact_str_pool();
    }

    pub fn metrics(&self) -> String {
        let metrics = self.0.metrics();
        format!(
            "total_gc_allocation: {}, total_external_allocation: {}, total_allocation: {}, str_pool_size: {}",
            metrics.total_gc_allocation(),
            metrics.total_external_allocation(),
            metrics.total_allocation(),
            crate::string::STR_POOL.with(|pool| pool.borrow().size()),
        )
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct RuntimeInner<'gc> {
    envs: Vec<Env<'gc>>,
    method_call_fn: Option<Closure<'gc>>,
    last_result: Option<Value<'gc>>,
}

impl<'gc> RuntimeInner<'gc> {
    pub fn new(_: &'gc Mutation<'gc>) -> Self {
        Self {
            envs: vec![],
            method_call_fn: None,
            last_result: None,
        }
    }

    pub fn push(&mut self, env: Env<'gc>) {
        self.envs.push(env);
    }

    pub fn pop(&mut self) -> Option<Env<'gc>> {
        self.envs.pop()
    }

    pub fn push_env(
        &mut self,
        mc: &Mutation<'gc>,
        mut new_env: impl for<'a> FnMut(&Mutation<'a>) -> Vec<((Arc<String>, bool), Value<'a>)>,
    ) -> Env<'gc> {
        let env = new_env(mc);
        let names: Vec<_> = env.iter().map(|(name, _)| name.clone()).collect();
        let env: Vec<_> = env.into_iter().map(|(_, value)| value).collect();
        let struct_type = Gc::new(
            mc,
            StructType {
                name: intern(""),
                fields: names.into_boxed_slice(),
                methods: Gc::new(mc, RefLock::new(Dict::default())),
            },
        );
        let env = Gc::new(
            mc,
            RefLock::new(Struct {
                struct_type,
                values: env.into_boxed_slice(),
            }),
        );
        self.push(env);
        env
    }

    pub fn push_env_from_src(
        &mut self,
        mc: &'gc Mutation<'gc>,
        src: &str,
    ) -> Result<Env<'gc>, String> {
        let defs = parser::parse(src)?;

        let mut builder = compile::Builder::new(mc)
            .wrap_envs(
                self.envs
                    .iter()
                    .map(|env| env.borrow().struct_type.fields.to_vec())
                    .collect(),
            )
            .use_method_call_fn(self.method_call_fn.clone());
        let env_map = builder.build_program(&defs)?;
        let struct_type = Gc::new(
            mc,
            StructType {
                name: intern(""),
                fields: env_map.into_boxed_slice(),
                methods: Gc::new(mc, RefLock::new(Dict::default())),
            },
        );

        let env =
            Vm::new(mc).make_env(self.envs.clone(), struct_type, builder.into_instructions())?;

        self.push(env);

        Ok(env)
    }

    pub fn get_value(&self, name: &str) -> Option<Value<'gc>> {
        for env in self.envs.iter().rev() {
            if let Some(value) = env.borrow().get_by_str(name) {
                return Some(value.clone());
            }
        }
        None
    }
}

const SRC: &str = r#"
struct Vec {}

fn Vec push(self, value) vecPush(self, value)
fn Vec pop(self) vecPop(self)
fn Vec insert(self, index, value) vecInsert(self, index, value)
fn Vec remove(self, index) vecRemove(self, index)
fn Vec map(self, f) {
    let ret = [];
    var i = 0;
    while i < self.len {
        vecPush(ret, f(self[i], i));
        i += 1;
    };
    ret
}
fn Vec it(self) {
    var i = 0;
    || if i < self.len {
        i += 1;
        self[i - 1]
    }
}

fn callMethod(args, obj, key) {
    var st = structType(obj);
    if !st && typeof(obj) == "vec" {
        st = Vec;
    };
    if st && typeof(key) == "string" {
        let method = structTypeMethods(st)[key];
        if method {
            return method(obj, ..args);
        }
    };
    obj[key](..args)
}"#;

#[test]
fn test() {
    let mut rt = Runtime::new();
    rt.push_env_from_src("let dict = {a: 1};").unwrap();
    rt.get_value("dict", |mc, value| {
        value
            .as_dict()
            .unwrap()
            .borrow_mut(mc)
            .set(intern("a"), Value::Int(2));
    })
    .unwrap();
    rt.get_value("dict", |mc, value| {
        assert_eq!(
            value.as_dict().unwrap().borrow_mut(mc).get_by_str("a"),
            Some(&Value::Int(2))
        );
    })
    .unwrap();
    rt.pop_env();

    // Runtime::call_fn test
    rt.push_env_from_src(r#"fn main(a, b) [a, b]"#).unwrap();
    rt.call_fn(
        "main",
        |_| vec![Value::Int(0), Value::Int(1)],
        |_, v| {
            assert_eq!(format!("{:?}", v), "[0, 1]");
        },
    )
    .unwrap();
}
