use std::{collections::HashMap, sync::Arc};

use gc_arena::{lock::RefLock, Gc, Mutation};

use crate::{
    value::{Closure, Dict, StructType, Value},
    vm::{Env, Vm},
};

pub struct Repository<'gc> {
    mc: &'gc Mutation<'gc>,
    files: Vec<File>,
    root_path: Arc<String>,
}

pub struct File {
    pub path: Arc<String>,
    pub defines: Vec<crate::model::Definition>,
}

pub struct BuildResult<'gc> {
    pub env: Env<'gc>,
}

#[derive(Debug)]
pub enum Error<E> {
    ParseError(String),
    LoadError(E),
}

impl<'gc> Repository<'gc> {
    pub fn load<E>(
        mc: &'gc Mutation<'gc>,
        mut loader: impl FnMut(&str) -> Result<String, E>,
        root_path: String,
    ) -> Result<Self, Error<E>> {
        let root_path = Arc::new(root_path);
        let mut files = vec![];
        let mut queue = vec![root_path.clone()];
        while let Some(path) = queue.pop() {
            let src = loader(path.as_str()).map_err(Error::LoadError)?;
            let defs = crate::parser::parse(&src).map_err(Error::ParseError)?;

            for def in &defs {
                if let crate::model::Definition::Module(name) = def {
                    let path = Arc::new(format!("{}/{}", path, name));
                    if !queue.contains(&path) {
                        queue.push(path.clone());
                    }
                }
            }

            files.push(File {
                path: path.clone(),
                defines: defs,
            });
        }
        Ok(Self {
            mc,
            files,
            root_path,
        })
    }

    pub fn build(
        &mut self,
        outer_envs: &[Env<'gc>],
        method_call_fn: Option<Closure<'gc>>,
    ) -> Result<Env<'gc>, String> {
        let mut build_results: HashMap<Arc<String>, BuildResult> = HashMap::new();

        for file in self.files.iter().rev() {
            let mut builder = crate::compile::Builder::new(self.mc)
                .wrap_envs(
                    outer_envs
                        .iter()
                        .map(|env| env.borrow().struct_type.fields.to_vec())
                        .collect(),
                )
                .use_method_call_fn(method_call_fn.clone());

            let env_map = builder.build_program(&file.defines)?;
            let struct_type = Gc::new(
                self.mc,
                StructType {
                    name: file.path.clone(),
                    fields: env_map.into_boxed_slice(),
                    methods: Gc::new(self.mc, RefLock::new(Dict::default())),
                },
            );

            let env = Vm::new(self.mc).make_env(
                outer_envs.to_vec(),
                struct_type,
                builder.into_instructions(),
            )?;

            // Inject modules
            for (i, def) in file.defines.iter().enumerate() {
                if let crate::model::Definition::Module(name) = def {
                    let path = Arc::new(format!("{}/{}", file.path, name));
                    let dict = build_results.get(&path).unwrap().env;
                    env.borrow_mut(self.mc).values[i] = Value::Struct(dict);
                }
            }

            build_results.insert(file.path.clone(), BuildResult { env });
        }

        Ok(build_results[&self.root_path].env.clone())
    }

    pub fn all_files(&self) -> impl Iterator<Item = &File> {
        self.files.iter()
    }

    pub fn root(&self) -> &File {
        self.files
            .iter()
            .find(|file| file.path.as_str() == self.root_path.as_str())
            .unwrap()
    }
}

#[test]
fn test() {
    use gc_arena::{Arena, Rootable};

    Arena::<Rootable![()]>::new(|mc| {
        let a = r#"
let hello = "Hello";
"#;
        let b = r#"
mod a;

let world = "World";
"#;
        let c = r#"
mod b;

fn main() b.a.hello + b.world
"#;

        let loader = |path: &str| -> Result<String, String> {
            match path {
                "main" => Ok(c.to_string()),
                "main/b" => Ok(b.to_string()),
                "main/b/a" => Ok(a.to_string()),
                _ => Err(format!("file not found: {}", path)),
            }
        };
        let mut repo = Repository::load(mc, loader, "main".to_string()).unwrap();
        let env = repo.build(&[], None).unwrap();

        let main_closure = env
            .borrow()
            .get_by_str("main")
            .unwrap()
            .as_closure()
            .unwrap()
            .clone();
        dbg!(Vm::new(mc).run_closure(main_closure).unwrap());
        ()
    });
}
