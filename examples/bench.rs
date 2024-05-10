use mucin::runtime::Runtime;

fn main() {
    let t = std::time::Instant::now();

    for _ in 0..10 {
        #[allow(unused)]
        enum Tree {
            Leaf,
            Node(Box<Tree>, Box<Tree>),
        }

        fn f(i: i32) -> Tree {
            if i == 0 {
                Tree::Leaf
            } else {
                Tree::Node(Box::new(f(i - 1)), Box::new(f(i - 1)))
            }
        }

        std::hint::black_box(f(20));
    }

    println!("{:?}", t.elapsed());

    let t = std::time::Instant::now();

    for _ in 0..10 {
        let mut runtime = Runtime::new();
        runtime.push_env_from_src(SRC).unwrap();
        runtime.call_fn("main", |_| vec![], |_, _| {}).unwrap();
    }

    println!("{:?}", t.elapsed());
}

const SRC: &str = r#"
fn main(): f(20)

fn f(i):
    if i == 0:
        ()
    else:
        {
            left: f(i - 1),
            right: f(i - 1),
        }
"#;
