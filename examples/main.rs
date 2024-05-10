use clap::Parser;
use mucin::runtime::Runtime;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Debug mode
    #[arg(short, long, default_value = "false")]
    debug: bool,

    /// Path to the file to run
    path: String,
}

fn main() {
    let args = Args::parse();

    let src = std::fs::read_to_string(&args.path).expect("failed to read file");

    let mut runtime = Runtime::new();
    // if args.debug {
    //     runtime = runtime.debug();
    // }

    // native_fns::prepare_import(runtime.root_var_names.clone(), runtime.root_env.clone());
    runtime.push_env_from_src(&src).unwrap();
    runtime
        .call_fn("main", |_| vec![], |_, value| println!("{}", value))
        .unwrap();
}
