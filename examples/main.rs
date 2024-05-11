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

    let loader = |path: &str| {
        let path = if !path.is_empty() {
            let name = path.split("/").last().unwrap();
            std::path::Path::new(&args.path)
                .parent()
                .unwrap()
                .join(format!("{}.mc", name))
        } else {
            std::path::Path::new(&args.path).to_path_buf()
        };

        println!("load: {:?}", &path);
        std::fs::read_to_string(&path).map_err(|e| e.to_string())
    };

    let mut runtime = Runtime::new();

    runtime.push_env_from_files(loader, "".to_string()).unwrap();
    runtime
        .call_fn("main", |_| vec![], |_, value| println!("{}", value))
        .unwrap();
}
