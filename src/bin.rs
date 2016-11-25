use std::{env, path, process};
use std::fs::File;

mod client;

fn main() {
    let mut args = env::args();
    let arg0 = args.next();
    let arg1 = args.next();

    if let None = arg1 {
        println!("usage: {} rom_file", arg0.unwrap());
        process::exit(1);
    }

    let filename = arg1.unwrap();
    let mut f = match File::open(path::Path::new(&filename)) {
        Ok(f) => f,
        Err(e) => {
            println!("Failed to open {}: {}", filename, e);
            process::exit(1)
        }
    };

    let mut cli = client::cli::Cli::new();
    if !cli.run(&mut f) {
        process::exit(1)
    }
}
