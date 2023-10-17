use std::{
    io::{stdin, Read},
    process::exit,
};

use clap::Parser;

#[derive(Parser)]
struct Args {
    /// Input file to read program from. Defaults to standard input.
    input: Option<String>,
}

fn main() {
    let args = Args::parse();

    let result = if let Some(filename) = args.input {
        std::fs::read_to_string(&filename)
            .map_err(|e| format!("Failed to read file contents from {filename}\n{e}"))
    } else {
        let mut program = String::new();
        stdin()
            .read_to_string(&mut program)
            .map(|_| program)
            .map_err(|e| format!("Failed to read file contents from standard input\n{e}"))
    };
    if let Err(e) = result.and_then(|program| interpreter::exec(&program)) {
        eprint!("{e}");
        exit(1)
    };
}
