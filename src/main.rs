use std::{
    io::{stdin, Read},
    process::exit,
};

use clap::Parser;

#[derive(Parser)]
struct Args {
    /// Run the program in a loop on a persistent input stack
    #[arg(short, long = "loop")]
    looped: bool,
    /// Enable verbose mode
    #[arg(short = 'V', long)]
    verbose: bool,
    /// Input file to read program from. Defaults to standard input
    file: Option<String>,
    /// Initial stack contents (doubles). The elements are pushed to the stack in the given order
    nums: Vec<f64>,
}

fn exit_with_message(msg: &str, code: i32) -> ! {
    eprintln!("{msg}");
    exit(code)
}

fn main() {
    let args = Args::parse();

    let result = if let Some(filename) = &args.file {
        std::fs::read_to_string(filename)
            .map_err(|e| format!("Failed to read file contents from {filename}\n{e}"))
    } else {
        let mut program = String::new();
        stdin()
            .read_to_string(&mut program)
            .map(|_| program)
            .map_err(|e| format!("Failed to read file contents from standard input\n{e}"))
    };

    let input = match args
        .nums
        .into_iter()
        .map(|x| if x.is_finite() { Ok(x) } else { Err(()) })
        .collect::<Result<_, _>>()
    {
        Ok(x) => x,
        Err(()) => exit_with_message("Invalid floating point numbers in input", 1),
    };

    match result {
        Err(e) => {
            println!("{e}");
            exit(1)
        }
        Ok(program) => {
            if args.looped {
                let completion =
                    interpreter::exec_loop(&program, input, args.file.as_deref(), args.verbose);
                exit_with_message(&completion.message, completion.code)
            } else if let Err(completion) =
                interpreter::exec_once(&program, input, args.file.as_deref(), args.verbose)
            {
                exit_with_message(&completion.message, completion.code)
            }
        }
    }
}
