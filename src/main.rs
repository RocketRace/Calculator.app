use std::{
    io::{stdin, Read},
    process::exit,
};

use clap::Parser;
use interpreter::ProgramCompletion;

/// The Calculator.app interpreter.
///
/// When running a finished program, the `-lqr` flags are customary.
#[derive(Parser)]
struct Args {
    /// Run the program in a loop on a persistent input stack
    #[arg(short, long = "loop")]
    looped: bool,
    /// Enable verbose mode
    #[arg(short = 'V', long)]
    verbose: bool,
    /// Suppress error messages
    #[arg(short, long)]
    quiet: bool,
    /// Print the topmost value on the stack on exit
    #[arg(short, long)]
    result: bool,
    /// Input file to read program from. Defaults to standard input
    file: Option<String>,
    /// Initial stack contents (doubles). The elements are pushed to the stack in the given order
    nums: Vec<f64>,
}

fn end_execution(completion: ProgramCompletion, quiet: bool, result: bool) -> ! {
    if !quiet {
        eprintln!("{}", completion.message);
    }
    if result {
        let value = match completion.state.stack.peek_either() {
            Ok(n) => n.to_string(),
            Err(n) => n.to_string(),
        };
        println!("{value}")
    }
    exit(completion.code)
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
        Err(()) => {
            eprintln!("Unable to parse: Invalid floating point numbers in arguments");
            exit(2)
        }
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
                end_execution(completion, args.quiet, args.result)
            } else {
                match interpreter::exec_once(&program, input, args.file.as_deref(), args.verbose) {
                    Err(completion) => end_execution(completion, args.quiet, args.result),
                    Ok(state) => {
                        if args.result {
                            let value = match state.stack.peek_either() {
                                Ok(n) => n.to_string(),
                                Err(n) => n.to_string(),
                            };
                            println!("{value}")
                        }
                        exit(0)
                    }
                }
            }
        }
    }
}
