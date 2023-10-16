use std::{
    io::{stdin, Read},
    process::exit,
};

fn main() {
    let mut program = String::new();
    if let Err(e) = stdin().read_to_string(&mut program) {
        panic!("Error reading program: {e}\n")
    }

    if let Err(message) = interpreter::exec(&program) {
        eprintln!("{message}");
        exit(1)
    }
}
