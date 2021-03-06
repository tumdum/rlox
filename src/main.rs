mod allocator;
mod chunk;
mod compiler;
mod debug;
mod scanner;
mod value;
mod vm;

use crate::vm::VM;
use chunk::{Chunk, OpCode};
use std::cell::RefCell;
use std::io::{stdin, stdout, BufReader};
use std::path::PathBuf;
use std::rc::Rc;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "rlox")]
struct Opt {
    /// Path to script to execute
    #[structopt(parse(from_os_str))]
    script: Option<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();
    let mut vm = VM::new(
        Rc::new(RefCell::new(stdout())),
        Rc::new(RefCell::new(BufReader::new(stdin()))),
    );

    vm.load_prelude().unwrap();
    vm.register_bulitins();

    let result = match opt.script {
        None => vm.repl(),
        Some(script) => vm.run_file(&script),
    };
    match result {
        Ok(_) => {}
        Err(vm::Error::RuntimeError(e)) => {
            println!("{}", e);
            e.print_callstack(std::io::stdout());
        }
        Err(other) => {
            println!("{}", other);
        }
    }
}
