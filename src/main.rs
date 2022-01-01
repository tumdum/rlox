mod allocator;
mod chunk;
mod compiler;
mod debug;
mod scanner;
mod value;
mod vm;

use crate::vm::VM;
use chunk::{Chunk, OpCode};
use std::path::PathBuf;
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
    let mut vm = VM::default();

    let result = match opt.script {
        None => vm.repl(),
        Some(script) => vm.run_file(&script),
    };
    if let Err(e) = result {
        println!("Final result: {}", e);
    }
}
