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

fn main() -> Result<(), vm::Error> {
    let opt = Opt::from_args();
    let mut vm = VM::default();

    match opt.script {
        None => vm.repl(),
        Some(script) => vm.run_file(&script),
    }
}
