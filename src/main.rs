mod chunk;
mod compiler;
mod debug;
mod scanner;
mod value;
mod vm;

use crate::vm::{repl, run_file, VM};
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

    match opt.script {
        None => repl(),
        Some(script) => run_file(&script),
    };
}
