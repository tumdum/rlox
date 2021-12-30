use crate::chunk::InvalidOpCode;
use crate::compiler::compile;
use crate::value::Value;
use crate::{Chunk, OpCode};
use std::collections::VecDeque;
use std::io::Write;
use std::path::PathBuf;
use thiserror::Error;

macro_rules! binary_op {
    ($target: expr, $op: tt) => {{
        let b = $target.stack.pop_back().unwrap().0;
        let a = $target.stack.pop_back().unwrap().0;
        let result = a $op b;
        $target.stack.push_back(result.into());
    }}
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Run time error: {0}")]
    InvalidOpCode(#[from] InvalidOpCode),
}

pub struct VM {
    chunk: Chunk,
    pc: usize,
    stack: VecDeque<Value>,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            pc: 0,
            stack: VecDeque::new(),
        }
    }

    fn read_byte(&mut self) -> u8 {
        let ret = self.chunk.code[self.pc];
        self.pc += 1;
        ret
    }

    fn read_opcode(&mut self) -> Result<OpCode, InvalidOpCode> {
        self.read_byte().try_into()
    }

    fn read_constant(&mut self) -> Value {
        let constant_index = self.read_byte() as usize;
        self.chunk.constants[constant_index].clone()
    }

    fn trace(&self) {
        println!("          {:?}", self.stack);
        self.chunk.disassemble_instruction(self.pc);
    }

    pub fn run(&mut self) -> Result<(), Error> {
        loop {
            #[cfg(debug_assertions)]
            self.trace();

            match self.read_opcode()? {
                OpCode::Return => {
                    let value = self.stack.pop_back().unwrap();
                    println!("{:?}", value);
                    return Ok(());
                }
                OpCode::Add => {
                    binary_op!(self, +);
                }
                OpCode::Subtract => {
                    binary_op!(self, -);
                }
                OpCode::Multiply => {
                    binary_op!(self, *);
                }
                OpCode::Divide => {
                    binary_op!(self, /);
                }
                OpCode::Negate => {
                    let constant = self.stack.pop_back().unwrap();
                    self.stack.push_back((-constant.0).into());
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.stack.push_back(constant);
                }
            }
        }
    }
}

pub fn repl() -> Result<(), std::io::Error> {
    loop {
        print!("> ");
        std::io::stdout().flush();
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;

        interpret(&line);
    }
}

pub fn run_file(path: &PathBuf) -> Result<(), std::io::Error> {
    let source = std::fs::read_to_string(path)?;
    interpret(&source)
}

fn interpret(source: &str) -> Result<(), std::io::Error> {
    compile(source);
    Ok(())
}
