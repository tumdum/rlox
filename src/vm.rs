use crate::chunk::InvalidOpCode;
use crate::compiler::Parser;
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
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Default)]
pub struct VM {
    chunk: Chunk,
    pc: usize,
    stack: VecDeque<Value>,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        let mut ret = Self::default();
        ret.set_chunk(chunk);
        ret
    }

    pub fn set_chunk(&mut self, chunk: Chunk) {
        self.chunk = chunk;
        self.pc = 0;
        self.stack.clear();
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

    pub fn repl(&mut self) -> Result<(), Error> {
        loop {
            print!("> ");
            std::io::stdout().flush().unwrap();
            let mut line = String::new();
            std::io::stdin().read_line(&mut line)?;

            self.interpret(&line).unwrap();
        }
    }

    pub fn run_file(&mut self, path: &PathBuf) -> Result<(), Error> {
        let source = std::fs::read_to_string(path)?;
        self.interpret(&source)
    }

    fn interpret(&mut self, source: &str) -> Result<(), Error> {
        let parser = Parser::new(source);
        let chunk = parser.compile().unwrap();

        self.set_chunk(chunk);

        self.run()
    }
}
