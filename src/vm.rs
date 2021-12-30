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
        let b = match $target.pop()? {
            Value::Number(b) => b,
            other => return Err(Error::InvalidOperand("Number".to_owned(), other)),
        };
        let a = match $target.pop()? {
            Value::Number(a) => a,
            other => return Err(Error::InvalidOperand("Number".to_owned(), other)),
        };

        let result = a $op b;
        $target.push(result.into());
    }}
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Run time error: {0}")]
    InvalidOpCode(#[from] InvalidOpCode),
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Invalid operand, expected {0}, got {1:?}")]
    InvalidOperand(String, Value),
    #[error("Compilation failed")]
    CompilationFailed,
    #[error("Pop from empty stack")]
    PopFromEmptyStack,
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

    fn push(&mut self, v: Value) {
        self.stack.push_back(v);
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack.pop_back().ok_or(Error::PopFromEmptyStack)
    }

    pub fn run(&mut self) -> Result<Value, Error> {
        loop {
            #[cfg(debug_assertions)]
            self.trace();

            match self.read_opcode()? {
                OpCode::Return => {
                    let value = self.pop()?;
                    println!("{:?}", value);
                    return Ok(value);
                }
                OpCode::Greater => binary_op!(self, >),
                OpCode::Less => binary_op!(self, <),
                OpCode::Add => binary_op!(self, +),
                OpCode::Subtract => binary_op!(self, -),
                OpCode::Multiply => binary_op!(self, *),
                OpCode::Divide => binary_op!(self, /),
                OpCode::Not => {
                    let val = self.pop()?;
                    self.push(val.is_falsey().into());
                }
                OpCode::Negate => {
                    let constant = self.pop()?;
                    match constant {
                        Value::Number(n) => self.push((-n).into()),
                        other => return Err(Error::InvalidOperand("Number".to_owned(), other)),
                    }
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(true.into()),
                OpCode::False => self.push(false.into()),
                OpCode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push((a == b).into());
                }
            }
        }
    }

    pub fn repl(&mut self) -> Result<(), Error> {
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            let mut line = String::new();
            std::io::stdin().read_line(&mut line)?;

            self.interpret(&line)?;
        }
    }

    pub fn run_file(&mut self, path: &PathBuf) -> Result<(), Error> {
        let source = std::fs::read_to_string(path)?;
        self.interpret(&source)?;
        Ok(())
    }

    fn interpret(&mut self, source: &str) -> Result<Value, Error> {
        let parser = Parser::new(source);
        let chunk = if let Some(chunk) = parser.compile() {
            chunk
        } else {
            return Err(Error::CompilationFailed);
        };

        self.set_chunk(chunk);

        self.run()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    #[test]
    fn literals() {
        let mut vm = VM::default();
        assert_matches!(vm.interpret("1"), Ok(Value::Number(1f64)));
        assert_matches!(vm.interpret("1.1"), Ok(Value::Number(1.1f64)));
        assert_matches!(vm.interpret("true"), Ok(Value::Boolean(true)));
        assert_matches!(vm.interpret("false"), Ok(Value::Boolean(false)));
        assert_matches!(vm.interpret("nil"), Ok(Value::Nil));
    }

    #[test]
    fn unary() {
        let mut vm = VM::default();
        assert_matches!(vm.interpret("-13.109"), Ok(Value::Number(-13.109f64)));
        assert_matches!(vm.interpret("-0"), Ok(Value::Number(-0f64)));

        assert_matches!(vm.interpret("!true"), Ok(Value::Boolean(false)));
        assert_matches!(vm.interpret("!!true"), Ok(Value::Boolean(true)));
        assert_matches!(vm.interpret("!false"), Ok(Value::Boolean(true)));
        assert_matches!(vm.interpret("!!false"), Ok(Value::Boolean(false)));

        assert_matches!(vm.interpret("!nil"), Ok(Value::Boolean(true)));
        assert_matches!(vm.interpret("!!nil"), Ok(Value::Boolean(false)));
    }

    #[test]
    fn arithmetic() {
        let mut vm = VM::default();
        assert_matches!(vm.interpret("1+2"), Ok(Value::Number(3.0)));
        assert_matches!(vm.interpret("1-2"), Ok(Value::Number(-1.0)));
        assert_matches!(vm.interpret("3*2"), Ok(Value::Number(6.0)));
        assert_matches!(vm.interpret("9.3/3"), Ok(Value::Number(3.1)));
        assert_matches!(vm.interpret("2*3+5"), Ok(Value::Number(11.0)));
        assert_matches!(vm.interpret("-2*(3+5)"), Ok(Value::Number(-16.0)));
    }

    #[test]
    fn binary() {
        let mut vm = VM::default();
        assert_matches!(vm.interpret("1+2==3"), Ok(Value::Boolean(true)));
        assert_matches!(vm.interpret("true!=false"), Ok(Value::Boolean(true)));
        assert_matches!(vm.interpret("true==false"), Ok(Value::Boolean(false)));
        assert_matches!(vm.interpret("2<3"), Ok(Value::Boolean(true)));
        assert_matches!(vm.interpret("2<=3"), Ok(Value::Boolean(true)));
        assert_matches!(vm.interpret("2>3"), Ok(Value::Boolean(false)));
        assert_matches!(vm.interpret("2>=3"), Ok(Value::Boolean(false)));


    }
}
