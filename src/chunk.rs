use crate::value::Value;
use std::num::TryFromIntError;
use thiserror::Error;

#[derive(Default, Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> Result<u8, TryFromIntError> {
        self.constants.push(value);
        (self.constants.len() - 1).try_into()
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum OpCode {
    Constant = 0,
    Nil = 1,
    True = 2,
    False = 3,
    Equal = 4,
    Greater = 5,
    Less = 6,
    Add = 7,
    Subtract = 8,
    Multiply = 9,
    Divide = 10,
    Not = 11,
    Negate = 12,
    Return = 13,
}

#[derive(Debug, Error)]
#[error("Invalid op code {0}")]
pub struct InvalidOpCode(u8);

impl TryFrom<u8> for OpCode {
    type Error = InvalidOpCode;

    fn try_from(b: u8) -> Result<Self, InvalidOpCode> {
        match b {
            0 => Ok(OpCode::Constant),
            1 => Ok(OpCode::Nil),
            2 => Ok(OpCode::True),
            3 => Ok(OpCode::False),
            4 => Ok(OpCode::Equal),
            5 => Ok(OpCode::Greater),
            6 => Ok(OpCode::Less),
            7 => Ok(OpCode::Add),
            8 => Ok(OpCode::Subtract),
            9 => Ok(OpCode::Multiply),
            10 => Ok(OpCode::Divide),
            11 => Ok(OpCode::Not),
            12 => Ok(OpCode::Negate),
            13 => Ok(OpCode::Return),
            other => Err(InvalidOpCode(other)),
        }
    }
}
