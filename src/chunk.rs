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
        match self.constants.iter().position(|v| v == &value) {
            Some(idx) => Ok(idx.try_into()?),
            None => {
                self.constants.push(value);
                (self.constants.len() - 1).try_into()
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum OpCode {
    Constant = 0,
    Nil = 1,
    True = 2,
    False = 3,
    Pop = 4,
    GetGlobal = 5,
    DefineGlobal = 6,
    SetGlobal = 7,
    Equal = 8,
    Greater = 9,
    Less = 10,
    Add = 11,
    Subtract = 12,
    Multiply = 13,
    Divide = 14,
    Not = 15,
    Negate = 16,
    Print = 17,
    Return = 18,
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
            4 => Ok(OpCode::Pop),
            5 => Ok(OpCode::GetGlobal),
            6 => Ok(OpCode::DefineGlobal),
            7 => Ok(OpCode::SetGlobal),
            8 => Ok(OpCode::Equal),
            9 => Ok(OpCode::Greater),
            10 => Ok(OpCode::Less),
            11 => Ok(OpCode::Add),
            12 => Ok(OpCode::Subtract),
            13 => Ok(OpCode::Multiply),
            14 => Ok(OpCode::Divide),
            15 => Ok(OpCode::Not),
            16 => Ok(OpCode::Negate),
            17 => Ok(OpCode::Print),
            18 => Ok(OpCode::Return),
            other => Err(InvalidOpCode(other)),
        }
    }
}
