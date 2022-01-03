use crate::value::Value;
use std::num::TryFromIntError;
use thiserror::Error;

#[derive(Clone, Default, Debug, PartialEq, PartialOrd, Hash)]
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
    GetLocal = 5,
    SetLocal = 6,
    GetGlobal = 7,
    DefineGlobal = 8,
    SetGlobal = 9,
    Equal = 10,
    Greater = 11,
    Less = 12,
    Add = 13,
    Subtract = 14,
    Multiply = 15,
    Divide = 16,
    Not = 17,
    Negate = 18,
    Print = 19,
    Jump = 20,
    JumpIfFalse = 21,
    Loop = 22,
    Call = 23,
    Return = 24,
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
            5 => Ok(OpCode::GetLocal),
            6 => Ok(OpCode::SetLocal),
            7 => Ok(OpCode::GetGlobal),
            8 => Ok(OpCode::DefineGlobal),
            9 => Ok(OpCode::SetGlobal),
            10 => Ok(OpCode::Equal),
            11 => Ok(OpCode::Greater),
            12 => Ok(OpCode::Less),
            13 => Ok(OpCode::Add),
            14 => Ok(OpCode::Subtract),
            15 => Ok(OpCode::Multiply),
            16 => Ok(OpCode::Divide),
            17 => Ok(OpCode::Not),
            18 => Ok(OpCode::Negate),
            19 => Ok(OpCode::Print),
            20 => Ok(OpCode::Jump),
            21 => Ok(OpCode::JumpIfFalse),
            22 => Ok(OpCode::Loop),
            23 => Ok(OpCode::Call),
            24 => Ok(OpCode::Return),
            other => Err(InvalidOpCode(other)),
        }
    }
}
