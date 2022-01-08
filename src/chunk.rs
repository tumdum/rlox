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

    pub fn mark(&mut self) {
        self.constants.iter_mut().for_each(|c| c.mark());
    }

    pub fn size(&self) -> usize {
        self.code.len() + self.lines.len() * std::mem::size_of::<usize>()
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
    GetUpValue = 10,
    SetUpValue = 11,
    Equal = 12,
    Greater = 13,
    Less = 14,
    Add = 15,
    Subtract = 16,
    Multiply = 17,
    Divide = 18,
    Not = 19,
    Negate = 20,
    Print = 21,
    Jump = 22,
    JumpIfFalse = 23,
    Loop = 24,
    Call = 25,
    Closure = 26,
    CloseUpValue = 27,
    Return = 28,
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
            10 => Ok(OpCode::GetUpValue),
            11 => Ok(OpCode::SetUpValue),
            12 => Ok(OpCode::Equal),
            13 => Ok(OpCode::Greater),
            14 => Ok(OpCode::Less),
            15 => Ok(OpCode::Add),
            16 => Ok(OpCode::Subtract),
            17 => Ok(OpCode::Multiply),
            18 => Ok(OpCode::Divide),
            19 => Ok(OpCode::Not),
            20 => Ok(OpCode::Negate),
            21 => Ok(OpCode::Print),
            22 => Ok(OpCode::Jump),
            23 => Ok(OpCode::JumpIfFalse),
            24 => Ok(OpCode::Loop),
            25 => Ok(OpCode::Call),
            26 => Ok(OpCode::Closure),
            27 => Ok(OpCode::CloseUpValue),
            28 => Ok(OpCode::Return),
            other => Err(InvalidOpCode(other)),
        }
    }
}
