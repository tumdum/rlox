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
    GetProperty = 12,
    SetProperty = 13,
    Equal = 14,
    Greater = 15,
    Less = 16,
    Add = 17,
    Subtract = 18,
    Multiply = 19,
    Divide = 20,
    Not = 21,
    Negate = 22,
    Print = 23,
    Jump = 24,
    JumpIfFalse = 25,
    Loop = 26,
    Call = 27,
    Closure = 28,
    CloseUpValue = 29,
    Return = 30,
    Class = 31,
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
            12 => Ok(OpCode::GetProperty),
            13 => Ok(OpCode::SetProperty),
            14 => Ok(OpCode::Equal),
            15 => Ok(OpCode::Greater),
            16 => Ok(OpCode::Less),
            17 => Ok(OpCode::Add),
            18 => Ok(OpCode::Subtract),
            19 => Ok(OpCode::Multiply),
            20 => Ok(OpCode::Divide),
            21 => Ok(OpCode::Not),
            22 => Ok(OpCode::Negate),
            23 => Ok(OpCode::Print),
            24 => Ok(OpCode::Jump),
            25 => Ok(OpCode::JumpIfFalse),
            26 => Ok(OpCode::Loop),
            27 => Ok(OpCode::Call),
            28 => Ok(OpCode::Closure),
            29 => Ok(OpCode::CloseUpValue),
            30 => Ok(OpCode::Return),
            31 => Ok(OpCode::Class),
            other => Err(InvalidOpCode(other)),
        }
    }
}
