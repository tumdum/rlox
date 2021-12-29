use crate::value::Value;

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { code: vec![], constants: vec![], lines: vec![]}
    }

    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len()-1).try_into().unwrap()
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum OpCode {
    Constant = 0,
    Return = 1,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(b: u8) -> Result<Self, ()> {
        match b {
            0 => Ok(OpCode::Constant),
            1 => Ok(OpCode::Return),
            _other => todo!(),
        }
    }
}

