mod chunk;
mod value;
mod debug;

use chunk::{OpCode, Chunk};

fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2.into());
    chunk.write(OpCode::Constant as u8, 123);
    chunk.write(constant, 123);
    chunk.write(OpCode::Return as u8, 123);
    chunk.disassemble("test chunk");
}
