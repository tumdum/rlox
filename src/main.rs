mod chunk;
mod value;
mod debug;
mod vm;

use chunk::{OpCode, Chunk};
use crate::vm::VM;

fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2.into());
    chunk.write(OpCode::Constant as u8, 123);
    chunk.write(constant, 123);

    let constant = chunk.add_constant(3.4.into());
    chunk.write(OpCode::Constant as u8, 123);
    chunk.write(constant, 123);

    chunk.write(OpCode::Add as u8, 123);

    let constant = chunk.add_constant(5.6.into());
    chunk.write(OpCode::Constant as u8, 123);
    chunk.write(constant, 123);

    chunk.write(OpCode::Divide as u8, 123);

    chunk.write(OpCode::Negate as u8, 123);
    chunk.write(OpCode::Return as u8, 123);
    chunk.disassemble("test chunk");
    let mut vm = VM::new(chunk);
    vm.run().unwrap();
}
