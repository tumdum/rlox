use crate::chunk::{Chunk, OpCode};

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let instruction = self.code[offset];
        match instruction.try_into() {
            Ok(OpCode::Constant) => self.constant_instruction("OP_CONSTANT", offset),
            Ok(OpCode::Nil) => simple_instruction("OP_NIL", offset),
            Ok(OpCode::True) => simple_instruction("OP_TRUE", offset),
            Ok(OpCode::False) => simple_instruction("OP_FALSE", offset),

            Ok(OpCode::Equal) => simple_instruction("OP_EQUAL", offset),
            Ok(OpCode::Greater) => simple_instruction("OP_GREATER", offset),
            Ok(OpCode::Less) => simple_instruction("OP_LESS", offset),

            Ok(OpCode::Add) => simple_instruction("OP_ADD", offset),
            Ok(OpCode::Subtract) => simple_instruction("OP_SUBTRACT", offset),
            Ok(OpCode::Multiply) => simple_instruction("OP_MULTIPLY", offset),
            Ok(OpCode::Divide) => simple_instruction("OP_DIVIDE", offset),
            Ok(OpCode::Not) => simple_instruction("OP_NOT", offset),
            Ok(OpCode::Negate) => simple_instruction("OP_NEGATE", offset),
            Ok(OpCode::Return) => simple_instruction("OP_RETURN", offset),
            _other => {
                println!("Unknown opcode {}", instruction);
                offset + 1
            }
        }
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1] as usize;
        println!(
            "{:>16} {:4} '{:?}'",
            name, constant, self.constants[constant]
        );
        offset + 2
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{:>16}", name);
    offset + 1
}
