use crate::chunk::{Chunk,OpCode};

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset-1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let instruction = self.code[offset];
        match instruction.try_into() {
            Ok(OpCode::Return) => {
                simple_instruction("OP_RETURN", offset)
            },
            Ok(OpCode::Constant) => {
                self.constant_instruction("OP_CONSTANT", offset)
            },
            _other => {
                println!("Unknown opcode {}", instruction);
                offset + 1
            }
        }
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset+1] as usize;
        println!("{:>16} {:4} '{:?}'", name, constant,
            self.constants[constant]);
        offset+2
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{:>16}", name);
    offset + 1
}
