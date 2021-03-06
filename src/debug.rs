use crate::chunk::{Chunk, OpCode};

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        println!("{} constants:", self.constants.len());
        for (i, v) in self.constants.iter().enumerate() {
            println!("{:3}: {:?}", i, v);
        }
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
            Ok(OpCode::Pop) => simple_instruction("OP_POP", offset),
            Ok(OpCode::GetLocal) => self.byte_instruction("OP_GET_LOCAL", offset),
            Ok(OpCode::SetLocal) => self.byte_instruction("OP_SET_LOCAL", offset),
            Ok(OpCode::GetGlobal) => self.constant_instruction("OP_GET_GLOBAL", offset),
            Ok(OpCode::DefineGlobal) => self.constant_instruction("OP_DEFINE_GLOBAL", offset),
            Ok(OpCode::SetGlobal) => self.constant_instruction("OP_SET_GLOBAL", offset),
            Ok(OpCode::GetUpValue) => self.byte_instruction("OP_GET_UPVALUE", offset),
            Ok(OpCode::SetUpValue) => self.byte_instruction("OP_SET_UPVALUE", offset),
            Ok(OpCode::GetProperty) => self.constant_instruction("OP_GET_PROPERTY", offset),
            Ok(OpCode::SetProperty) => self.constant_instruction("OP_SET_PROPERTY", offset),
            Ok(OpCode::GetSuper) => self.constant_instruction("OP_GET_SUPER", offset),
            Ok(OpCode::Equal) => simple_instruction("OP_EQUAL", offset),
            Ok(OpCode::Greater) => simple_instruction("OP_GREATER", offset),
            Ok(OpCode::Less) => simple_instruction("OP_LESS", offset),
            Ok(OpCode::Add) => simple_instruction("OP_ADD", offset),
            Ok(OpCode::Subtract) => simple_instruction("OP_SUBTRACT", offset),
            Ok(OpCode::Multiply) => simple_instruction("OP_MULTIPLY", offset),
            Ok(OpCode::Divide) => simple_instruction("OP_DIVIDE", offset),
            Ok(OpCode::Not) => simple_instruction("OP_NOT", offset),
            Ok(OpCode::Negate) => simple_instruction("OP_NEGATE", offset),
            Ok(OpCode::Print) => simple_instruction("OP_PRINT", offset),
            Ok(OpCode::Jump) => self.jump_instruction("OP_JUMP", 1, offset),
            Ok(OpCode::JumpIfFalse) => self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset),
            Ok(OpCode::Loop) => self.jump_instruction("OP_JUMP", -1, offset),
            Ok(OpCode::Call) => self.byte_instruction("OP_CALL", offset),
            Ok(OpCode::Invoke) => self.invoke_instruction("OP_INVOKE", offset),
            Ok(OpCode::SuperInvoke) => self.invoke_instruction("OP_SUPER_INVOKE", offset),
            Ok(OpCode::Closure) => {
                let mut offset = offset + 2;
                let constant = self.code[offset - 1] as usize;
                let fun = &self.constants[constant].function().unwrap();
                println!("{:>16} {:4} '{:?}'", "OP_CLOSURE", constant, fun);
                for _ in 0..fun.upvalue_count {
                    let is_local = self.code[offset] == 1;
                    let index = self.code[offset + 1];
                    offset += 2;
                    println!(
                        "{:04}    |                     {} {}",
                        offset - 2,
                        if is_local { "local" } else { "upvalue" },
                        index
                    );
                }
                offset
            }
            Ok(OpCode::CloseUpValue) => simple_instruction("OP_CLOSE_UPVALUE", offset),
            Ok(OpCode::Return) => simple_instruction("OP_RETURN", offset),
            Ok(OpCode::Class) => self.constant_instruction("OP_CLASS", offset),
            Ok(OpCode::Inherit) => simple_instruction("OP_INHERIT", offset),
            Ok(OpCode::Method) => self.constant_instruction("OP_METHOD", offset),
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

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{:>16} {:4}", name, slot);
        offset + 2
    }

    fn jump_instruction(&self, name: &str, sign: isize, offset: usize) -> usize {
        let jump = ((self.code[offset + 1] as u16) << 8 | self.code[offset + 2] as u16) as isize;
        let off = offset as isize;
        println!("{:>16} {:4} -> {}", name, offset, off + 3 + sign * jump);
        offset + 3
    }

    fn invoke_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1] as usize;
        let arg_count = self.code[offset + 2];
        println!(
            "{:>16} ({} args) {:4} '{:?}'",
            name, arg_count, constant, self.constants[constant]
        );
        offset + 3
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{:>16}", name);
    offset + 1
}
