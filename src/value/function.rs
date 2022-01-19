use crate::Chunk;

#[derive(Clone, Default, PartialEq, PartialOrd, Hash)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
    pub upvalue_count: usize,
}

impl Function {
    pub fn line(&self, pc: usize) -> Option<usize> {
        self.chunk.lines.get(pc).cloned()
    }

    pub fn mark(&mut self) {
        self.chunk.mark();
    }
}
