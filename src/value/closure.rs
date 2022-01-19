use crate::value::Value;
use std::fmt::{Debug, Formatter};

#[derive(Clone, PartialEq, PartialOrd, Hash)]
pub struct Closure {
    pub function: Value,
    pub upvalues: Vec<Value>,
}

impl Closure {
    pub fn mark(&mut self) {
        self.function.mark();
        self.upvalues.iter_mut().for_each(|v| v.mark());
    }

    pub fn size(&self) -> usize {
        0
    }
}

impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let fun = self.function.function().unwrap();
        write!(f, "<fn {}@{}>", fun.name, fun.arity)
    }
}
