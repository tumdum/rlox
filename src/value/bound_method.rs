use crate::value::Value;

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: Value,
}

impl BoundMethod {
    pub fn mark(&mut self) {
        self.receiver.mark();
        self.method.mark();
    }
}
