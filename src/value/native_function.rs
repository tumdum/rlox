use crate::value::{Function, Value};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub function: Rc<dyn Fn(&[Value]) -> Value>, // TODO: this should return a Result
}

impl Hash for NativeFunction {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.name.hash(h);
        Rc::as_ptr(&self.function).hash(h);
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "<native fn {}>", self.name)
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "<fn {}@{}>", self.name, self.arity)
    }
}
