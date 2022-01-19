use crate::value::{NativeMethod,Value};
use std::ops::{DerefMut, Deref};

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct Vector(pub Vec<Value>);

impl Vector {
    pub fn get_native_method(&self, name: &str) -> Option<NativeMethod> {
        match name {
            "len" => Some(&len),
            "get" => Some(&get),
            _ => None,
        }
    }
}

fn len(this: &Value, args: &[Value]) -> Value {
    assert!(args.is_empty());
    (this.vector().unwrap().len() as f64).into()
}

fn get(this: &Value, args: &[Value]) -> Value {
    let idx = *args[0].number().unwrap() as usize;
    this.vector().unwrap()[idx].clone()
}

impl Deref for Vector {
    type Target = [Value];

    fn deref(&self) -> &[Value] {
        &self.0
    }
}

impl DerefMut for Vector {

    fn deref_mut(&mut self) -> &mut [Value] {
        &mut self.0
    }
}
