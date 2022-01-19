use crate::value::{NativeMethod, Value};
use crate::allocator::Allocator;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct Vector(pub Vec<Value>);

impl Vector {
    pub fn get_native_method(&self, name: &str) -> Option<NativeMethod> {
        match name {
            "len" => Some(&len),
            "get" => Some(&get),
            "set" => Some(&set),
            "push" => Some(&push),
            "rotateLeft" => Some(&rotate_left),
            _ => None,
        }
    }
}

fn len(_allocator: &mut Allocator, this: &mut Value, args: &[Value]) -> Value {
    assert!(args.is_empty());
    (this.vector().unwrap().len() as f64).into()
}

fn get(_allocator: &mut Allocator, this: &mut Value, args: &[Value]) -> Value {
    let idx = *args[0].number().unwrap() as usize;
    this.vector().unwrap()[idx].clone()
}

fn set(_allocator: &mut Allocator, this: &mut Value, args: &[Value]) -> Value {
    let idx = *args[0].number().unwrap() as usize;
    this.vector_mut().unwrap()[idx] = args[1].clone();
    Value::Nil
}

fn rotate_left(_allocator: &mut Allocator, this: &mut Value, args: &[Value]) -> Value {
    assert!(args.len() == 1);
    let n = *args[0].number().unwrap() as usize;
    this.vector_mut().unwrap().rotate_left(n);
    Value::Nil
}

fn push(_allocator: &mut Allocator, this: &mut Value, args: &[Value]) -> Value {
    assert!(args.len() > 0);
    let v = &mut this.vector_mut().unwrap();
    v.0.extend_from_slice(args);
    Value::Nil
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