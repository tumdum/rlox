use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    // todo in future
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Obj {
    String(String),
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    Obj(*const Obj),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil) || matches!(self, Value::Boolean(false))
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Number(n) => write!(f, "Number({})", n),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Obj(ptr) => {
                write!(f, "Obj({:p} => {:?})", ptr, unsafe { &**ptr })
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Number(l), Number(r)) => l == r,
            (Boolean(l), Boolean(r)) => l == r,
            (Obj(l), Obj(r)) => unsafe {
                let l = &**l;
                let r = &**r;
                l == r
            },
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Value::*;
        match (self, other) {
            (Nil, Nil) => Some(Ordering::Equal),
            (Number(l), Number(r)) => l.partial_cmp(r),
            (Boolean(l), Boolean(r)) => l.partial_cmp(r),
            (Obj(l), Obj(r)) => unsafe {
                let l = &**l;
                let r = &**r;
                l.partial_cmp(r)
            },
            _ => todo!(),
        }
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Number(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}
