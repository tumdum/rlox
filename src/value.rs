use crate::chunk::Chunk;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    // todo in future
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd, Hash)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub enum Obj {
    String(String),
    Function(Function),
}

impl Display for Obj {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Function(Function { name, .. }) => write!(f, "<fn {}>", name),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    Obj(*mut Obj),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, h: &mut H) {
        use Value::*;
        match self {
            Nil => {}
            Number(f) => {
                let i: u64 = unsafe { std::mem::transmute(f) };
                h.write_u64(i);
            }
            Boolean(b) => h.write_u8(*b as u8),
            Obj(ptr) => {
                let obj: &self::Obj = unsafe { &**ptr };
                obj.hash(h);
            }
        }
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil) || matches!(self, Value::Boolean(false))
    }

    pub fn string(&self) -> Option<&str> {
        if let Self::Obj(ptr) = self {
            if let Obj::String(s) = unsafe { &**ptr } {
                return Some(s);
            }
        }
        None
    }

    pub fn function_mut(&mut self) -> Option<&mut Function> {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let Obj::Function(ref mut f) = obj {
                return Some(f);
            }
        }
        None
    }

    pub fn function(&self) -> Option<&Function> {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let Obj::Function(f) = obj {
                return Some(f);
            }
        }
        None
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let Obj::Function(f) = obj {
                return &mut f.chunk;
            }
        }
        todo!()
    }

    pub fn chunk(&self) -> &Chunk {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let Obj::Function(f) = obj {
                return &f.chunk;
            }
        }
        todo!()
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Obj(ptr) => {
                write!(f, "{}", unsafe { &**ptr })
            }
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Number(n) => write!(f, "Number({})", n),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Obj(ptr) => {
                write!(f, "Obj({:p} => {:?})", *ptr, unsafe { &**ptr })
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
            (Obj(l), Obj(r)) if l == r => true,
            (Obj(l), Obj(r)) => unsafe {
                let l = &**l;
                let r = &**r;
                l == r
            },
            _ => false,
        }
    }
}

impl Eq for Value {}

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
