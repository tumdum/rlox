use crate::chunk::Chunk;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    // todo in future
}

#[derive(Clone, PartialEq, PartialOrd, Hash)]
pub struct Closure {
    pub function: Value,
}

impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let fun = self.function.function().unwrap();
        write!(f, "<fn {}@{}>", fun.name, fun.arity)
    }
}

/*
impl Deref for Closure {
    type Target = Function;
    fn deref(&self) -> &Function {
        self.function.function().unwrap()
    }
}

impl DerefMut for Closure {
    fn deref_mut(&mut self) -> &mut Function {
        self.function.function_mut().unwrap()
    }
}
*/

#[derive(Clone, Default, PartialEq, PartialOrd, Hash)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}

impl Function {
    pub fn line(&self, pc: usize) -> Option<usize> {
        self.chunk.lines.get(pc).cloned()
    }
}

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

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub enum Obj {
    String(String),
    Function(Function),
    Closure(Closure),
}

impl Display for Obj {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Function(function) => function.fmt(f),
            Self::Closure(closure) => closure.function.function().unwrap().fmt(f),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    NativeFunction(NativeFunction),
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
            NativeFunction(nf) => {
                nf.hash(h);
            }
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
                return Some(&mut *f);
            }
        }
        None
    }

    pub fn function(&self) -> Option<&Function> {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let Obj::Function(f) = obj {
                return Some(&*f);
            }
        }
        None
    }

    pub fn closure_mut(&mut self) -> Option<&mut Closure> {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let Obj::Closure(ref mut f) = obj {
                return Some(&mut *f);
            }
        }
        None
    }
    pub fn closure(&self) -> Option<&Closure> {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let Obj::Closure(f) = obj {
                return Some(&*f);
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

    pub fn callable(&self) -> Option<Callable> {
        match self {
            Self::NativeFunction(nf) => Some(Callable::Native(nf)),
            Self::Obj(ptr) => match unsafe { &**ptr } {
                Obj::Function(f) => Some(Callable::Function(f)),
                Obj::Closure(c) => Some(Callable::Closure(c)),
                _ => None,
            },
            _ => None,
        }
    }
}

pub enum Callable<'a> {
    Function(&'a Function),
    Closure(&'a Closure),
    Native(&'a NativeFunction),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::NativeFunction(nf) => write!(f, "{}", nf),
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
            Value::NativeFunction(nf) => write!(f, "NativeFunction({})", nf),
            Value::Obj(ptr) => {
                write!(f, "Obj({:?})", unsafe { &**ptr })
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
