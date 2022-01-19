use crate::chunk::Chunk;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use thiserror::Error;

pub type NativeMethod = &'static dyn Fn(&Value, &[Value]) -> Value;

mod closure;
pub use closure::Closure;

mod function;
pub use function::Function;

mod native_function;
pub use native_function::NativeFunction;

mod up_value;
pub use up_value::UpValue;

mod obj;
pub use obj::Obj;

mod class;
pub use class::Class;

mod obj_instance;
pub use obj_instance::ObjInstance;

mod obj_inner;
pub use obj_inner::ObjInner;

mod bound_method;
pub use bound_method::BoundMethod;

mod vector;
pub use vector::Vector;

mod string;
pub use string::ObjString;

#[derive(Debug, Error)]
pub enum Error {
    // TODO in future :)
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

    fn inner(&self) -> Option<&ObjInner> {
        if let Self::Obj(ptr) = self {
            Some(&**unsafe { &**ptr })
        } else {
            None
        }
    }

    fn inner_mut(&mut self) -> Option<&mut ObjInner> {
        if let Value::Obj(ptr) = self {
            Some(unsafe { &mut **ptr })
        } else {
            None
        }
    }

    pub fn string(&self) -> Option<&ObjString> {
        if let Some(ObjInner::String(s)) = self.inner() {
            return Some(s);
        }
        None
    }

    pub fn function_mut(&mut self) -> Option<&mut Function> {
        if let Some(ObjInner::Function(ref mut f)) = self.inner_mut() {
            return Some(f);
        }
        None
    }

    pub fn function(&self) -> Option<&Function> {
        if let Some(ObjInner::Function(f)) = self.inner() {
            return Some(f);
        }
        None
    }

    pub fn vector(&self) -> Option<&Vector> {
        if let Some(ObjInner::Vector(v)) = self.inner() {
            return Some(v);
        }
        None
    }

    pub fn number(&self) -> Option<&f64> {
        if let Self::Number(f) = self {
            return Some(f);
        }
        None
    }

    pub fn closure_mut(&mut self) -> Option<&mut Closure> {
        if let Some(ObjInner::Closure(ref mut c)) = self.inner_mut() {
            return Some(c);
        }
        None
    }

    pub fn closure(&self) -> Option<&Closure> {
        if let Some(ObjInner::Closure(c)) = self.inner() {
            return Some(c);
        }
        None
    }

    pub fn upvalue_mut(&mut self) -> Option<&mut UpValue> {
        if let Some(ObjInner::UpValue(ref mut v)) = self.inner_mut() {
            return Some(v);
        }
        None
    }

    pub fn upvalue(&self) -> Option<&UpValue> {
        if let Some(ObjInner::UpValue(v)) = self.inner() {
            return Some(v);
        }
        None
    }

    pub fn class_mut(&mut self) -> Option<&mut Class> {
        if let Some(ObjInner::Class(ref mut c)) = self.inner_mut() {
            return Some(c);
        }
        None
    }

    pub fn class(&self) -> Option<&Class> {
        if let Some(ObjInner::Class(c)) = self.inner() {
            return Some(c);
        }
        None
    }

    pub fn instance_mut(&mut self) -> Option<&mut ObjInstance> {
        if let Some(ObjInner::ObjInstance(ref mut v)) = self.inner_mut() {
            return Some(v);
        }
        None
    }

    pub fn instance(&self) -> Option<&ObjInstance> {
        if let Some(ObjInner::ObjInstance(v)) = self.inner() {
            return Some(v);
        }
        None
    }

    pub fn chunk_mut(&mut self) -> Option<&mut Chunk> {
        self.function_mut().map(|f| &mut f.chunk)
    }

    pub fn get_native_method(&self, name: &str) -> Option<NativeMethod> {
        match self {
            Self::Obj(ptr) => unsafe { &**ptr }.get_native_method(name),
            _ => None,
        }
    }

    pub fn callable(&self) -> Option<Callable> {
        match self {
            Self::NativeFunction(nf) => Some(Callable::Native(nf)),
            Self::Obj(ptr) => match &unsafe { &**ptr }.inner {
                ObjInner::Function(f) => Some(Callable::Function(f)),
                ObjInner::Closure(c) => Some(Callable::Closure(c)),
                ObjInner::Class(c) => Some(Callable::Class(c)),
                ObjInner::BoundMethod(bm) => Some(Callable::BoundMethod(bm)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn mark(&mut self) {
        use Value::*;
        match self {
            Nil => {}
            Number(_f) => {}
            Boolean(_b) => {}
            NativeFunction(_nf) => {}
            Obj(ptr) => {
                if !ptr.is_null() {
                    let obj: &mut self::Obj = unsafe { &mut **ptr };
                    if !obj.is_marked {
                        obj.mark();
                    }
                }
            }
        }
    }
    pub fn type_name(&self) -> &'static str {
        use Value::*;
        match self {
            Nil => "nil",
            Number(_f) => "number",
            Boolean(_b) => "boolean",
            NativeFunction(_nf) => "native_function",
            Obj(ptr) => {
                let obj: &self::Obj = unsafe { &**ptr };
                obj.type_name()
            }
        }
    }

    pub fn is_reachable(&self) -> bool {
        use Value::*;
        match self {
            Nil => true,
            Number(_f) => true,
            Boolean(_b) => true,
            NativeFunction(_nf) => true,
            Obj(ptr) => {
                let obj: &mut self::Obj = unsafe { &mut **ptr };
                obj.is_marked
            }
        }
    }
}

pub enum Callable<'a> {
    Function(&'a Function),
    Closure(&'a Closure),
    Native(&'a NativeFunction),
    Class(&'a Class),
    BoundMethod(&'a BoundMethod),
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
