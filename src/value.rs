use crate::chunk::Chunk;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    // TODO in future :)
}

#[derive(Clone, PartialEq, PartialOrd, Hash)]
pub struct Closure {
    pub function: Value,
    pub upvalues: Vec<Value>,
}

impl Closure {
    fn mark(&mut self) {
        self.function.mark();
        self.upvalues.iter_mut().for_each(|v| v.mark());
    }

    fn size(&self) -> usize {
        0
    }
}

impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let fun = self.function.function().unwrap();
        write!(f, "<fn {}@{}>", fun.name, fun.arity)
    }
}

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

    fn mark(&mut self) {
        self.chunk.mark();
    }

    fn size(&self) -> usize {
        self.name.as_bytes().len() + self.chunk.size()
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
pub struct UpValue {
    // TODO: this could be an enum.
    pub location: *mut Value, // This could be an index into stack
    pub closed: Option<Value>,
}

impl UpValue {
    fn mark(&mut self) {
        self.closed.as_mut().iter_mut().for_each(|c| c.mark());
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct Obj {
    pub inner: ObjInner,
    pub is_marked: bool,
}

impl Obj {
    fn mark(&mut self) {
        #[cfg(debug_assertions)]
        println!("marking {:p} => {:?}", self, self);
        self.is_marked = true;
        self.inner.mark();
    }
}

impl Deref for Obj {
    type Target = ObjInner;
    fn deref(&self) -> &ObjInner {
        &self.inner
    }
}

impl DerefMut for Obj {
    fn deref_mut(&mut self) -> &mut ObjInner {
        &mut self.inner
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct Class {
    pub name: String,
    methods: std::collections::BTreeMap<String, Value>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: Default::default(),
        }
    }

    pub fn get_method(&self, name: &str) -> Option<&Value> {
        self.methods.get(name)
    }

    pub fn add_method(&mut self, name: &str, closure: Value) {
        self.methods.insert(name.to_owned(), closure);
    }

    fn mark(&mut self) {
        self.methods.values_mut().for_each(|m| m.mark());
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct ObjInstance {
    pub class: Value,
    // fields: FxHashMap<String, Value>, TODO: make it a hash map/faster/hashable
    fields: std::collections::BTreeMap<String, Value>,
}

impl ObjInstance {
    pub fn new(class: Value) -> ObjInstance {
        Self {
            class,
            fields: Default::default(),
        }
    }
    fn mark(&mut self) {
        self.class.mark();
        self.fields.values_mut().for_each(|v| v.mark());
    }

    pub fn get_field(&self, name: &str) -> Option<&Value> {
        self.fields.get(name)
    }

    pub fn set_field(&mut self, name: String, value: Value) {
        self.fields.insert(name, value);
    }

    fn size(&self) -> usize {
        self.fields.keys().map(|k| k.as_bytes().len()).sum()
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: Value,
}

impl BoundMethod {
    fn mark(&mut self) {
        self.receiver.mark();
        self.method.mark();
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub enum ObjInner {
    String(String),
    Function(Function),
    Closure(Closure),
    UpValue(UpValue),
    Class(Class),
    ObjInstance(ObjInstance),
    BoundMethod(BoundMethod),
}

impl ObjInner {
    #[allow(dead_code)]
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::String(_) => "string",
            Self::Function(_) => "function",
            Self::Closure(_) => "closure",
            Self::UpValue(_) => "upvalue",
            Self::Class(_) => "class",
            Self::ObjInstance(_) => "instance",
            Self::BoundMethod(_) => "bound_method",
        }
    }

    fn mark(&mut self) {
        match self {
            Self::String(_s) => {}
            Self::Function(function) => function.mark(),
            Self::Closure(closure) => closure.mark(),
            Self::UpValue(upvalue) => upvalue.mark(),
            Self::Class(class) => class.mark(),
            Self::ObjInstance(instance) => instance.mark(),
            Self::BoundMethod(bound_method) => bound_method.mark(),
        }
    }

    pub fn size(&self) -> usize {
        std::mem::size_of::<Self>()
            + match self {
                Self::String(s) => s.as_bytes().len(),
                Self::Function(f) => f.size(),
                Self::Closure(c) => c.size(),
                Self::UpValue(_) => 0,
                Self::Class(_) => 0, // TODO
                Self::ObjInstance(instance) => instance.size(),
                Self::BoundMethod(_) => 0,
            }
    }
}

impl Drop for Obj {
    fn drop(&mut self) {
        #[cfg(feature = "alloc_logs")]
        println!("deallocated {:p}: {}", self, self.type_name());
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &**self {
            ObjInner::String(s) => write!(f, "{}", s),
            ObjInner::Function(function) => function.fmt(f),
            ObjInner::Closure(closure) => closure.function.function().unwrap().fmt(f),
            ObjInner::UpValue(_location) => todo!(),
            ObjInner::Class(class) => write!(f, "class {}", class.name),
            ObjInner::ObjInstance(instance) => write!(f, "{} instance", instance.class),
            ObjInner::BoundMethod(bound_method) => {
                let class = &bound_method
                    .receiver
                    .instance()
                    .unwrap()
                    .class
                    .class()
                    .unwrap();
                let fun = bound_method
                    .method
                    .closure()
                    .unwrap()
                    .function
                    .function()
                    .unwrap();
                write!(f, "<fn {}::{}@{}>", class.name, fun.name, fun.arity)
            }
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
            if let ObjInner::String(s) = &**unsafe { &**ptr } {
                return Some(s);
            }
        }
        None
    }

    pub fn function_mut(&mut self) -> Option<&mut Function> {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let ObjInner::Function(ref mut f) = **obj {
                return Some(f);
            }
        }
        None
    }

    pub fn function(&self) -> Option<&Function> {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let ObjInner::Function(f) = &obj.inner {
                return Some(f);
            }
        }
        None
    }

    pub fn closure_mut(&mut self) -> Option<&mut Closure> {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let ObjInner::Closure(ref mut f) = **obj {
                return Some(f);
            }
        }
        None
    }
    pub fn closure(&self) -> Option<&Closure> {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let ObjInner::Closure(f) = &obj.inner {
                return Some(f);
            }
        }
        None
    }
    pub fn upvalue_mut(&mut self) -> Option<&mut UpValue> {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let ObjInner::UpValue(ref mut v) = &mut obj.inner {
                return Some(v);
            }
        }
        None
    }
    pub fn upvalue(&self) -> Option<&UpValue> {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let ObjInner::UpValue(v) = &**obj {
                return Some(v);
            }
        }
        None
    }

    pub fn class_mut(&mut self) -> Option<&mut Class> {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let ObjInner::Class(ref mut c) = &mut obj.inner {
                return Some(c);
            }
        }
        None
    }

    pub fn class(&self) -> Option<&Class> {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let ObjInner::Class(c) = &**obj {
                return Some(c);
            }
        }
        None
    }

    pub fn instance_mut(&mut self) -> Option<&mut ObjInstance> {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let ObjInner::ObjInstance(ref mut v) = &mut obj.inner {
                return Some(v);
            }
        }
        None
    }

    pub fn instance(&self) -> Option<&ObjInstance> {
        if let Value::Obj(ptr) = self {
            let obj: &Obj = unsafe { &**ptr };
            if let ObjInner::ObjInstance(i) = &obj.inner {
                return Some(i);
            }
        }
        None
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        if let Value::Obj(ptr) = self {
            let obj: &mut Obj = unsafe { &mut **ptr };
            if let ObjInner::Function(f) = &mut **obj {
                return &mut f.chunk;
            }
        }
        todo!()
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
