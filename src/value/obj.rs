use crate::value::ObjInner;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct Obj {
    pub inner: ObjInner,
    pub is_marked: bool,
}

impl Obj {
    pub fn mark(&mut self) {
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
            ObjInner::Vector(vec) => {
                write!(f, "[")?;
                let mut first = true;
                for v in &vec.0 {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
        }
    }
}
