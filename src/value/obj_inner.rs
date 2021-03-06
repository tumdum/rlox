use crate::value::{
    BoundMethod, Class, Closure, Function, NativeMethod, ObjInstance, ObjString, UpValue, Vector,
};

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub enum ObjInner {
    String(ObjString),
    Function(Function),
    Closure(Closure),
    UpValue(UpValue),
    Class(Class),
    ObjInstance(ObjInstance),
    BoundMethod(BoundMethod),
    Vector(Vector),
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
            Self::Vector(_) => "vector",
        }
    }

    pub fn mark(&mut self) {
        match self {
            Self::String(_s) => {}
            Self::Function(function) => function.mark(),
            Self::Closure(closure) => closure.mark(),
            Self::UpValue(upvalue) => upvalue.mark(),
            Self::Class(class) => class.mark(),
            Self::ObjInstance(instance) => instance.mark(),
            Self::BoundMethod(bound_method) => bound_method.mark(),
            Self::Vector(v) => v.iter_mut().for_each(|v| v.mark()),
        }
    }

    pub fn get_native_method(&self, name: &str) -> Option<NativeMethod> {
        match self {
            Self::Vector(v) => v.get_native_method(name),
            Self::String(v) => v.get_native_method(name),
            _ => None,
        }
    }
}
