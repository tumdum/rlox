use crate::value::{NativeMethod, Value};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct ObjString(pub String);

impl ObjString {
    pub fn get_native_method(&self, name: &str) -> Option<NativeMethod> {
        match name {
            "len" => Some(&len),
            _ => None,
        }
    }
}

fn len(this: &Value, args: &[Value]) -> Value {
    assert!(args.is_empty());
    (this.string().unwrap().len() as f64).into()
}

impl Display for ObjString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl Deref for ObjString {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}
