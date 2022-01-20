use crate::allocator::Allocator;
use crate::value::{NativeMethod, Value};
use fxhash::FxHashMap;
use std::borrow::BorrowMut;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct ObjString(pub String);

impl ObjString {
    pub fn get_native_method(&self, name: &str) -> Option<NativeMethod> {
        match name {
            "len" => Some(&len),
            "strip" => Some(&strip),
            "split" => Some(&split),
            _ => None,
        }
    }
}

fn len(
    _globals: &FxHashMap<String, Value>,
    _allocator: &mut Allocator,
    this: &mut Value,
    args: &[Value],
) -> Value {
    assert!(args.is_empty());
    (this.string().unwrap().len() as f64).into()
}

fn strip(
    _globals: &FxHashMap<String, Value>,
    allocator: &mut Allocator,
    this: &mut Value,
    args: &[Value],
) -> Value {
    assert!(args.is_empty());
    let ret = this.string().unwrap().trim();
    allocator.allocate_string(ret.to_owned())
}

fn split(
    _globals: &FxHashMap<String, Value>,
    allocator: &mut Allocator,
    this: &mut Value,
    args: &[Value],
) -> Value {
    let this: &str = this.string().unwrap();
    let sep: &str = args[0].string().unwrap();
    let strings: Vec<_> = this
        .split(&sep)
        .map(|s| allocator.borrow_mut().allocate_string(s.to_owned()))
        .collect();
    allocator.allocate_vector(strings)
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
