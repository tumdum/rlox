use crate::value::{Closure, Function, Obj, Value};
use std::collections::HashSet;
use std::mem::transmute;

#[derive(Debug, Default)]
pub struct Allocator {
    all_objects: Vec<*const Obj>,
    values: HashSet<Value>,
}

impl Drop for Allocator {
    fn drop(&mut self) {
        eprintln!("Dropping {} objects", self.all_objects.len());
        self.free_all_objects();
    }
}

impl Allocator {
    pub fn allocate_string(&mut self, v: String) -> Value {
        let b = Box::new(Obj::String(v));
        let obj = Box::into_raw(b);
        self.record_ptr(obj)
    }

    pub fn allocate_closure(&mut self, function: Value) -> Value {
        let b = Box::new(Obj::Closure(Closure { function }));
        let obj = Box::into_raw(b);
        self.record_ptr(obj)
    }

    pub fn allocate_function(&mut self, f: Function) -> Value {
        let b = Box::new(Obj::Function(f));
        let obj = Box::into_raw(b);
        self.record_ptr(obj)
    }

    fn record_ptr(&mut self, obj: *mut Obj) -> Value {
        debug_assert!(!self.all_objects.contains(&(obj as *const Obj)));
        let value = Value::Obj(obj);
        if self.values.contains(&value) {
            self.values.get(&value).unwrap().clone()
        } else {
            self.all_objects.push(obj);
            self.values.insert(value.clone());
            value
        }
    }

    fn free_all_objects(&mut self) {
        for ptr in self.all_objects.drain(..) {
            let ptr: Box<Obj> = unsafe { Box::from_raw(transmute(ptr)) };
            drop(ptr);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interning() {
        let mut allocator = Allocator::default();
        let s1 = allocator.allocate_string("test".to_owned());
        let s2 = allocator.allocate_string("test".to_owned());
        assert_eq!(s1, s2);
        match (s1, s2) {
            (Value::Obj(p1), Value::Obj(p2)) => assert_eq!(p1, p2),
            _ => unreachable!(),
        }
    }
}
