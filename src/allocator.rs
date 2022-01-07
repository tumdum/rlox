use crate::value::{Closure, Function, Obj, UpValue, Value};
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
        let obj = Obj::String(v);
        self.record_object(obj)
    }

    pub fn allocate_closure(&mut self, function: Value) -> Value {
        let obj = Obj::Closure(Closure {
            function,
            upvalues: vec![],
        });
        self.record_object(obj)
    }

    pub fn allocate_function(&mut self, f: Function) -> Value {
        let obj = Obj::Function(f);
        self.record_object(obj)
    }

    pub fn allocate_upvalue(&mut self, slot: *mut Value) -> Value {
        let obj = Obj::UpValue(UpValue { location: slot });
        self.record_object(obj)
    }

    fn record_object(&mut self, obj: Obj) -> Value {
        let obj = Box::into_raw(Box::new(obj));
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
