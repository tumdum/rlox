use crate::value::{Obj, Value};
use std::mem::transmute;

#[derive(Debug, Default)]
pub struct Allocator {
    all_objects: Vec<*const Obj>,
}

impl Drop for Allocator {
    fn drop(&mut self) {
        eprintln!("Dropping {} objects", self.all_objects.len());
        self.free_all_objects();
    }
}

impl Allocator {
    pub fn make_string(&mut self, v: String) -> Value {
        let b = Box::new(Obj::String(v));
        let obj = Box::into_raw(b);
        debug_assert!(!self.all_objects.contains(&(obj as *const Obj)));
        self.all_objects.push(obj);
        Value::Obj(obj)
    }

    fn free_all_objects(&mut self) {
        for ptr in self.all_objects.drain(..) {
            let ptr: Box<Obj> = unsafe { Box::from_raw(transmute(ptr)) };
            drop(ptr);
        }
    }
}
