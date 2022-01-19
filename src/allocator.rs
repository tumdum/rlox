use crate::value::{
    BoundMethod, Class, Closure, Function, Obj, ObjInner, ObjInstance, UpValue, Value,
};
use std::collections::HashSet;
use std::mem::transmute;

const HEAP_GROWTH_FACTOR: usize = 2;

#[derive(Debug)]
pub struct Allocator {
    all_objects: Vec<*mut Obj>,
    values: HashSet<Value>,
    bytes_allocated: usize,
    next_gc: usize,
}

impl Default for Allocator {
    fn default() -> Self {
        Self {
            all_objects: Default::default(),
            values: Default::default(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
        }
    }
}

impl Drop for Allocator {
    fn drop(&mut self) {
        #[cfg(feature = "alloc_logs")]
        {
            println!("Dropping {} objects", self.all_objects.len());
            println!("bytes allocated: {}", self.bytes_allocated);
        }
        self.free_all_objects();
    }
}

impl Allocator {
    pub fn should_gc(&self) -> bool {
        self.next_gc <= self.bytes_allocated
    }
    pub fn sweep(&mut self) {
        self.values.retain(|v| v.is_reachable());
        self.all_objects.retain(|v| {
            let ret: bool = unsafe { &**v }.is_marked;

            if !ret {
                let obj = unsafe { Box::from_raw(*v) };
                drop(obj);
            } else {
                unsafe { &mut **v }.is_marked = false;
            }

            ret
        });
        self.next_gc = self.bytes_allocated * HEAP_GROWTH_FACTOR;
    }

    pub fn allocate_string(&mut self, v: String) -> Value {
        let obj = ObjInner::String(v);
        self.record_object(obj, true)
    }

    pub fn allocate_closure(&mut self, function: Value) -> Value {
        let obj = ObjInner::Closure(Closure {
            function,
            upvalues: vec![],
        });
        self.record_object(obj, false)
    }

    pub fn allocate_function(&mut self, f: Function) -> Value {
        let obj = ObjInner::Function(f);
        self.record_object(obj, false)
    }

    pub fn allocate_upvalue(&mut self, slot: *mut Value) -> Value {
        let obj = ObjInner::UpValue(UpValue {
            location: slot,
            closed: None,
        });
        self.record_object(obj, false)
    }

    pub fn allocate_class(&mut self, name: String) -> Value {
        let class = ObjInner::Class(Class::new(name));
        self.record_object(class, false)
    }

    pub fn allocate_obj_instance(&mut self, class: Value) -> Value {
        let obj_instance = ObjInner::ObjInstance(ObjInstance::new(class));
        self.record_object(obj_instance, false)
    }

    pub fn allocate_bound_method(&mut self, receiver: Value, method: Value) -> Value {
        let bound_method = ObjInner::BoundMethod(BoundMethod { receiver, method });
        self.record_object(bound_method, false)
    }

    pub fn allocate_vector(&mut self, values: Vec<Value>) -> Value {
        let bound_method = ObjInner::Vector(values);
        self.record_object(bound_method, false)
    }

    fn record_object(&mut self, obj: ObjInner, intern: bool) -> Value {
        self.bytes_allocated += obj.size();
        let obj = Obj {
            inner: obj,
            is_marked: false,
        };
        #[cfg(feature = "alloc_logs")]
        let ty = obj.type_name();
        let ptr = Box::into_raw(Box::new(obj));
        #[cfg(feature = "alloc_logs")]
        println!("allocated {:p}: {}", ptr, ty);
        debug_assert!(!self.all_objects.contains(&ptr));
        let value = Value::Obj(ptr);
        if intern && self.values.contains(&value) {
            self.values.get(&value).unwrap().clone()
        } else {
            self.all_objects.push(ptr);
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
