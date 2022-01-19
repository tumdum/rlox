use crate::value::{
    BoundMethod, Class, Closure, Function, Obj, ObjInner, ObjInstance, ObjString, UpValue, Value,
    Vector,
};
use std::collections::HashSet;
use std::mem::transmute;

const GROWTH_FACTOR: usize = 2;

#[derive(Debug)]
pub struct Allocator {
    all_objects: Vec<*mut Obj>,
    interned: HashSet<Value>,
    next_gc: usize,
}

impl Default for Allocator {
    fn default() -> Self {
        Self {
            all_objects: Default::default(),
            interned: Default::default(),
            next_gc: 128,
        }
    }
}

impl Drop for Allocator {
    fn drop(&mut self) {
        #[cfg(feature = "alloc_logs")]
        println!("Dropping {} objects", self.all_objects.len());

        self.free_all_objects();
    }
}

impl Allocator {
    pub fn should_gc(&self) -> bool {
        let ret = self.next_gc <= self.all_objects.len();
        #[cfg(feature = "alloc_logs")]
        if ret {
            println!("GC needed: {} <= {}", self.next_gc, self.all_objects.len());
        }
        ret
    }

    pub fn sweep(&mut self) {
        let objects = self.all_objects.len();
        self.interned.retain(|v| v.is_reachable());
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
        #[cfg(feature = "alloc_logs")]
        println!("GC live objects {} -> {}", objects, self.all_objects.len());
        assert!(self.all_objects.len() <= objects);
        self.next_gc = objects * GROWTH_FACTOR;
    }

    pub fn allocate_string(&mut self, v: String) -> Value {
        let obj = ObjInner::String(ObjString(v));
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
        let bound_method = ObjInner::Vector(Vector(values));
        self.record_object(bound_method, false)
    }

    fn record_object(&mut self, obj: ObjInner, intern: bool) -> Value {
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
        if intern && self.interned.contains(&value) {
            self.interned.get(&value).unwrap().clone()
        } else {
            self.all_objects.push(ptr);
            self.interned.insert(value.clone());
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
