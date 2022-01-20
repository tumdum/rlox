use crate::value::Value;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct ObjInstance {
    pub class: Value,
    // fields: FxHashMap<String, Value>, TODO: make it a hash map/faster/hashable
    fields: std::collections::BTreeMap<String, Value>,
}

impl Display for ObjInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}{{", self.class.class().unwrap().name)?;
        let mut first = true;
        for (name, value) in &self.fields {
            if first {
                first = false;
            } else {
                write!(f, " ")?;
            }
            write!(f, "{}={}", name, value)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl ObjInstance {
    pub fn new(class: Value) -> ObjInstance {
        Self {
            class,
            fields: Default::default(),
        }
    }

    pub fn mark(&mut self) {
        self.class.mark();
        self.fields.values_mut().for_each(|v| v.mark());
    }

    pub fn get_field(&self, name: &str) -> Option<&Value> {
        self.fields.get(name)
    }

    pub fn set_field(&mut self, name: String, value: Value) {
        self.fields.insert(name, value);
    }
}
