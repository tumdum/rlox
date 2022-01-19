use crate::value::{Value};

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct Class {
    pub name: String,
    methods: std::collections::BTreeMap<String, Value>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: Default::default(),
        }
    }

    pub fn get_method(&self, name: &str) -> Option<&Value> {
        self.methods.get(name)
    }

    pub fn add_method(&mut self, name: &str, closure: Value) {
        self.methods.insert(name.to_owned(), closure);
    }

    pub fn mark(&mut self) {
        self.methods.values_mut().for_each(|m| m.mark());
    }

    pub fn copy_methods_to(&self, target: &mut Self) {
        for (name, value) in &self.methods {
            // Err is ok here since we don't wan to override existing method in
            // subclass.
            let _ = target.add_method(name, value.clone());
        }
    }
}
