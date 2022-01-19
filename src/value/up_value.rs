use crate::value::Value;

#[derive(Clone, Debug, PartialEq, PartialOrd, Hash)]
pub struct UpValue {
    // TODO: this could be an enum.
    pub location: *mut Value, // This could be an index into stack
    pub closed: Option<Value>,
}

impl UpValue {
    pub fn mark(&mut self) {
        self.closed.as_mut().iter_mut().for_each(|c| c.mark());
    }
}
