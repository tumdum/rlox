#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil) || matches!(self, Value::Boolean(false))
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Number(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}
