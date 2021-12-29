#[derive(Clone, Debug)]
pub struct Value(pub f64);

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self(v)
    }
}
