#[derive(Clone, Debug)]
pub struct Value(f64);

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self(v)
    }
}
