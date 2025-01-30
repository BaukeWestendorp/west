#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

impl Value {
    pub fn as_int(&self) -> i64 {
        match self {
            Value::Int(v) => *v,
            _ => panic!("expected an int"),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            Value::Float(v) => *v,
            _ => panic!("expected a float"),
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Value::Str(v) => v,
            _ => panic!("expected a string"),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(v) => *v,
            _ => panic!("expected a bool"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Str(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
        }
    }
}
