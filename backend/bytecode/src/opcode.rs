use crate::reg::Register;

#[derive(Debug, Clone)]
pub enum Opcode {
    Load { value: f64, dest: Register },
    Add { left: Register, right: Register, dest: Register },
    Print { value: Register },
    Store { name: String, value: Register },
}

impl std::fmt::Display for Opcode {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Load { value, dest } =>      write!(f, "load  {} {}",    value, dest),
            Opcode::Add { left, right, dest } => write!(f, "add   {} {} {}", left,  right, dest),
            Opcode::Print { value } =>           write!(f, "print {}",      value),
            Opcode::Store { name, value } =>     write!(f, "store {} {}",   name, value),
        }
    }
}
