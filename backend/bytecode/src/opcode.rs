use crate::reg::{RegOrImm, Register};

#[derive(Debug, Clone)]
pub enum Opcode {
    Load { value: RegOrImm, dest: Register },

    Add { left: RegOrImm, right: RegOrImm, dest: Register },
    Sub { left: RegOrImm, right: RegOrImm, dest: Register },
    Mul { left: RegOrImm, right: RegOrImm, dest: Register },
    Div { left: RegOrImm, right: RegOrImm, dest: Register },
    Not { value: RegOrImm, dest: Register },

    Print { value: Register },

    Store { name: String, value: Register },
}

impl std::fmt::Display for Opcode {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Load { value, dest } =>      write!(f, "load  {} {}",    value, dest),

            Opcode::Add { left, right, dest } => write!(f, "add   {} {} {}", left,  right, dest),
            Opcode::Sub { left, right, dest } => write!(f, "sub   {} {} {}", left,  right, dest),
            Opcode::Mul { left, right, dest } => write!(f, "mul   {} {} {}", left,  right, dest),
            Opcode::Div { left, right, dest } => write!(f, "div   {} {} {}", left,  right, dest),
            Opcode::Not { value, dest } =>       write!(f, "not   {} {}",    value, dest),

            Opcode::Print { value } =>           write!(f, "print {}",      value),
            Opcode::Store { name, value } =>     write!(f, "store {} {}",   name, value),
        }
    }
}
