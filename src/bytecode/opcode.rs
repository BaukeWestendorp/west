use crate::ast::InfixOp;

use super::module::Label;
use super::reg::{RegOrImm, Register};

#[derive(Debug, Clone)]
pub enum Opcode {
    Load { value: RegOrImm, dest: Register },
    Push { value: RegOrImm },
    Pop { dest: Register },

    Add { left: RegOrImm, right: RegOrImm, dest: Register },
    Sub { left: RegOrImm, right: RegOrImm, dest: Register },
    Mul { left: RegOrImm, right: RegOrImm, dest: Register },
    Div { left: RegOrImm, right: RegOrImm, dest: Register },
    Not { value: RegOrImm, dest: Register },

    Cmp { left: RegOrImm, op: InfixOp, right: RegOrImm, dest: Register },

    Jump { label: Label },
    JumpIfTrue { condition: RegOrImm, label: Label },
    JumpIfFalse { condition: RegOrImm, label: Label },
    Call { label: Label },
    Return { value: Option<RegOrImm> },

    Print { value: RegOrImm },
}

impl std::fmt::Display for Opcode {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Load { value, dest } =>             write!(f, "load         {} -> {}", value, dest),
            Opcode::Push { value } =>                   write!(f, "push         {}", value),
            Opcode::Pop { dest } =>                     write!(f, "pop          -> {}", dest),

            Opcode::Add { left, right, dest } =>        write!(f, "add          {} {} -> {}", left,  right, dest),
            Opcode::Sub { left, right, dest } =>        write!(f, "sub          {} {} -> {}", left,  right, dest),
            Opcode::Mul { left, right, dest } =>        write!(f, "mul          {} {} -> {}", left,  right, dest),
            Opcode::Div { left, right, dest } =>        write!(f, "div          {} {} -> {}", left,  right, dest),
            Opcode::Not { value, dest } =>              write!(f, "not          {} -> {}", value, dest),

            Opcode::Cmp { left, op, right, dest } =>    write!(f, "cmp          {} {} {} -> {}", left, op, right, dest),

            Opcode::Jump { label } =>                   write!(f, "jmp          {}", label),
            Opcode::JumpIfTrue { condition, label } =>  write!(f, "jmp_if_true  {} {}", condition, label),
            Opcode::JumpIfFalse { condition, label } => write!(f, "jmp_if_false {} {}", condition, label),
            Opcode::Call { label } =>                   write!(f, "call         {}", label),
            Opcode::Return { value } =>                 write!(f, "return       {}", value.as_ref().map(|v| v.to_string()).unwrap_or_default()),

            Opcode::Print { value } =>                  write!(f, "print        {}", value),
        }
    }
}
