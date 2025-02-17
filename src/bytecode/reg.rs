use super::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Register(u32);

impl Register {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    /// The register used for the return value of a function.
    pub const R0: Register = Register(0);
}

impl std::ops::Deref for Register {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegOrImm {
    Register(Register),
    Immediate(Value),
}

impl std::fmt::Display for RegOrImm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegOrImm::Register(reg) => write!(f, "{}", reg),
            // FIXME: Str's should be escaped here.
            RegOrImm::Immediate(imm) => write!(f, "{}", imm),
        }
    }
}

impl From<Register> for RegOrImm {
    fn from(reg: Register) -> Self {
        RegOrImm::Register(reg)
    }
}

impl From<Value> for RegOrImm {
    fn from(imm: Value) -> Self {
        RegOrImm::Immediate(imm)
    }
}
