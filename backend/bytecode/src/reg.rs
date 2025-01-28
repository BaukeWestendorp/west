#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Register(u32);

impl Register {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
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
