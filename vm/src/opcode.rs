pub type ConstantIx = usize;

pub enum Opcode {
    /// Pushes a constant onto the stack
    PushConstant(usize),

    /// Negates the top value on the stack
    /// `-x`
    Negate,

    /// Adds the top two values on the stack
    /// `x + y`
    Add,
    /// Subtracts the top two values on the stack
    /// `x - y`
    Subtract,
    /// Multiplies the top two values on the stack
    /// `x * y`
    Multiply,
    /// Divides the top two values on the stack
    /// `x / y`
    Divide,

    /// Returns the top value on the stack
    /// `return x`
    Return,
}
