pub type ConstantIx = usize;

#[derive(Clone)]
pub enum Opcode {
    /// Pushes a constant onto the stack
    Push { value: f64 },
    /// Pop the top value off the stack
    Pop,

    /// Set the value of a local variable
    /// `x = y`
    SetLocal,
    /// Get the value of a local variable
    /// `x`
    GetLocal { slot: usize },

    /// Inverts the sign of the top value on the stack
    /// `-x`
    Minus,

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

    /// Prints the top value on the stack
    Print,
}
