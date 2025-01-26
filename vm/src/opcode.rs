pub type ConstantIx = usize;

#[derive(Clone)]
pub enum Opcode {
    /// Pushes a constant onto the stack
    Push(f64),
    /// Pop the top value off the stack
    Pop,

    /// Set the value of a local variable
    /// `x = y`
    SetLocal,
    /// Get the value of a local variable
    /// `x`
    GetLocal(usize),

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

    /// Prints the top value on the stack
    Print,
}
