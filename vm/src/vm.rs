use std::collections::VecDeque;
use std::io::Write;

use miette::Result;

use crate::chunk::Chunk;
use crate::opcode::Opcode;

pub struct Vm<'w, W>
where
    W: Write + 'w,
{
    chunks: VecDeque<Chunk>,
    stack: Vec<f64>,

    writer: &'w mut W,
}

impl<'w, W> Vm<'w, W>
where
    W: Write + 'w,
{
    pub fn new(writer: &'w mut W) -> Self {
        Self { chunks: VecDeque::new(), stack: Vec::with_capacity(256), writer }
    }

    pub fn push_chunk(&mut self, chunk: Chunk) {
        self.chunks.push_back(chunk);
    }

    pub fn run(&mut self) -> Result<()> {
        while let Some(chunk) = self.chunks.pop_front() {
            self.run_chunk(chunk)?;
        }

        Ok(())
    }

    pub fn run_chunk(&mut self, chunk: Chunk) -> Result<()> {
        for opcode in chunk.opcodes() {
            match opcode {
                Opcode::Push { value } => {
                    self.stack.push(*value);
                }
                Opcode::Pop => {
                    self.stack.pop().expect("should have a value to pop");
                }

                Opcode::GetLocal { slot } => {
                    let value = self.stack[*slot];
                    self.stack.push(value);
                }
                Opcode::SetLocal => {
                    let value = self.stack.pop().expect("should have a value to set");
                    self.stack.push(value);
                }

                Opcode::Minus => {
                    let value = self.stack.pop().expect("should have a value to negate");
                    self.stack.push(-value);
                }

                Opcode::Add => {
                    let b = self.stack.pop().expect("should have a value to add");
                    let a = self.stack.pop().expect("should have a value to add");
                    self.stack.push(a + b);
                }

                Opcode::Subtract => {
                    let b = self.stack.pop().expect("should have a value to subtract");
                    let a = self.stack.pop().expect("should have a value to subtract");
                    self.stack.push(a - b);
                }

                Opcode::Multiply => {
                    let b = self.stack.pop().expect("should have a value to multiply");
                    let a = self.stack.pop().expect("should have a value to multiply");
                    self.stack.push(a * b);
                }

                Opcode::Divide => {
                    let b = self.stack.pop().expect("should have a value to divide");
                    let a = self.stack.pop().expect("should have a value to divide");
                    self.stack.push(a / b);
                }

                Opcode::Return => {
                    todo!();
                }

                Opcode::Print => {
                    let value = self.stack.last().expect("should have a value to print");
                    let res = write!(self.writer, "{value}");
                    match res {
                        Ok(_) => (),
                        Err(err) => miette::bail!("failed to write: {err}"),
                    }
                }
            }
        }

        Ok(())
    }
}

mod disassembler {
    use crate::chunk::Chunk;
    use crate::opcode::Opcode;

    impl Chunk {
        pub fn disassemble(&self) -> String {
            let mut res = String::new();
            res.push_str("==== chunk ====\n");

            for (offset, opcode) in self.opcodes().enumerate() {
                let line = self.line_for_offset(offset);
                if offset > 0 && self.line_for_offset(offset - 1) == line {
                    res.push_str(&format!("{offset:04}    | "));
                } else {
                    res.push_str(&format!("{offset:04} {:4} ", line));
                }
                res.push_str(&opcode.disassemble());
                res.push('\n');
            }

            res
        }
    }

    impl Opcode {
        pub fn disassemble(&self) -> String {
            match self {
                Opcode::Push { value } => format!("Push            {{ value = {value} }}"),
                Opcode::Pop => "Pop".to_string(),

                Opcode::GetLocal { slot } => format!("GetLocal        {{ slot = {slot} }}"),
                Opcode::SetLocal => format!("SetLocal"),

                Opcode::Minus => "Minus".to_string(),

                Opcode::Add => "Add".to_string(),
                Opcode::Subtract => "Subtract".to_string(),
                Opcode::Multiply => "Multiply".to_string(),
                Opcode::Divide => "Divide".to_string(),

                Opcode::Return => "Return".to_string(),
                Opcode::Print => "Print".to_string(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Vm;
    use crate::chunk::Chunk;
    use crate::opcode::Opcode;

    fn test_chunk(chunk: Chunk) -> Option<f64> {
        let mut output = std::io::stdout();
        let mut vm = Vm::new(&mut output);
        vm.push_chunk(chunk);
        vm.run().unwrap();
        vm.stack.last().copied()
    }

    #[test]
    fn push() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 42.5 }, 0);
        assert_eq!(test_chunk(chunk), Some(42.5));
    }

    #[test]
    fn pop() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 42.5 }, 0);
        chunk.write(Opcode::Pop, 0);
        assert_eq!(test_chunk(chunk), None);
    }

    #[test]
    fn get_local() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 42.5 }, 0);
        chunk.write(Opcode::Push { value: 22.5 }, 0);
        chunk.write(Opcode::GetLocal { slot: 1 }, 0);
        assert_eq!(test_chunk(chunk), Some(22.5));
    }

    #[test]
    fn set_local() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 42.5 }, 0);
        chunk.write(Opcode::SetLocal, 0);
        chunk.write(Opcode::GetLocal { slot: 0 }, 0);
        assert_eq!(test_chunk(chunk), Some(42.5));
    }

    #[test]
    fn negate() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 42.5 }, 0);
        chunk.write(Opcode::Minus, 0);
        assert_eq!(test_chunk(chunk), Some(-42.5));
    }

    #[test]
    fn add() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 42.5 }, 0);
        chunk.write(Opcode::Push { value: 7.5 }, 0);
        chunk.write(Opcode::Add, 0);
        assert_eq!(test_chunk(chunk), Some(50.0));
    }

    #[test]
    fn subtract() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 42.5 }, 0);
        chunk.write(Opcode::Push { value: 7.5 }, 0);
        chunk.write(Opcode::Subtract, 0);
        assert_eq!(test_chunk(chunk), Some(35.0));
    }

    #[test]
    fn multiply() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 42.5 }, 0);
        chunk.write(Opcode::Push { value: 7.5 }, 0);
        chunk.write(Opcode::Multiply, 0);
        assert_eq!(test_chunk(chunk), Some(318.75));
    }

    #[test]
    fn divide() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 25.0 }, 0);
        chunk.write(Opcode::Push { value: 2.0 }, 0);
        chunk.write(Opcode::Divide, 0);
        assert_eq!(test_chunk(chunk), Some(12.5));
    }

    #[test]
    fn complex() {
        let mut chunk = Chunk::new();
        chunk.write(Opcode::Push { value: 1.0 }, 0);
        chunk.write(Opcode::Push { value: 2.0 }, 0);
        chunk.write(Opcode::Add, 0);
        chunk.write(Opcode::Push { value: 3.0 }, 0);
        chunk.write(Opcode::Push { value: 4.0 }, 0);
        chunk.write(Opcode::Multiply, 0);
        chunk.write(Opcode::Divide, 0);
        assert_eq!(test_chunk(chunk), Some(0.25));
    }
}
