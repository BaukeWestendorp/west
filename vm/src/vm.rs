use std::collections::VecDeque;

use crate::chunk::Chunk;
use crate::opcode::Opcode;

pub struct Vm {
    chunks: VecDeque<Chunk>,
    stack: Vec<f32>,
}

impl Vm {
    pub fn new() -> Self {
        Self { chunks: VecDeque::new(), stack: Vec::with_capacity(256) }
    }

    pub fn push_chunk(&mut self, chunk: Chunk) {
        self.chunks.push_back(chunk);
    }

    pub fn run(&mut self) {
        while let Some(chunk) = self.chunks.pop_front() {
            self.run_chunk(chunk);
        }
    }

    pub fn run_chunk(&mut self, chunk: Chunk) {
        for opcode in chunk.opcodes() {
            match opcode {
                Opcode::PushConstant(ix) => {
                    let constant = chunk.get_constant(*ix);
                    self.stack.push(constant);
                }

                Opcode::Negate => {
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
                    let value = self.stack.pop().expect("should have a value to be returned");
                    println!("return {value}");
                }
            }
        }
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
                res.push_str(&opcode.disassemble(self));
                res.push_str("\n");
            }

            res
        }
    }

    impl Opcode {
        pub fn disassemble(&self, chunk: &Chunk) -> String {
            match self {
                Opcode::PushConstant(ix) => {
                    format!("Push Constant {ix} ({})", chunk.get_constant(*ix))
                }

                Opcode::Negate => format!("Negate"),

                Opcode::Add => format!("Add"),
                Opcode::Subtract => format!("Subtract"),
                Opcode::Multiply => format!("Multiply"),
                Opcode::Divide => format!("Divide"),

                Opcode::Return => format!("Return"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Vm;
    use crate::chunk::Chunk;
    use crate::opcode::Opcode;

    fn test_chunk(chunk: Chunk) -> Option<f32> {
        let mut vm = Vm::new();
        vm.push_chunk(chunk);
        vm.run();
        vm.stack.last().copied()
    }

    #[test]
    fn test_push_constant() {
        let mut chunk = Chunk::new();
        chunk.add_constant(42.5);
        chunk.write(Opcode::PushConstant(0), 0);
        assert_eq!(test_chunk(chunk), Some(42.5));
    }

    #[test]
    fn test_negate() {
        let mut chunk = Chunk::new();
        chunk.add_constant(42.5);
        chunk.write(Opcode::PushConstant(0), 0);
        chunk.write(Opcode::Negate, 0);
        assert_eq!(test_chunk(chunk), Some(-42.5));
    }

    #[test]
    fn test_add() {
        let mut chunk = Chunk::new();
        chunk.add_constant(42.5);
        chunk.add_constant(7.5);
        chunk.write(Opcode::PushConstant(0), 0);
        chunk.write(Opcode::PushConstant(1), 0);
        chunk.write(Opcode::Add, 0);
        assert_eq!(test_chunk(chunk), Some(50.0));
    }

    #[test]
    fn test_subtract() {
        let mut chunk = Chunk::new();
        chunk.add_constant(42.5);
        chunk.add_constant(7.5);
        chunk.write(Opcode::PushConstant(0), 0);
        chunk.write(Opcode::PushConstant(1), 0);
        chunk.write(Opcode::Subtract, 0);
        assert_eq!(test_chunk(chunk), Some(35.0));
    }

    #[test]
    fn test_multiply() {
        let mut chunk = Chunk::new();
        chunk.add_constant(42.5);
        chunk.add_constant(7.5);
        chunk.write(Opcode::PushConstant(0), 0);
        chunk.write(Opcode::PushConstant(1), 0);
        chunk.write(Opcode::Multiply, 0);
        assert_eq!(test_chunk(chunk), Some(318.75));
    }

    #[test]
    fn test_divide() {
        let mut chunk = Chunk::new();
        chunk.add_constant(25.0);
        chunk.add_constant(2.0);
        chunk.write(Opcode::PushConstant(0), 0);
        chunk.write(Opcode::PushConstant(1), 0);
        chunk.write(Opcode::Divide, 0);
        assert_eq!(test_chunk(chunk), Some(12.5));
    }

    #[test]
    fn test_complex() {
        let mut chunk = Chunk::new();
        chunk.add_constant(1.0);
        chunk.add_constant(2.0);
        chunk.add_constant(3.0);
        chunk.add_constant(4.0);
        chunk.write(Opcode::PushConstant(0), 0);
        chunk.write(Opcode::PushConstant(1), 0);
        chunk.write(Opcode::Add, 0);
        chunk.write(Opcode::PushConstant(2), 0);
        chunk.write(Opcode::PushConstant(3), 0);
        chunk.write(Opcode::Multiply, 0);
        chunk.write(Opcode::Divide, 0);
        assert_eq!(test_chunk(chunk), Some(0.25));
    }
}
