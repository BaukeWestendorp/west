use crate::opcode::Opcode;

#[derive(Clone)]
pub struct Chunk {
    opcodes: Vec<Opcode>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { opcodes: Vec::new(), lines: Vec::new() }
    }

    pub fn write(&mut self, opcode: Opcode, line: usize) {
        self.lines.push(line);
        self.opcodes.push(opcode);
    }

    pub fn opcodes(&self) -> impl Iterator<Item = &Opcode> {
        self.opcodes.iter()
    }

    pub fn line_for_offset(&self, offset: usize) -> usize {
        self.lines[offset]
    }
}
