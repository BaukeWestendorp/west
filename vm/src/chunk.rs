use crate::opcode::Opcode;

pub struct Chunk {
    opcodes: Vec<Opcode>,
    constants: Vec<f64>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { opcodes: Vec::new(), constants: Vec::new(), lines: Vec::new() }
    }

    pub fn write(&mut self, opcode: Opcode, line: usize) {
        self.lines.push(line);
        self.opcodes.push(opcode);
    }

    pub fn opcodes(&self) -> impl Iterator<Item = &Opcode> {
        self.opcodes.iter()
    }

    pub fn add_constant(&mut self, constant: f64) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    pub fn get_constant(&self, ix: usize) -> f32 {
        self.constants[ix] as f32
    }

    pub fn line_for_offset(&self, offset: usize) -> usize {
        self.lines[offset]
    }
}
