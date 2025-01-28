use std::collections::HashMap;
use std::io::Write;

use bytecode::module::BytecodeModule;
use bytecode::opcode::Opcode;
use bytecode::reg::{RegOrImm, Register};

pub struct Vm<'w, W>
where
    W: Write + 'w,
{
    modules: Vec<BytecodeModule>,

    registers: HashMap<Register, f64>,

    writer: &'w mut W,
}

impl<'w, W> Vm<'w, W>
where
    W: Write + 'w,
{
    pub fn new(modules: Vec<BytecodeModule>, writer: &'w mut W) -> Self {
        Self { modules, registers: HashMap::new(), writer }
    }

    pub fn run(mut self) {
        let modules = std::mem::take(&mut self.modules);
        for module in modules {
            self.run_module(&module);
        }
    }

    fn run_module(&mut self, module: &BytecodeModule) {
        for opcode in module.opcodes() {
            self.run_opcode(opcode);
        }
    }

    fn run_opcode(&mut self, opcode: &Opcode) {
        match opcode {
            Opcode::Load { value, dest } => {
                let value = self.read_reg_or_imm(*value);
                self.allocate_register(*dest, value);
            }

            Opcode::Add { left, right, dest } => {
                let left = self.read_reg_or_imm(*left);
                let right = self.read_reg_or_imm(*right);
                self.allocate_register(*dest, left + right);
            }
            Opcode::Sub { left, right, dest } => {
                let left = self.read_reg_or_imm(*left);
                let right = self.read_reg_or_imm(*right);
                self.allocate_register(*dest, left - right);
            }
            Opcode::Mul { left, right, dest } => {
                let left = self.read_reg_or_imm(*left);
                let right = self.read_reg_or_imm(*right);
                self.allocate_register(*dest, left * right);
            }
            Opcode::Div { left, right, dest } => {
                let left = self.read_reg_or_imm(*left);
                let right = self.read_reg_or_imm(*right);
                self.allocate_register(*dest, left / right);
            }
            Opcode::Not { value, dest } => {
                let value = self.read_reg_or_imm(*value);
                self.allocate_register(*dest, if value == 0.0 { 1.0 } else { 0.0 });
            }

            Opcode::Print { value } => {
                let value = *self.read_register(value);
                writeln!(self.writer, "{}", value).expect("should write to writer");
            }
        }
    }

    fn allocate_register(&mut self, reg: Register, value: f64) {
        self.registers.insert(reg, value);
    }

    fn read_register(&self, reg: &Register) -> &f64 {
        self.registers.get(reg).expect("register should exist")
    }

    fn read_reg_or_imm(&self, reg_or_imm: RegOrImm) -> f64 {
        match reg_or_imm.into() {
            RegOrImm::Register(reg) => *self.read_register(&reg),
            RegOrImm::Immediate(imm) => imm,
        }
    }
}
