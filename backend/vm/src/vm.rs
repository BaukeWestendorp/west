use std::collections::HashMap;
use std::io::Write;

use bytecode::module::BytecodeModule;
use bytecode::opcode::Opcode;
use bytecode::reg::{RegOrImm, Register};
use bytecode::value::Value;

pub struct Vm<'w, W>
where
    W: Write + 'w,
{
    modules: Vec<BytecodeModule>,

    registers: HashMap<Register, Value>,

    writer: &'w mut W,

    // FIMXE: Right now the instruction pointer only works for a single module.
    ip: usize,
}

impl<'w, W> Vm<'w, W>
where
    W: Write + 'w,
{
    pub fn new(modules: Vec<BytecodeModule>, writer: &'w mut W) -> Self {
        Self { modules, registers: HashMap::new(), writer, ip: 0 }
    }

    pub fn run(mut self) {
        let modules = std::mem::take(&mut self.modules);
        for module in modules {
            self.run_module(&module);
        }
    }

    fn run_module(&mut self, module: &BytecodeModule) {
        for opcode in module.opcodes() {
            self.run_opcode(opcode, module);
        }
    }

    fn run_opcode(&mut self, opcode: &Opcode, module: &BytecodeModule) {
        match opcode {
            Opcode::Load { value, dest } => {
                let value = self.read_reg_or_imm(value).clone();
                self.allocate_register(*dest, value);
            }

            Opcode::Add { left, right, dest } => {
                let left = self.read_reg_or_imm(left);
                let right = self.read_reg_or_imm(right);

                let value = match (left, right) {
                    (Value::Int(left), Value::Int(right)) => Value::Int(left + right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
                    _ => panic!("invalid types for add operation"),
                };

                self.allocate_register(*dest, value);
            }
            Opcode::Sub { left, right, dest } => {
                let left = self.read_reg_or_imm(left);
                let right = self.read_reg_or_imm(right);

                let value = match (left, right) {
                    (Value::Int(left), Value::Int(right)) => Value::Int(left - right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
                    _ => panic!("invalid types for add operation"),
                };

                self.allocate_register(*dest, value);
            }
            Opcode::Mul { left, right, dest } => {
                let left = self.read_reg_or_imm(left);
                let right = self.read_reg_or_imm(right);

                let value = match (left, right) {
                    (Value::Int(left), Value::Int(right)) => Value::Int(left * right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
                    _ => panic!("invalid types for add operation"),
                };

                self.allocate_register(*dest, value);
            }
            Opcode::Div { left, right, dest } => {
                let left = self.read_reg_or_imm(left);
                let right = self.read_reg_or_imm(right);

                let value = match (left, right) {
                    (Value::Int(left), Value::Int(right)) => Value::Int(left / right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                    _ => panic!("invalid types for add operation"),
                };

                self.allocate_register(*dest, value);
            }
            Opcode::Not { value, dest } => {
                let value = self.read_reg_or_imm(value);

                let value = match value {
                    Value::Bool(value) => Value::Bool(!value),
                    _ => panic!("invalid type for not operation"),
                };

                self.allocate_register(*dest, value);
            }

            Opcode::Jump { label } => self.ip = module.get_label_address(label),
            Opcode::Return { value } => {
                if let Some(value) = value {
                    let value = self.read_reg_or_imm(value).clone();
                    self.set_register(Register::R0, value);
                }
            }

            Opcode::Print { value } => {
                let value = self.read_register(value).clone();
                writeln!(self.writer, "{}", value).expect("should write to writer");
            }
        }
    }

    fn allocate_register(&mut self, reg: Register, value: Value) {
        self.registers.insert(reg, value);
    }

    fn set_register(&mut self, reg: Register, value: Value) {
        self.registers.insert(reg, value);
    }

    fn read_register(&self, reg: &Register) -> &Value {
        self.registers.get(reg).expect("register should exist")
    }

    fn read_reg_or_imm<'a>(&'a self, reg_or_imm: &'a RegOrImm) -> &'a Value {
        match reg_or_imm {
            RegOrImm::Register(reg) => self.read_register(&reg),
            RegOrImm::Immediate(imm) => imm,
        }
    }
}
