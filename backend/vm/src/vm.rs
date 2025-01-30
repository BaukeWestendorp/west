use std::collections::HashMap;
use std::io::Write;

use bytecode::module::{Address, BytecodeModule};
use bytecode::opcode::Opcode;
use bytecode::reg::{RegOrImm, Register};
use bytecode::value::Value;

pub struct Vm<'w, W>
where
    W: Write + 'w,
{
    module: BytecodeModule,

    registers: HashMap<Register, Value>,
    constant_stack: Vec<Value>,
    call_stack: Vec<Address>,

    writer: &'w mut W,

    // FIMXE: Right now the instruction pointer only works for a single module.
    ip: Address,
}

impl<'w, W> Vm<'w, W>
where
    W: Write + 'w,
{
    pub fn new(module: BytecodeModule, writer: &'w mut W) -> Self {
        let entry_address = module.get_entry_address();
        Self {
            module,
            registers: HashMap::new(),
            call_stack: vec![],
            constant_stack: vec![],
            writer,
            ip: entry_address,
        }
    }

    pub fn run(mut self) {
        while self.ip < self.module.opcodes().len() {
            // FIXME: We should not clone the opcode here.
            let opcode = self.module.opcodes()[self.ip].clone();
            self.run_opcode(&opcode);
        }
    }

    fn run_opcode(&mut self, opcode: &Opcode) {
        self.ip += 1;

        match opcode {
            Opcode::Load { value, dest } => {
                let value = self.read_reg_or_imm(value).clone();
                self.allocate_register(*dest, value.clone());
            }
            Opcode::Push { value } => {
                self.constant_stack.push(self.read_reg_or_imm(value).clone());
            }
            Opcode::Pop { dest } => {
                let value = self.constant_stack.pop().expect("stack should not be empty");
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

            Opcode::Jump { label } => {
                self.call_stack.push(self.ip);
                self.ip = self.module.get_label_address(label)
            }
            Opcode::Return { value } => {
                if let Some(value) = value {
                    let value = self.read_reg_or_imm(value).clone();
                    self.set_register(Register::R0, value);
                }
                match self.call_stack.pop() {
                    Some(address) => self.ip = address,
                    None => {
                        // Returned from main function.
                        self.ip = self.module.opcodes().len();
                    }
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
        self.registers.get(reg).expect(&format!("register should exist: {reg}"))
    }

    fn read_reg_or_imm<'a>(&'a self, reg_or_imm: &'a RegOrImm) -> &'a Value {
        match reg_or_imm {
            RegOrImm::Register(reg) => self.read_register(&reg),
            RegOrImm::Immediate(imm) => imm,
        }
    }
}
