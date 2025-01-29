use std::collections::HashMap;

use crate::opcode::Opcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label {
    id: usize,
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

pub type Address = usize;

impl Label {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

#[derive(Debug, Clone)]
pub struct BytecodeModule {
    opcodes: Vec<Opcode>,

    labels: HashMap<Label, Address>,
    // FIXME: Store the function using it's path instead.
    function_labels: HashMap<String, Label>,
}

impl BytecodeModule {
    pub fn new() -> Self {
        Self { opcodes: Vec::new(), labels: HashMap::new(), function_labels: HashMap::new() }
    }

    pub fn opcodes(&self) -> &[Opcode] {
        &self.opcodes
    }

    pub fn push(&mut self, opcode: Opcode) {
        self.opcodes.push(opcode);
    }

    pub fn add_label(&mut self, id: usize, address: Address) -> Label {
        let label = Label { id };
        self.labels.insert(label, address);
        label
    }

    pub fn get_label_address(&self, label: &Label) -> Address {
        *self.labels.get(label).expect("an address should exist for every label")
    }

    pub fn add_function_label(&mut self, name: String, label: Label) {
        self.function_labels.insert(name, label);
    }

    pub fn get_function_label(&self, name: &str) -> Label {
        *self.function_labels.get(name).expect("a label should exist for every function")
    }
}

impl std::fmt::Display for BytecodeModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut lines = vec![];

        for (i, opcode) in self.opcodes.iter().enumerate() {
            lines.push(format!("{:8} {}", i, opcode));
        }

        for label in self.labels.keys() {
            let addr = self.get_label_address(label);
            if let Some((path, _)) = self.function_labels.iter().find(|(_, l)| *l == label) {
                lines.insert(addr, format!("-- label {} (fn {})", label, path));
            } else {
                lines.insert(addr, format!("-- label {}", label));
            }
        }

        // The header after the labels to make sure we don't mess up the addresses.
        lines.insert(0, format!("==== bytecode module ===="));

        for line in lines {
            writeln!(f, "{}", line)?;
        }

        Ok(())
    }
}
