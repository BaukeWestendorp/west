use ast::{Ast, Item, Module};
use bytecode::BytecodeModule;

pub struct Compiler<'src> {
    ast: &'src Ast<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(ast: &'src Ast<'src>) -> Compiler<'src> {
        Compiler { ast }
    }

    pub fn compile(&mut self) -> Vec<BytecodeModule> {
        self.ast.modules.iter().map(|module| self.compile_module(module)).collect()
    }

    fn compile_module(&mut self, module: &Module<'src>) -> BytecodeModule {
        let mut bytecode_module = BytecodeModule::new();
        for item in &module.items {
            match item {
                Item::Fn(function) => self.compile_item_fn(function, &mut bytecode_module),
            }
        }
        bytecode_module
    }

    fn compile_item_fn(&mut self, function: &ast::Fn<'src>, module: &mut BytecodeModule) {
        todo!();
    }
}
