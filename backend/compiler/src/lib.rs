use std::collections::HashMap;

use ast::{
    Ast, Block, ExpressionId, ExpressionKind, Fn, InfixOp, ItemKind, LiteralKind, Module, PrefixOp,
    Statement, StatementKind,
};
use bytecode::module::{BytecodeModule, Label};
use bytecode::opcode::Opcode;
use bytecode::reg::Register;
use bytecode::value::Value;

pub struct Compiler<'src> {
    ast: &'src Ast<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(ast: &'src Ast<'src>) -> Compiler<'src> {
        Compiler { ast }
    }

    pub fn compile(&mut self) -> Vec<BytecodeModule> {
        self.ast
            .modules
            .iter()
            .map(|module| ModuleCompiler::new(&self.ast, module).compile())
            .collect()
    }
}

struct ModuleCompiler<'src> {
    ast: &'src Ast<'src>,
    module: &'src Module<'src>,
    bc_module: BytecodeModule,

    locals: HashMap<(usize, &'src str), Register>,
    depth: usize,

    label_counter: usize,
    reg_counter: u32,
    ip: usize,
}

impl<'src> ModuleCompiler<'src> {
    pub fn new(ast: &'src Ast<'src>, module: &'src Module<'src>) -> ModuleCompiler<'src> {
        ModuleCompiler {
            ast,
            module,
            bc_module: BytecodeModule::new(),

            locals: HashMap::new(),
            depth: 0,

            ip: 0,
            label_counter: 0,
            reg_counter: 0,
        }
    }

    fn add_label(&mut self) -> Label {
        let label = self.bc_module.add_label(self.label_counter, self.ip);
        self.label_counter += 1;
        label
    }

    fn add_register(&mut self) -> Register {
        let reg = Register::new(self.reg_counter);
        self.reg_counter += 1;
        reg
    }

    fn compile(mut self) -> BytecodeModule {
        for item in &self.module.items {
            match &item.kind {
                ItemKind::Fn(function) => self.compile_item_fn(function),
            }
        }
        self.bc_module
    }

    fn compile_item_fn(&mut self, function: &Fn<'src>) {
        let label = self.add_label();
        self.bc_module.add_function_label(function.name.to_string(), label);

        self.compile_block(&function.body);

        self.bc_module.push(Opcode::Return { value: Value::Int(0).into() });
    }

    fn compile_block(&mut self, block: &Block<'src>) {
        self.enter_scope();
        for statement in &block.statements {
            self.compile_statement(statement);
        }
        self.exit_scope();
    }

    fn compile_statement(&mut self, statement: &Statement<'src>) {
        match &statement.kind {
            StatementKind::Expression(expression) => {
                self.compile_expression(expression);
            }
            StatementKind::Print { value } => {
                let value_reg = self.compile_expression(value);
                self.bc_module.push(Opcode::Print { value: value_reg });
            }
            StatementKind::Let { name, value } => {
                let value_reg = self.compile_expression(value);
                self.locals.insert((self.depth, name.as_str()), value_reg);
            }
        }
    }

    fn compile_expression(&mut self, expression: &ExpressionId) -> Register {
        let expression = &self.ast.get_expression(expression);
        match &expression.kind {
            ExpressionKind::Literal(literal) => {
                let value = match literal.kind {
                    LiteralKind::Int(value) => Value::Int(value),
                    LiteralKind::Float(value) => Value::Float(value),
                    LiteralKind::Str(value) => Value::Str(value.to_string()),
                    LiteralKind::Bool(value) => Value::Bool(value),
                };

                let reg = self.add_register();
                self.bc_module.push(Opcode::Load { value: value.into(), dest: reg });

                reg
            }
            ExpressionKind::Ident(ident) => {
                *self.locals.get(&(self.depth, ident.as_str())).expect("local should exist")
            }
            ExpressionKind::UnaryOp { op, rhs } => {
                let rhs_reg = self.compile_expression(rhs);
                let dest_reg = self.add_register();

                match op {
                    PrefixOp::Minus => {
                        self.bc_module.push(Opcode::Mul {
                            left: rhs_reg.into(),
                            right: Value::Float(-1.0).into(),
                            dest: dest_reg,
                        });
                    }
                    PrefixOp::Negate => {
                        self.bc_module.push(Opcode::Not { value: rhs_reg.into(), dest: dest_reg });
                    }
                }

                dest_reg
            }
            ExpressionKind::BinaryOp { lhs, op, rhs } => {
                let lhs_reg = self.compile_expression(lhs);
                let rhs_reg = self.compile_expression(rhs);
                let dest_reg = self.add_register();

                match op {
                    InfixOp::Add => {
                        self.bc_module.push(Opcode::Add {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    InfixOp::Subtract => {
                        self.bc_module.push(Opcode::Sub {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    InfixOp::Multiply => {
                        self.bc_module.push(Opcode::Mul {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    InfixOp::Divide => {
                        self.bc_module.push(Opcode::Div {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    op => todo!("implement {op}"),
                }

                dest_reg
            }
            ExpressionKind::FnCall { callee, args: _args } => {
                let ExpressionKind::Ident(callee_label) = &self.ast.get_expression(callee).kind
                else {
                    panic!("invalid fn call. expected callee to be an ident");
                };

                let fn_label = self.bc_module.get_function_label(callee_label.as_str());

                self.bc_module.push(Opcode::Jump { label: fn_label });
                Register::R0
            }
        }
    }

    fn enter_scope(&mut self) {
        self.depth += 1;
    }

    fn exit_scope(&mut self) {
        self.depth -= 1;
    }
}
