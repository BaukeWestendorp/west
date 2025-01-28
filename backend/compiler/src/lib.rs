use ast::{Ast, Block, Expression, ExpressionId, Fn, Item, Literal, Module, Operator, Statement};
use bytecode::module::{BytecodeModule, Label};
use bytecode::opcode::Opcode;
use bytecode::reg::Register;

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
            match item {
                Item::Fn(function) => self.compile_item_fn(function),
            }
        }
        self.bc_module
    }

    fn compile_item_fn(&mut self, function: &Fn<'src>) {
        let label = self.add_label();
        self.bc_module.add_function_label(function.name.to_string(), label);

        self.compile_block(&function.body);
    }

    fn compile_block(&mut self, block: &Block<'src>) {
        for statement in &block.statements {
            self.compile_statement(statement);
        }
    }

    fn compile_statement(&mut self, statement: &Statement<'src>) {
        match statement {
            Statement::Print { value } => {
                let value_reg = self.compile_expression(value);
                self.bc_module.push(Opcode::Print { value: value_reg });
            }
            Statement::Let { name, value } => {
                let value_reg = self.compile_expression(value);
                self.bc_module.push(Opcode::Store { name: name.to_string(), value: value_reg });
            }
        }
    }

    fn compile_expression(&mut self, expression: &ExpressionId) -> Register {
        let expression = &self.ast.get_expression(expression);
        match expression {
            Expression::Literal(literal) => {
                let float = match literal {
                    Literal::Float(float) => *float,
                    _ => todo!(),
                };

                let reg = self.add_register();
                self.bc_module.push(Opcode::Load { value: float.into(), dest: reg });

                reg
            }
            Expression::Ident(_ident) => {
                todo!()
            }
            Expression::UnaryOp { op, rhs } => {
                let rhs_reg = self.compile_expression(rhs);
                let dest_reg = self.add_register();

                match op {
                    Operator::Negate => {
                        self.bc_module.push(Opcode::Mul {
                            left: rhs_reg.into(),
                            right: (-1.0).into(),
                            dest: dest_reg,
                        });
                    }
                    Operator::Invert => {
                        self.bc_module.push(Opcode::Not { value: rhs_reg.into(), dest: dest_reg });
                    }
                    // FIXME: Refactor `Operator` to make this `unreachable()` unnecessary.
                    _ => unreachable!(),
                }

                dest_reg
            }
            Expression::BinaryOp { lhs, op, rhs } => {
                let lhs_reg = self.compile_expression(lhs);
                let rhs_reg = self.compile_expression(rhs);
                let dest_reg = self.add_register();

                match op {
                    Operator::Add => {
                        self.bc_module.push(Opcode::Add {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    Operator::Subtract => {
                        self.bc_module.push(Opcode::Sub {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    Operator::Multiply => {
                        self.bc_module.push(Opcode::Mul {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    Operator::Divide => {
                        self.bc_module.push(Opcode::Div {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    // FIXME: Refactor `Operator` to make this `unreachable()` unnecessary.
                    _ => unreachable!(),
                }

                dest_reg
            }
            Expression::FnCall { callee: _callee, args: _args } => todo!(),
        }
    }
}
