use crate::ast::{
    Ast, Block, Expr, ExprKind, Fn, InfixOp, ItemKind, LiteralKind, Module, PrefixOp, Stmt,
    StmtKind,
};
use crate::bytecode::module::{BytecodeModule, Label};
use crate::bytecode::opcode::Opcode;
use crate::bytecode::reg::{RegOrImm, Register};
use crate::bytecode::value::Value;

struct Local<'src> {
    depth: usize,
    name: &'src str,
    reg: Register,
}

pub struct Compiler<'src> {
    ast: &'src Ast<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(ast: &'src Ast<'src>) -> Compiler<'src> {
        Compiler { ast }
    }

    pub fn compile(&mut self) -> Vec<BytecodeModule> {
        self.ast.modules.iter().map(|module| ModuleCompiler::new(module).compile()).collect()
    }
}

struct ModuleCompiler<'src> {
    module: &'src Module<'src>,
    bc_module: BytecodeModule,

    locals: Vec<Local<'src>>,
    depth: usize,

    /// Whether the current function can return on a different code flow path.
    can_return_on_other_path: bool,
    /// Whether the current function has a return statement
    has_return: bool,

    label_counter: usize,
    reg_counter: u32,
    ip: usize,
}

impl<'src> ModuleCompiler<'src> {
    pub fn new(module: &'src Module<'src>) -> ModuleCompiler<'src> {
        ModuleCompiler {
            module,
            bc_module: BytecodeModule::new(),

            locals: Vec::new(),
            depth: 0,

            can_return_on_other_path: false,
            has_return: false,

            ip: 0,
            label_counter: 0,
            reg_counter: 1,
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

        for param in function.params.iter() {
            let reg = self.add_register();
            self.push(Opcode::Pop { dest: reg });

            // NOTE: We have to add one to the depth, because we are not in the scope of the block yet.
            self.locals.push(Local { depth: self.depth + 1, name: param.name.as_str(), reg });
        }

        self.compile_block(&function.body);

        if !self.has_return {
            self.push(Opcode::Return { value: None });
        }

        self.has_return = false;
    }

    fn compile_block(&mut self, block: &Block<'src>) {
        self.enter_scope();
        for stmt in &block.stmts {
            self.compile_stat(stmt);
            if !self.can_return_on_other_path && self.has_return {
                break;
            }
        }
        self.exit_scope();
    }

    fn compile_stat(&mut self, stmt: &Stmt<'src>) {
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                self.compile_expr(expr);
            }
            StmtKind::Assignment { target, value } => {
                let value_reg = self.compile_expr(value);

                let target = match &target.kind {
                    ExprKind::Ident(ident) => ident.as_str(),
                    _ => todo!("invalid assignment target"),
                };

                let target_reg = self.get_local(target).reg;
                self.push(Opcode::Load { value: value_reg.into(), dest: target_reg });
            }
            StmtKind::Print { value } => {
                let value_reg = self.compile_expr(value);
                self.push(Opcode::Print { value: value_reg.into() });
            }
            StmtKind::Return { value } => {
                let value_reg = value.as_ref().map(|v| RegOrImm::from(self.compile_expr(&v)));
                self.push(Opcode::Return { value: value_reg });
                self.has_return = true;
            }
            StmtKind::IfElse { condition, then_block, else_block } => {
                let condition_reg = self.compile_expr(condition);
                let else_label = self.add_label();
                let end_label = self.add_label();

                // If condition is false, jump to the 'else' block.
                self.push(Opcode::JumpIfFalse {
                    condition: condition_reg.into(),
                    label: else_label,
                });
                // Otherwise, execute the 'then' block, and jump to the end of the statement.
                self.compile_block(then_block);
                self.push(Opcode::Jump { label: end_label });

                // Set the address of the 'else' block.
                self.bc_module.set_label_address(else_label, self.ip);
                // If there is an 'else' block, compile it.
                if let Some(else_block) = else_block {
                    self.compile_block(else_block);
                }

                // Set the address of the end of the statement.
                self.bc_module.set_label_address(end_label, self.ip);
            }
            StmtKind::Let { name, value } => {
                let value_reg = self.compile_expr(value);
                self.locals.push(Local { depth: self.depth, name: name.as_str(), reg: value_reg });
            }
            StmtKind::Loop { body } => {
                let loop_label = self.add_label();
                self.compile_block(body);
                self.push(Opcode::Jump { label: loop_label });
            }
            StmtKind::While { condition, body } => {
                let start_label = self.add_label();
                let end_label = self.add_label();

                let condition_reg = self.compile_expr(condition).into();

                self.push(Opcode::JumpIfFalse { condition: condition_reg, label: end_label });

                self.compile_block(body);

                self.push(Opcode::Jump { label: start_label });

                // Set the address of the end of the statement.
                self.bc_module.set_label_address(end_label, self.ip);
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Register {
        match &expr.kind {
            ExprKind::Literal(literal) => {
                let value = match literal.kind {
                    LiteralKind::Int(value) => Value::Int(value),
                    LiteralKind::Float(value) => Value::Float(value),
                    LiteralKind::Str(value) => Value::Str(value.to_string()),
                    LiteralKind::Bool(value) => Value::Bool(value),
                };

                let reg = self.add_register();
                self.push(Opcode::Load { value: value.into(), dest: reg });

                reg
            }
            ExprKind::Ident(ident) => self.get_local(ident.as_str()).reg,
            ExprKind::UnaryOp { op, rhs } => {
                let rhs_reg = self.compile_expr(rhs);
                let dest_reg = self.add_register();

                match op {
                    PrefixOp::Minus => {
                        self.push(Opcode::Mul {
                            left: rhs_reg.into(),
                            right: Value::Float(-1.0).into(),
                            dest: dest_reg,
                        });
                    }
                    PrefixOp::Negate => {
                        self.push(Opcode::Not { value: rhs_reg.into(), dest: dest_reg });
                    }
                }

                dest_reg
            }
            ExprKind::BinaryOp { lhs, op, rhs } => {
                let lhs_reg = self.compile_expr(lhs);
                let rhs_reg = self.compile_expr(rhs);
                let dest_reg = self.add_register();

                match op {
                    InfixOp::Add => {
                        self.push(Opcode::Add {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    InfixOp::Subtract => {
                        self.push(Opcode::Sub {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    InfixOp::Multiply => {
                        self.push(Opcode::Mul {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                    InfixOp::Divide => {
                        self.push(Opcode::Div {
                            left: lhs_reg.into(),
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }

                    InfixOp::Equals
                    | InfixOp::And
                    | InfixOp::Or
                    | InfixOp::LessThan
                    | InfixOp::MoreThan
                    | InfixOp::LessThanEqual
                    | InfixOp::MoreThanEqual
                    | InfixOp::NotEqual
                    | InfixOp::BitAnd
                    | InfixOp::BitOr => {
                        self.push(Opcode::Cmp {
                            left: lhs_reg.into(),
                            op: *op,
                            right: rhs_reg.into(),
                            dest: dest_reg,
                        });
                    }
                }

                dest_reg
            }
            ExprKind::FnCall { callee, args } => {
                let ExprKind::Ident(callee_label) = &callee.kind else {
                    panic!("invalid fn call. expected callee to be an ident");
                };

                let fn_label = self.bc_module.get_function_label(callee_label.as_str());

                for argument in args {
                    let arg_reg = self.compile_expr(argument);
                    self.push(Opcode::Push { value: arg_reg.into() });
                }

                self.push(Opcode::Call { label: fn_label });
                Register::R0
            }
        }
    }

    fn push(&mut self, opcode: Opcode) {
        self.bc_module.push(opcode);
        self.ip += 1;
    }

    fn enter_scope(&mut self) {
        self.depth += 1;
    }

    fn exit_scope(&mut self) {
        self.depth -= 1;
    }

    fn get_local(&self, name: &str) -> &Local {
        self.locals
            .iter()
            .find(|l| l.depth <= self.depth && l.name == name)
            .expect("local should exist")
    }
}
