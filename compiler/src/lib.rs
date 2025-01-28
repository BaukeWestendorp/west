use ast::{Ast, Block, Expression, ExpressionId, Ident, Item, Operator, Statement};
use error::ErrorKind;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use miette::Result;
use west_error::ErrorProducer;
use west_error::source::SourceFile;

mod error;

pub struct Compiler<'src, 'ctx> {
    source: &'src SourceFile<'src>,

    ast: &'src Ast<'src>,

    ctx: &'ctx Context,
    builder: Builder<'ctx>,
}

impl<'src, 'ctx> Compiler<'src, 'ctx> {
    pub fn new(
        ast: &'src Ast<'src>,
        source: &'src SourceFile<'src>,
        ctx: &'ctx Context,
    ) -> Compiler<'src, 'ctx> {
        let builder = ctx.create_builder();
        Compiler { source, ast, ctx: &ctx, builder }
    }

    pub fn compile(mut self) -> Result<Vec<Module<'ctx>>> {
        self.ast.mods.iter().map(|m| self.compile_mod(&m)).collect()
    }

    pub fn compile_mod(&mut self, r#mod: &ast::Mod<'src>) -> Result<Module<'ctx>> {
        let module = self.ctx.create_module("main");

        for item in &r#mod.items {
            self.compile_item(item, &module)?;
        }

        Ok(module)
    }

    fn compile_item(&mut self, item: &Item<'src>, module: &Module<'ctx>) -> Result<()> {
        match item {
            Item::Fn(f) => self.compile_item_fn(f, module),
        }
    }

    fn compile_item_fn(&mut self, f: &ast::Fn<'src>, module: &Module<'ctx>) -> Result<()> {
        // FIMXE: Actually get function return type
        let return_type = self.ctx.i32_type();

        // FIXME: Actually get function parameter types
        let param_types = &[];

        let fn_type = return_type.fn_type(param_types, false);
        let function = module.add_function(&f.name, fn_type, None);
        let basic_block = self.ctx.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);
        self.builder.build_return(Some(&self.ctx.i32_type().const_int(42, true))).unwrap();

        Ok(())
    }

    fn compile_block(&mut self, block: &Block<'src>) -> Result<BasicBlock> {
        todo!();
    }

    fn compile_statement(&mut self, statement: &Statement<'src>) -> Result<()> {
        match statement {
            Statement::Let { name, value } => self.compile_statement_let(name, value)?,
            Statement::Print { value } => self.compile_statement_print(value)?,
        }
        Ok(())
    }

    fn compile_statement_let(&mut self, _name: &Ident<'src>, _value: &ExpressionId) -> Result<()> {
        todo!();
    }

    fn compile_statement_print(&mut self, _value: &ExpressionId) -> Result<()> {
        todo!();
    }

    fn compile_expression(&mut self, expression: &ExpressionId) -> Result<()> {
        match self.ast.get_expression(expression) {
            Expression::Literal(_literal) => {
                todo!();
            }
            Expression::Ident(_ident) => {
                todo!();
            }
            Expression::BinaryOp { lhs, op, rhs } => {
                self.compile_expression(&lhs)?;
                self.compile_expression(&rhs)?;
                match op {
                    Operator::Add => todo!(),
                    Operator::Subtract => todo!(),
                    Operator::Multiply => todo!(),
                    Operator::Divide => todo!(),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn enter_scope(&mut self) {}

    fn exit_scope(&mut self) {}
}

impl ErrorProducer for Compiler<'_, '_> {
    type ErrorKind = ErrorKind;

    fn name(&self) -> &str {
        "compiler"
    }

    fn source(&self) -> &SourceFile {
        self.source
    }

    fn current_span(&mut self) -> std::ops::Range<usize> {
        todo!()
    }
}
