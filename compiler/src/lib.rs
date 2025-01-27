use ast::{Ast, Block, Expression, ExpressionId, Ident, Item, Operator, Statement};
use error::ErrorKind;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use miette::Result;
use west_error::ErrorProducer;
use west_error::source::SourceFile;

mod error;

struct CodeGen<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

pub struct Compiler<'src, 'ctx> {
    source: &'src SourceFile<'src>,

    ast: &'src Ast<'src>,

    codegen: CodeGen<'ctx>,
}

impl<'src, 'ctx> Compiler<'src, 'ctx> {
    pub fn new(
        ast: &'src Ast<'src>,
        source: &'src SourceFile<'src>,
        ctx: &'ctx Context,
    ) -> Compiler<'src, 'ctx> {
        let module = ctx.create_module("main");
        let builder = ctx.create_builder();
        let codegen = CodeGen { ctx: &ctx, module, builder };
        Compiler { source, ast, codegen }
    }

    pub fn compile(&mut self) -> Result<()> {
        let Item::Fn(main) = &self.ast.files[0].items[0];

        self.compile_block(&main.body)?;

        Ok(())
    }

    fn compile_block(&mut self, block: &Block<'src>) -> Result<()> {
        self.enter_scope();
        for statement in &block.statements {
            self.compile_statement(statement)?;
        }
        self.exit_scope();
        Ok(())
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
