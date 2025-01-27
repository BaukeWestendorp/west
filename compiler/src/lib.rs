use ast::{Ast, Block, Expression, ExpressionId, Ident, Item, Literal, Operator, Statement};
use error::ErrorKind;
use miette::Result;
use west_error::ErrorProducer;
use west_error::source::SourceFile;

mod error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Local<'src> {
    name: Ident<'src>,
    depth: usize,
}

pub struct Compiler<'src> {
    source: &'src SourceFile<'src>,

    ast: &'src Ast<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(ast: &'src Ast<'src>, source: &'src SourceFile<'src>) -> Compiler<'src> {
        Compiler { source, ast }
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

    fn compile_statement_let(&mut self, name: &Ident<'src>, value: &ExpressionId) -> Result<()> {
        todo!();
    }

    fn compile_statement_print(&mut self, value: &ExpressionId) -> Result<()> {
        todo!();
    }

    fn compile_expression(&mut self, expression: &ExpressionId) -> Result<()> {
        match self.ast.get_expression(expression) {
            Expression::Literal(literal) => {
                todo!();
            }
            Expression::Ident(ident) => {
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
        Ok(())
    }

    fn enter_scope(&mut self) {
        todo!();
    }

    fn exit_scope(&mut self) {
        todo!();
    }
}

impl ErrorProducer for Compiler<'_> {
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
