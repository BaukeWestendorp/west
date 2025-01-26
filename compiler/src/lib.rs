use ast::{Ast, Expression, ExpressionId, Item, Literal, Operator, Statement};
use lexer::source::SourceFile;
use miette::Result;
use vm::chunk::Chunk;
use vm::opcode::Opcode;

pub struct Compiler<'src> {
    ast: &'src Ast<'src>,

    current_chunk: Chunk,
}

impl<'src> Compiler<'src> {
    pub fn new(ast: &'src Ast<'src>, _source: &'src SourceFile<'src>) -> Compiler<'src> {
        Compiler { ast, current_chunk: Chunk::new() }
    }

    pub fn compile(&mut self) -> Result<&Chunk> {
        let Item::Fn(main) = self.ast.files[0].items.first().unwrap();

        let statement = main.body.statements.first().unwrap();
        self.compile_statement(statement)?;

        // FIXME: We should remove this when we have implemented functions.
        self.current_chunk.write(Opcode::Return, 0);

        Ok(&self.current_chunk)
    }

    fn compile_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Expression(expression) => self.compile_expression(expression)?,
        }
        Ok(())
    }

    fn compile_expression(&mut self, expression: &ExpressionId) -> Result<()> {
        match self.ast.get_expression(expression) {
            Expression::Literal(literal) => {
                let float = match literal {
                    Literal::Float(float) => float,
                    _ => unimplemented!(),
                };

                self.current_chunk.write(Opcode::Value(*float), 0);
            }
            Expression::BinaryOp { lhs, op, rhs } => {
                self.compile_expression(&lhs)?;
                self.compile_expression(&rhs)?;
                match op {
                    Operator::Add => self.current_chunk.write(Opcode::Add, 0),
                    Operator::Subtract => self.current_chunk.write(Opcode::Subtract, 0),
                    Operator::Multiply => self.current_chunk.write(Opcode::Multiply, 0),
                    Operator::Divide => self.current_chunk.write(Opcode::Divide, 0),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
        Ok(())
    }
}
