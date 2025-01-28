use ast::{Ast, Block, Expression, ExpressionId, Ident, Item, Literal, Operator, Statement};
use error::ErrorKind;
use miette::Result;
use vm::chunk::Chunk;
use vm::opcode::Opcode;
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

    scope_depth: usize,
    locals: Vec<Local<'src>>,

    current_chunk: Chunk,
}

impl<'src> Compiler<'src> {
    pub fn new(ast: &'src Ast<'src>, source: &'src SourceFile<'src>) -> Compiler<'src> {
        Compiler { source, ast, scope_depth: 0, locals: Vec::new(), current_chunk: Chunk::new() }
    }

    pub fn compile(&mut self) -> Result<&Chunk> {
        let Item::Fn(main) = &self.ast.modules[0].items[0];

        self.compile_block(&main.body)?;

        Ok(&self.current_chunk)
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
        self.compile_expression(value)?;
        self.add_local(*name);
        self.current_chunk.write(Opcode::SetLocal, 0);
        Ok(())
    }

    fn compile_statement_print(&mut self, value: &ExpressionId) -> Result<()> {
        self.compile_expression(value)?;
        self.current_chunk.write(Opcode::Print, 0);
        Ok(())
    }

    fn compile_expression(&mut self, expression: &ExpressionId) -> Result<()> {
        match self.ast.get_expression(expression) {
            Expression::Literal(literal) => {
                let float = match literal {
                    Literal::Float(float) => float,
                    _ => unimplemented!(),
                };

                self.current_chunk.write(Opcode::Push { value: *float }, 0);
            }
            Expression::Ident(ident) => {
                if let Some(slot) = self.resolve_local(ident) {
                    self.current_chunk.write(Opcode::GetLocal { slot }, 0);
                } else {
                    unimplemented!();
                }
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

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        while let Some(Local { depth, .. }) = self.locals.last() {
            if *depth != self.scope_depth {
                break;
            }

            self.locals.pop();
        }

        self.scope_depth -= 1;
    }

    fn add_local(&mut self, name: Ident<'src>) {
        if self.locals.contains(&Local { name, depth: self.scope_depth }) {
            return;
        }

        self.locals.push(Local { name, depth: self.scope_depth });
    }

    fn resolve_local(&self, name: &Ident<'src>) -> Option<usize> {
        self.locals
            .iter()
            .rev()
            .enumerate()
            .find_map(|(i, local)| if local.name == *name { Some(i) } else { None })
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
