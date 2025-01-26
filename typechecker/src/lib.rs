use std::ops::Range;

use ast::Ast;
use error::ErrorKind;
use west_error::ErrorProducer;
use west_error::source::SourceFile;

mod error;

pub struct TypeId(pub usize);

pub struct Typechecker<'src> {
    source: &'src SourceFile<'src>,
    ast: &'src Ast<'src>,
}

impl<'src> Typechecker<'src> {
    pub fn new(ast: &'src Ast<'src>, source: &'src SourceFile<'src>) -> Self {
        Self { ast, source }
    }

    pub fn typecheck(&mut self) {
        todo!();
    }
}

impl ErrorProducer for Typechecker<'_> {
    type ErrorKind = ErrorKind;

    fn name(&self) -> &str {
        "typechecker"
    }

    fn source(&self) -> &SourceFile {
        self.source
    }

    fn current_span(&mut self) -> Range<usize> {
        todo!()
    }
}
