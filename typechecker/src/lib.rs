use ast::Ast;
use lexer::source::SourceFile;

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
