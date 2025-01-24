use lexer::token::TokenKind;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

pub struct ParserError<'src> {
    pub kind: ErrorKind<'src>,
    pub source: NamedSource<String>,
    pub span: SourceSpan,
}

#[derive(Error, Diagnostic, Debug)]
pub enum ErrorKind<'src> {
    #[error("expected block")]
    #[diagnostic(code(west::expected_block))]
    ExpectedBlock,

    #[error("expected expression")]
    #[diagnostic(code(west::failed_to_parse_expression))]
    ExpectedExpression,

    #[error("expected ident")]
    #[diagnostic(code(west::expected_ident))]
    ExpectedIdent,

    #[error("expected item")]
    #[diagnostic(code(west::expected_item))]
    ExpectedItem,

    #[error("expected literal")]
    #[diagnostic(code(west::expected_literal))]
    ExpectedLiteral,

    #[error("expected statement")]
    #[diagnostic(code(west::expected_statement))]
    ExpectedStatement,

    #[error("expected {expected}, found {found}")]
    ExpectedToken { expected: TokenKind, found: &'src str },

    #[error("expected EOF")]
    #[diagnostic(code(west::expected_eof))]
    ExpectedEof,

    #[error("unexpected EOF")]
    #[diagnostic(code(west::unexpected_eof))]
    UnexpectedEof,
}
