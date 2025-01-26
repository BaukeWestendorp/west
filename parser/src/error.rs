use lexer::token::TokenKind;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum ErrorKind {
    #[error("expected block")]
    #[diagnostic(code(west::parser::expected_block))]
    ExpectedBlock,

    #[error("expected expression")]
    #[diagnostic(code(west::parser::failed_to_parse_expression))]
    ExpectedExpression,

    #[error("expected item")]
    #[diagnostic(code(west::parser::expected_item))]
    ExpectedItem,

    #[error("expected {expected}, found {found}")]
    ExpectedToken { expected: TokenKind, found: String },

    #[error("expected EOF")]
    #[diagnostic(code(west::parser::expected_eof))]
    ExpectedEof,

    #[error("unexpected EOF")]
    #[diagnostic(code(west::parser::unexpected_eof))]
    UnexpectedEof,
}
