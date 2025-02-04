use lexer::token::TokenKind;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, fout::Error<ErrorKind>>;

#[derive(Error, Debug, PartialEq)]
pub enum ErrorKind {
    #[error("expected block")]
    ExpectedBlock,

    #[error("expected expression")]
    ExpectedExpression,

    #[error("expected item")]
    ExpectedItem,

    #[error("expected {expected}, found {found}")]
    ExpectedToken { expected: TokenKind, found: String },

    #[error("expected EOF")]
    ExpectedEof,

    #[error("unexpected EOF")]
    UnexpectedEof,
}
