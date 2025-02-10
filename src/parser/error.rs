use ariadne::{Report, ReportKind};

use crate::{
    lexer::{error::LexerError, token::TokenKind},
    source::{Span, Spanned},
};

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ParserError {
    #[error("lexer error")]
    LexerError(#[from] LexerError),

    #[error("expected block")]
    ExpectedBlock,

    #[error("expected expression")]
    ExpectedExpression,

    #[error("expected item")]
    ExpectedItem,

    #[error("expected {expected}, found {found}")]
    ExpectedToken { expected: TokenKind, found: TokenKind },

    #[error("expected EOF")]
    ExpectedEof,

    #[error("expected identifier")]
    ExpectedIdent,

    #[error("expected type")]
    ExpectedType,

    #[error("unexpected EOF")]
    UnexpectedEof,
}

impl From<Spanned<ParserError>> for Report<'_, Span> {
    fn from(error: Spanned<ParserError>) -> Self {
        Report::build(ReportKind::Error, error.span).with_message(error.value.to_string()).finish()
    }
}
