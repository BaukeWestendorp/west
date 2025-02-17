use crate::{
    lexer::{error::LexerError, token::TokenKind},
    source::{Span, Spanned},
};

use ariadne::{Label, Report, ReportKind};

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum ParserError {
    #[error("{0}")]
    LexerError(#[from] LexerError),

    #[error("expected block")]
    ExpectedBlock,

    #[error("expected expression")]
    ExpectedExpression,

    #[error("expected '{expected}', found '{found}'")]
    ExpectedToken { expected: TokenKind, found: TokenKind },

    #[error("expected identifier")]
    ExpectedIdent,

    #[error("expected type")]
    ExpectedType,

    #[error("expected EOF")]
    ExpectedEof,

    #[error("expected item")]
    ExpectedItem,

    #[error("unexpected EOF")]
    UnexpectedEof,
}

impl ParserError {
    pub fn code(&self) -> &str {
        match self {
            Self::LexerError(lexer_error) => match lexer_error {
                LexerError::InvalidCharacter(_) => "invalid-character",
                LexerError::UnknownKeyword(_) => "unknown-keyword",
            },
            Self::ExpectedBlock => "expected-block",
            Self::ExpectedExpression => "expected-expression",
            Self::ExpectedToken { .. } => "expected-token",
            Self::ExpectedIdent => "expected-ident",
            Self::ExpectedType => "expected-type",
            Self::ExpectedEof => "expected-eof",
            Self::ExpectedItem => "expected-item",
            Self::UnexpectedEof => "unexpected-eof",
        }
    }
}

impl From<Spanned<ParserError>> for Report<'_, Span> {
    fn from(error: Spanned<ParserError>) -> Self {
        let report = Report::build(ReportKind::Error, error.span)
            .with_code(error.value.code())
            .with_message(error.value.to_string())
            .with_label(Label::new(error.span).with_message(error.value.to_string()));

        report.finish()
    }
}
