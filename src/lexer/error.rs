use ariadne::{Report, ReportKind};

use crate::{
    parser::error::ParserError,
    source::{Span, Spanned},
};

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum LexerError {
    #[error("invalid character: {0}")]
    InvalidCharacter(char),

    #[error("unknown keyword: {0}")]
    UnknownKeyword(String),
}

impl From<Spanned<LexerError>> for Report<'_, Span> {
    fn from(error: Spanned<LexerError>) -> Self {
        Report::build(ReportKind::Error, error.span).with_message(error.value.to_string()).finish()
    }
}

impl From<Spanned<LexerError>> for Spanned<ParserError> {
    fn from(error: Spanned<LexerError>) -> Self {
        Spanned::new(ParserError::LexerError(error.value), error.span)
    }
}
