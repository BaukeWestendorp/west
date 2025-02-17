use crate::{parser::error::ParserError, source::Spanned};

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum LexerError {
    #[error("invalid character: {0}")]
    InvalidCharacter(char),

    #[error("unknown keyword: {0}")]
    UnknownKeyword(String),
}

impl From<Spanned<LexerError>> for Spanned<ParserError> {
    fn from(error: Spanned<LexerError>) -> Self {
        Spanned::new(ParserError::LexerError(error.value), error.span)
    }
}
