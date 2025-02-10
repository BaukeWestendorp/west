use std::str::Chars;

use crate::{
    source::{SourceFile, Span},
    span,
};

#[derive(Clone)]
pub struct Cursor<'src> {
    source: &'src SourceFile<'src>,

    span_start: usize,
    rest: Chars<'src>,
}

pub(crate) const EOF_CHAR: char = '\0';

impl<'src> Cursor<'src> {
    pub fn new(source: &'src SourceFile<'src>) -> Cursor<'src> {
        Cursor { rest: source.as_str().chars(), source, span_start: 0 }
    }

    /// Peeks the next symbol from the input stream without consuming it.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method.
    pub fn first(&self) -> char {
        // `.next()` optimizes better than `.nth(0)`
        self.rest.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Checks if there is nothing mroe to consume.
    pub fn is_eof(&self) -> bool {
        self.rest.as_str().is_empty()
    }

    /// Resets the position within the current token.
    pub fn reset_span_start(&mut self) {
        self.span_start = self.source.as_str().len() - self.rest.as_str().len();
    }

    /// Returns the current token as a string.
    pub fn current_token_str(&mut self) -> &'src str {
        &self.source.as_str()[self.span().to_range()]
    }

    /// Moves to the next character.
    pub fn consume(&mut self) -> Option<char> {
        self.rest.next()
    }

    /// Consumes the next character as long as the predicate is true or the end of file is reached.
    pub fn consume_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.consume();
        }
    }

    /// Current span of the token that's being processed.
    pub fn span(&mut self) -> Span {
        let start = self.span_start;
        let end = self.source.as_str().len() - self.rest.as_str().len();
        span!(start, end)
    }
}
