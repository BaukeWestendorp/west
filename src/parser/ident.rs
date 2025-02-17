use crate::ast::Ident;
use crate::lexer::token::{Token, TokenKind};
use crate::source::Spanned;

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn parse_ident(&mut self) -> Result<Ident<'src>, Spanned<ParserError>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Ident, .. })) => {
                let span = self.eat()?.span;
                Ok(Ident { name: &self.source.as_str()[span.to_range()], span })
            }

            _ => Err(Spanned::new(ParserError::ExpectedIdent, self.current_span())),
        }
    }

    #[tracing::instrument(level = "trace", skip(self))]
    pub fn parse_type(&mut self) -> Result<Ident<'src>, Spanned<ParserError>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Ident, .. })) => {
                let span = self.eat()?.span;
                Ok(Ident { name: &self.source.as_str()[span.to_range()], span })
            }

            _ => Err(Spanned::new(ParserError::ExpectedType, self.current_span())),
        }
    }
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use crate::{ast::Ident, check_parser, parser::error::ParserError, source::Spanned, span};

    #[test]
    fn ident() {
        check_parser! {
            source: "a",
            fn: parse_ident,
            expected: Ok(Ident { name: "a", span: span!(0, 1) }),
            expected_errors: vec![]
        };
    }

    #[test]
    fn ident_invalid() {
        check_parser! {
            source: "1",
            fn: parse_ident,
            expected: Err(Spanned::new(ParserError::ExpectedIdent, span!(0, 1))),
            expected_errors: vec![]
        };
    }
}
