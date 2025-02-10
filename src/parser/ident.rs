use crate::ast::Ident;
use crate::lexer::token::{Token, TokenKind};

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    pub fn parse_ident(&mut self) -> Option<Ident<'src>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Ident, .. })) => {
                let span = self.eat()?.span;
                Some(Ident { name: &self.source.as_str()[span.to_range()], span })
            }
            _ => {
                self.error_here(ParserError::ExpectedIdent);
                // Recover.
                self.eat();
                None
            }
        }
    }

    pub fn parse_type(&mut self) -> Option<Ident<'src>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Ident, .. })) => {
                let span = self.eat()?.span;
                Some(Ident { name: &self.source.as_str()[span.to_range()], span })
            }
            _ => {
                self.error_here(ParserError::ExpectedType);
                // Recover.
                self.eat();
                None
            }
        }
    }
}

// #[cfg(test)]
// mod tests {
//     #[test]
//     fn ident() {
//         check_parser! {
//             source: "a",
//             fn: parse_ident,
//             expected: Some(Ident { name: "a", span: span!(0, 1) })
//         };
//     }

//     #[test]
//     fn ident_invalid() {
//         check_parser! {
//             source: "1",
//             fn: parse_ident,
//             expected: None
//         };
//     }
// }
