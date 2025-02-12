use crate::ast::Ident;
use crate::lexer::token::{Token, TokenKind};
use crate::source::Spanned;

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    pub fn parse_ident(&mut self) -> Result<Ident<'src>, Spanned<ParserError>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Ident, .. })) => {
                let span = self.eat()?.span;
                Ok(Ident { name: &self.source.as_str()[span.to_range()], span })
            }

            _ => Err(Spanned::new(ParserError::ExpectedIdent, self.current_span())),
        }
    }

    pub fn parse_type(&mut self) -> Result<Ident<'src>, Spanned<ParserError>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Ident, .. })) => {
                let span = self.eat()?.span;
                Ok(Ident { name: &self.source.as_str()[span.to_range()], span })
            }

            _ => Err(Spanned::new(ParserError::ExpectedType, self.current_span())),
        }
    }

    // pub fn eat_ident(&mut self, expected: TokenKind) -> Result<Ident<'src>, Spanned<ParserError>> {
    //     match self.eat()? {
    //         Token { kind: TokenKind::Ident, span } => {
    //             Ok(Ident { name: &self.source.as_str()[span.to_range()], span })
    //         }
    //         Token { kind, span } => {
    //             self.error(ParserError::ExpectedToken { expected, found: kind }, span);
    //             None
    //         }
    //     }
    // }
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
