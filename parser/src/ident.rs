use ast::Ident;
use lexer::token::{Token, TokenKind};

use crate::Parser;
use crate::error::Result;

impl<'src> Parser<'src> {
    pub fn parse_ident(&mut self) -> Result<Option<Ident<'src>>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Ident, .. })) => {
                let span = self.eat()?.span;
                Ok(Some(Ident { name: &self.source.as_str()[span.to_range()], span }))
            }
            _ => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::Ident;
    use fout::span;

    use crate::check_parser;

    #[test]
    fn ident() {
        check_parser! {
            source: "a",
            fn: parse_ident,
            expected: Some(Ident { name: "a", span: span!(0, 1) })
        };
    }

    #[test]
    fn ident_invalid() {
        check_parser! {
            source: "1",
            fn: parse_ident,
            expected: None
        };
    }
}
