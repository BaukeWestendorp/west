use ast::Ident;
use lexer::token::{Token, TokenKind};
use miette::Result;

use crate::{Parser, error::ErrorKind};

impl<'src> Parser<'src> {
    pub fn parse_ident(&mut self) -> Result<Ident<'src>> {
        match self.eat()? {
            Token { kind: TokenKind::Ident, span } => Ok(Ident(&self.ses.source[span])),
            Token { span, .. } => Err(self.err_here(ErrorKind::ExpectedIdent, Some(span.into()))),
        }
    }

    pub fn can_parse_ident(&mut self) -> bool {
        matches!(self.lexer.peek(), Some(Ok(Token { kind: TokenKind::Ident, .. })))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::new_parser;
    use ast::Ident;

    #[test]
    fn ident() {
        let actual = new_parser("a").parse_ident().unwrap();
        let expected = Ident("a");
        assert_eq!(actual, expected);
    }

    #[test]
    fn ident_invalid() {
        let actual = new_parser("1").parse_ident().unwrap_err();
        let expected = miette::miette!("expected ident");
        assert_eq!(actual.to_string(), expected.to_string());
    }
}
