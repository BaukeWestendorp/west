use ast::Ident;
use lexer::token::{Token, TokenKind};
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_ident(&mut self) -> Result<Option<Ident<'src>>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Ident, .. })) => {
                let span = self.eat()?.span;
                Ok(Some(Ident(&self.ses.source[span])))
            }
            _ => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::Ident;

    use crate::tests::new_parser;

    #[test]
    fn ident() {
        let actual = new_parser("a").parse_ident().unwrap();
        let expected = Some(Ident("a"));
        assert_eq!(actual, expected);
    }

    #[test]
    fn ident_invalid() {
        let actual = new_parser("1").parse_ident().unwrap();
        let expected = None;
        assert_eq!(actual, expected);
    }
}
