use ast::Ident;
use lexer::token::{Token, TokenKind};
use miette::{LabeledSpan, Result};

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_ident(&mut self) -> Result<Ident<'src>> {
        match self.eat()? {
            Token { kind: TokenKind::Ident, span } => Ok(Ident(&self.ses.source[span.clone()])),
            Token { kind, span, .. } => Err(miette::miette!(
                labels = vec![LabeledSpan::at(span.clone(), format!("here"))],
                "expected ident, found {}",
                kind
            )
            .with_source_code(self.ses.source.to_string())),
        }
    }

    pub fn can_parse_ident(&mut self) -> bool {
        matches!(self.lexer.peek(), Some(Ok(Token { kind: TokenKind::Ident, .. })))
    }
}
