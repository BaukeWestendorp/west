use ast::Item;
use lexer::token::{Keyword, TokenKind};
use miette::{LabeledSpan, Result};

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_item(&mut self) -> Result<Option<Item<'src>>> {
        if self.eat_keyword(Keyword::Fn) {
            Ok(Some(Item::Fn(self.parse_fn()?)))
        } else {
            match self.eat_or_eof()? {
                Some(token) => Err(miette::miette!(
                    labels = vec![LabeledSpan::at(token.span, format!("here"))],
                    "expected item, found {}: '{}'",
                    token.kind,
                    &self.ses.source[token.clone().span]
                )
                .with_source_code(self.ses.source.to_string())),
                _ => Ok(None),
            }
        }
    }

    pub fn parse_fn(&mut self) -> Result<ast::Fn<'src>> {
        // FIXME: Make error better.
        let name = self.parse_ident()?;
        self.eat_expected(TokenKind::OpenParen)?;
        let params = ();
        self.eat_expected(TokenKind::CloseParen)?;
        let body = self.parse_block()?;
        Ok(ast::Fn { name, params, body })
    }
}
