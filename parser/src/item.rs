use ast::Item;
use lexer::token::{Keyword, TokenKind};
use miette::Result;

use crate::{Parser, error::ErrorKind};

impl<'src> Parser<'src> {
    pub fn parse_item(&mut self) -> Result<Option<Item<'src>>> {
        if self.eat_keyword(&Keyword::Fn) {
            Ok(Some(Item::Fn(self.parse_fn()?)))
        } else {
            match self.eat_or_eof()? {
                Some(token) => Err(self.err_here(ErrorKind::ExpectedItem, Some(token.span.into()))),
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

#[cfg(test)]
mod tests {
    use crate::tests::new_parser;
    use ast::{Block, Fn, Ident, Item};

    #[test]
    fn fn_item() {
        let actual = new_parser(r#"fn a() {}"#).parse_item().unwrap().unwrap();
        let expected =
            Item::Fn(Fn { name: Ident("a"), params: (), body: Block { statements: vec![] } });
        assert_eq!(actual, expected);
    }

    #[test]
    fn fn_item_unexpected_eof() {
        let actual = new_parser(r#"fn a() {"#).parse_item().unwrap_err();
        let expected = miette::miette!("unexpected EOF");
        assert_eq!(actual.to_string(), expected.to_string());
    }

    #[test]
    fn fn_long_name() {
        let actual = new_parser(r#"fn a_very_long_name_here() {}"#).parse_item().unwrap().unwrap();
        let expected = Item::Fn(Fn {
            name: Ident("a_very_long_name_here"),
            params: (),
            body: Block { statements: vec![] },
        });
        assert_eq!(actual, expected);
    }
}
