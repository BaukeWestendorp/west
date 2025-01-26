use ast::Item;
use lexer::token::{Keyword, TokenKind};
use miette::{Context, Result};

use crate::Parser;
use crate::error::ErrorKind;

impl<'src> Parser<'src> {
    pub fn parse_item(&mut self) -> Result<Option<Item<'src>>> {
        if self.eat_keyword(Keyword::Fn) {
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
        let name = self.parse_ident()?.wrap_err("expected function name")?;
        self.eat_expected(TokenKind::ParenOpen)?;
        let params = ();
        self.eat_expected(TokenKind::ParenClose)?;
        let body = self.parse_block()?;
        Ok(ast::Fn { name, params, body })
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Ident, Item};

    use crate::{check_parser, check_parser_error};

    #[test]
    fn fn_item() {
        check_parser! {
            source: r#"fn a() {}"#,
            fn: parse_item,
            expected: Some(Item::Fn(ast::Fn { name: Ident("a"), params: (), body: Block { statements: vec![] } }))
        };
    }

    #[test]
    fn fn_long_name() {
        check_parser! {
            source: r#"fn a_very_long_name_here() {}"#,
            fn: parse_item,
            expected: Some(Item::Fn(ast::Fn { name: Ident("a_very_long_name_here"), params: (), body: Block { statements: vec![] } }))
        };
    }

    #[test]
    fn fn_item_unexpected_eof() {
        check_parser_error! {
            source: r#"fn a() {"#,
            fn: parse_item,
            expected: "unexpected EOF"
        };
    }

    #[test]
    fn fn_no_name() {
        check_parser_error! {
            source: r#"fn () {}"#,
            fn: parse_item,
            expected: "expected function name"
        };
    }
}
