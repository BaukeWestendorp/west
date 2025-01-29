use ast::{Item, ItemKind};
use lexer::token::{Keyword, TokenKind};
use miette::{Context, Result};
use west_error::ErrorProducer;

use crate::Parser;
use crate::error::ErrorKind;

impl<'src> Parser<'src> {
    pub fn parse_item(&mut self) -> Result<Option<Item<'src>>> {
        self.start_span();
        if let Some(fn_item) = self.parse_item_fn()? {
            Ok(Some(Item { kind: ItemKind::Fn(fn_item), span: self.end_span() }))
        } else {
            match self.eat_or_eof()? {
                Some(_) => Err(self.err_here(ErrorKind::ExpectedItem)),
                _ => Ok(None),
            }
        }
    }

    pub fn parse_item_fn(&mut self) -> Result<Option<ast::Fn<'src>>> {
        if !self.try_eat_keyword(Keyword::Fn) {
            return Ok(None);
        }

        let name = self.parse_ident()?.wrap_err("expected function name")?;

        self.eat_expected(TokenKind::ParenOpen)?;
        let params = ();
        self.eat_expected(TokenKind::ParenClose)?;

        let body = self.parse_block()?;

        Ok(Some(ast::Fn { name, params, body }))
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Ident, Item, ItemKind};

    use crate::{check_parser, check_parser_error};

    #[test]
    fn fn_item() {
        check_parser! {
            source: r#"fn a() {}"#,
            fn: parse_item,
            expected: Some(Item {
                kind: ItemKind::Fn(ast::Fn {
                    name: Ident { name: "a", span: 3..4 },
                    params: (),
                    body: Block { statements: vec![], span: 7..9 } }
                ),
                span: 0..9
            })
        };
    }

    #[test]
    fn fn_long_name() {
        check_parser! {
            source: r#"fn a_very_long_name_here() {}"#,
            fn: parse_item,
            expected: Some(Item {
                kind: ItemKind::Fn(ast::Fn {
                    name: Ident { name: "a_very_long_name_here", span: 3..24 },
                    params: (),
                    body: Block { statements: vec![], span: 27..29 } }
                ),
                span: 0..29
            })
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
