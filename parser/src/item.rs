use ast::Item;
use lexer::token::{Keyword, TokenKind};
use miette::{Context, Result};
use west_error::ErrorProducer;

use crate::Parser;
use crate::error::ErrorKind;

impl<'src> Parser<'src> {
    pub fn parse_item(&mut self) -> Result<Option<Item<'src>>> {
        if let Some(fn_item) = self.parse_item_fn()? {
            Ok(Some(Item::Fn(fn_item)))
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

        let mut return_type = None;
        if self.try_eat(TokenKind::Colon).is_some() {
            return_type = Some(self.parse_type()?);
        }

        let body = self.parse_block()?;
        Ok(Some(ast::Fn { name, params, return_type, body }))
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Ident, Item, ParsedType};

    use crate::{check_parser, check_parser_error};

    #[test]
    fn fn_item() {
        check_parser! {
            source: r#"fn a() {}"#,
            fn: parse_item,
            expected: Some(Item::Fn(ast::Fn { name: Ident("a"), params: (), return_type: None, body: Block { statements: vec![] } }))
        };
    }

    #[test]
    fn fn_long_name() {
        check_parser! {
            source: r#"fn a_very_long_name_here() {}"#,
            fn: parse_item,
            expected: Some(Item::Fn(ast::Fn { name: Ident("a_very_long_name_here"), params: (), return_type: None, body: Block { statements: vec![] } }))
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

    #[test]
    fn fn_with_type() {
        check_parser! {
            source: r#"fn test(): int {}"#,
            fn: parse_item,
            expected: Some(Item::Fn(ast::Fn { name: Ident("test"), params: (), return_type: Some(ParsedType { ident: Ident("int"), id: 0 }), body: Block { statements: vec![] } }))
        };
    }
}
