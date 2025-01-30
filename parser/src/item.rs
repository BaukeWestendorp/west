use ast::{FnParam, Item, ItemKind};
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

        let mut params = vec![];
        if self.try_eat(TokenKind::ParenOpen).is_some() {
            if let Some(p) = self.parse_item_fn_params()? {
                params = p;
            }

            self.eat_expected(TokenKind::ParenClose)?;
        };

        let mut return_type = None;
        if self.try_eat(TokenKind::Colon).is_some() {
            return_type = self.parse_ident()?;
        }

        let body = self.parse_block()?;

        Ok(Some(ast::Fn { name, params, return_type, body }))
    }

    fn parse_item_fn_params(&mut self) -> Result<Option<Vec<FnParam<'src>>>> {
        let mut params = vec![];

        loop {
            let Some(name) = self.parse_ident()? else { return Ok(None) };
            self.eat_expected(TokenKind::Colon)?;
            let ty = self.parse_ident()?.wrap_err("expected parameter type")?;

            params.push(FnParam { name, ty });

            if self.try_eat(TokenKind::Comma).is_none() {
                break;
            }
        }

        Ok(Some(params))
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, FnParam, Ident, Item, ItemKind};

    use crate::{check_parser, check_parser_error};

    #[test]
    fn fn_item() {
        check_parser! {
            source: r#"fn a() {}"#,
            fn: parse_item,
            expected: Some(Item {
                kind: ItemKind::Fn(ast::Fn {
                    name: Ident { name: "a", span: 3..4 },
                    params: vec![],
                    return_type: None,
                    body: Block { statements: vec![], span: 7..9 } }
                ),
                span: 0..9
            })
        };
    }

    #[test]
    fn fn_item_without_parens() {
        check_parser! {
            source: r#"fn a {}"#,
            fn: parse_item,
            expected: Some(Item {
                kind: ItemKind::Fn(ast::Fn {
                    name: Ident { name: "a", span: 3..4 },
                    params: vec![],
                    return_type: None,
                    body: Block { statements: vec![], span: 5..7 } }
                ),
                span: 0..7
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
                    params: vec![],
                    return_type: None,
                    body: Block { statements: vec![], span: 27..29 } }
                ),
                span: 0..29
            })
        };
    }

    #[test]
    fn fn_return_type() {
        check_parser! {
            source: r#"fn a(): int {}"#,
            fn: parse_item,
            expected: Some(Item {
                kind: ItemKind::Fn(ast::Fn {
                    name: Ident { name: "a", span: 3..4 },
                    params: vec![],
                    return_type: Some(Ident { name: "int", span: 8..11 }),
                    body: Block { statements: vec![], span: 12..14 } }
                ),
                span: 0..14
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

    #[test]
    fn fn_single_argument() {
        check_parser! {
            source: r#"fn a(x: int) {}"#,
            fn: parse_item,
            expected: Some(Item {
                kind: ItemKind::Fn(ast::Fn {
                    name: Ident { name: "a", span: 3..4 },
                    params: vec![FnParam { name: Ident { name: "x", span: 5..6 }, ty: Ident { name: "int", span: 8..11 }}],
                    return_type: None,
                    body: Block { statements: vec![], span: 13..15 } }
                ),
                span: 0..15
            })
        };
    }

    #[test]
    fn fn_multiple_arguments() {
        check_parser! {
            source: r#"fn a(x: int, y: str) {}"#,
            fn: parse_item,
            expected: Some(Item {
                kind: ItemKind::Fn(ast::Fn {
                    name: Ident { name: "a", span: 3..4 },
                    params: vec![
                        FnParam { name: Ident { name: "x", span: 5..6 }, ty: Ident { name: "int", span: 8..11 }},
                        FnParam { name: Ident { name: "y", span: 13..14 }, ty: Ident { name: "str", span: 16..19 }}
                    ],
                    return_type: None,
                    body: Block { statements: vec![], span: 21..23 } }
                ),
                span: 0..23
            })
        };
    }
}
