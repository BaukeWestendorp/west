use crate::{
    ast::Module,
    lexer::token::{Keyword, TokenKind},
    source::Spanned,
};

use super::{Parser, error::ParserError};

impl<'src> Parser<'src> {
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn parse_module(&mut self) -> Module<'src> {
        let mut items = Vec::new();

        loop {
            if self.at_eof() {
                break;
            }

            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(error) => {
                    self.errors.push(error);
                    if let Err(err) = self.skip_to_next_item() {
                        self.errors.push(err);
                    };
                }
            }
        }

        if !self.at_eof() {
            self.error_here(ParserError::ExpectedItem);
        }

        Module { items }
    }

    fn skip_to_next_item(&mut self) -> Result<(), Spanned<ParserError>> {
        let mut current_scope_depth = self.scope_depth;
        while current_scope_depth > 0 {
            if self.eat()?.kind == TokenKind::BraceClose {
                current_scope_depth -= 1;
            }
        }

        while let Some(token) = self.lexer.peek() {
            match token {
                Ok(token) => match token.kind {
                    TokenKind::Keyword(Keyword::Fn) => {
                        break;
                    }
                    _ => {
                        self.eat()?;
                    }
                },
                Err(err) => return Err(err.clone().into()),
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use crate::ast::{Block, Fn, Ident, Item, ItemKind, Module};

    use crate::parser::error::ParserError;
    use crate::source::Spanned;
    use crate::{check_parser, span};

    #[test]
    fn module_main_fn() {
        check_parser! {
            source: r#"fn main() {}"#,
            fn: parse_module,
            expected: Module {
                items: vec![
                    Item {
                        kind: ItemKind::Fn(Fn {
                            name: Ident { span: span!(3, 7), name: "main" },
                            params: vec![],
                            return_type: None,
                            body: Block { stmts: vec![], span: span!(10, 12) },
                        }),
                        span: span!(0, 12)
                    }
                ],
            },
            expected_errors: vec![]
        };
    }

    #[test]
    fn module_empty() {
        check_parser! {
            source: r#""#,
            fn: parse_module,
            expected: Module {
                items: vec![]
            },
            expected_errors: vec![]
        };
    }

    #[test]
    fn module_multiple_items() {
        check_parser! {
            source: r#"fn a() {} fn b() {}"#,
            fn: parse_module,
            expected: Module {
                items: vec![
                    Item {
                        kind: ItemKind::Fn(Fn {
                            name: Ident { span: span!(3, 4), name: "a" },
                            params: vec![],
                            return_type: None,
                            body: Block { stmts: vec![], span: span!(7, 9) }
                        }),
                        span: span!(0, 9)
                    },
                    Item {
                        kind: ItemKind::Fn(Fn {
                            name: Ident { span: span!(13, 14), name: "b" },
                            params: vec![],
                            return_type: None,
                            body: Block { stmts: vec![], span: span!(17, 19) },
                        }),
                        span: span!(10, 19)
                    }
                ],
            },
            expected_errors: vec![]
        };
    }

    #[test]
    fn module_invalid_first_item() {
        check_parser! {
            source: r#"1.0; fn a() {}"#,
            fn: parse_module,
            expected: Module { items: vec![
                Item {
                    kind: ItemKind::Fn(Fn {
                        name: Ident { span: span!(8, 9), name: "a" },
                        params: vec![],
                        return_type: None,
                        body: Block { stmts: vec![], span: span!(12, 14) }
                    }),
                    span: span!(5, 14)
                }
            ] },
            expected_errors: vec![
                Spanned::new(ParserError::ExpectedItem, span!(0, 3))
            ]
        };
    }

    #[test]
    fn module_invalid_last_item() {
        check_parser! {
            source: r#"fn a {} 1.0"#,
            fn: parse_module,
            expected: Module { items: vec![
                Item {
                    kind: ItemKind::Fn(Fn {
                        name: Ident { span: span!(3, 4), name: "a" },
                        params: vec![],
                        return_type: None,
                        body: Block { stmts: vec![], span: span!(5, 7) }
                    }),
                    span: span!(0, 7)
                }
            ] },
            expected_errors: vec![
                Spanned::new(ParserError::ExpectedItem, span!(8, 11)),
            ]
        };
    }

    #[test]
    fn module_invalid_last_item_parens() {
        check_parser! {
            source: r#"fn a() {} 1.0"#,
            fn: parse_module,
            expected: Module { items: vec![
                Item {
                    kind: ItemKind::Fn(Fn {
                        name: Ident { span: span!(3, 4), name: "a" },
                        params: vec![],
                        return_type: None,
                        body: Block { stmts: vec![], span: span!(7, 9) }
                    }),
                    span: span!(0, 9)
                }
            ] },
            expected_errors: vec![Spanned::new(ParserError::ExpectedItem, span!(10, 13))]
        };
    }

    #[test]
    fn module_invalid_root() {
        check_parser! {
            source: r#"1.0"#,
            fn: parse_module,
            expected: Module { items: vec![] },
            expected_errors: vec![Spanned::new(ParserError::ExpectedItem, span!(0, 3))]
        };
    }

    #[test]
    fn module_multiple_item_errors() {
        check_parser! {
            source: r#"1.0; fn a() {} 1.0; fn b() {}"#,
            fn: parse_module,
            expected: Module { items: vec![
                Item {
                    kind: ItemKind::Fn(Fn {
                        name: Ident { span: span!(8, 9), name: "a" },
                        params: vec![],
                        return_type: None,
                        body: Block { stmts: vec![], span: span!(12, 14) }
                    }),
                    span: span!(5, 14)
                },
                Item {
                    kind: ItemKind::Fn(Fn {
                        name: Ident { span: span!(23, 24), name: "b" },
                        params: vec![],
                        return_type: None,
                        body: Block { stmts: vec![], span: span!(27, 29) }
                    }),
                    span: span!(20, 29)
                }
            ] },
            expected_errors: vec![
                Spanned::new(ParserError::ExpectedItem, span!(0, 3)),
                Spanned::new(ParserError::ExpectedItem, span!(15, 18)),
            ]
        };
    }
}
