use ast::Mod;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_mod(&mut self) -> Result<Mod<'src>> {
        let mut items = Vec::new();

        while let Some(item) = self.parse_item()? {
            items.push(item);
        }

        self.expect_eof()?;

        Ok(Mod { items })
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Fn, Ident, Item, Mod};

    use crate::{check_parser, check_parser_error};

    #[test]
    fn module_main_fn() {
        check_parser! {
            source: r#"fn main() {}"#,
            fn: parse_mod,
            expected: Mod {
                items: vec![Item::Fn(Fn {
                    name: Ident("main"),
                    params: (),
                    body: Block { statements: vec![] },
                })]
            }
        };
    }

    #[test]
    fn module_empty() {
        check_parser! {
            source: r#""#,
            fn: parse_mod,
            expected: Mod {
                items: vec![]
            }
        };
    }

    #[test]
    fn module_multiple_items() {
        check_parser! {
            source: r#"
                fn a() {}
                fn b() {}
            "#,
            fn: parse_mod,
            expected: Mod {
                items: vec![
                    Item::Fn(Fn {
                        name: Ident("a"),
                        params: (),
                        body: Block { statements: vec![] }
                    }),
                    Item::Fn(Fn {
                        name: Ident("b"),
                        params: (),
                        body: Block { statements: vec![] }
                    }),
                ],
            }
        };
    }

    #[test]
    fn module_invalid_first_item() {
        check_parser_error! {
            source: r#"1.0; fn a() {}"#,
            fn: parse_mod,
            expected: "expected item"
        };
    }

    #[test]
    fn module_invalid_last_item() {
        check_parser_error! {
            source: r#"fn a() {} 1.0"#,
            fn: parse_mod,
            expected: "expected item"
        };
    }

    #[test]
    fn module_invalid_root() {
        check_parser_error! {
            source: r#"1.0"#,
            fn: parse_mod,
            expected: "expected item"
        };
    }
}
