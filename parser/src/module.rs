use ast::Module;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_module(&mut self) -> Result<Module<'src>> {
        let mut items = Vec::new();

        while let Some(item) = self.parse_item()? {
            items.push(item);
        }

        self.expect_eof()?;

        Ok(Module { items })
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Fn, Ident, Item, ItemKind, Module};

    use crate::{check_parser, check_parser_error};

    #[test]
    fn module_main_fn() {
        check_parser! {
            source: r#"fn main() {}"#,
            fn: parse_module,
            expected: Module {
                items: vec![
                    Item {
                        kind: ItemKind::Fn(Fn {
                            name: Ident { span: 3..7, name: "main" },
                            params: (),
                            body: Block { statements: vec![], span: 10..12 },
                        }),
                        span: 0..12
                    }
                ],
            }
        };
    }

    #[test]
    fn module_empty() {
        check_parser! {
            source: r#""#,
            fn: parse_module,
            expected: Module {
                items: vec![]
            }
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
                            name: Ident { span: 3..4, name: "a" },
                            params: (),
                            body: Block { statements: vec![], span: 7..9 },
                        }),
                        span: 0..9
                    },
                    Item {
                        kind: ItemKind::Fn(Fn {
                            name: Ident { span: 13..14, name: "b" },
                            params: (),
                            body: Block { statements: vec![], span: 17..19 },
                        }),
                        span: 10..19
                    }
                ],
            }
        };
    }

    #[test]
    fn module_invalid_first_item() {
        check_parser_error! {
            source: r#"1.0; fn a() {}"#,
            fn: parse_module,
            expected: "expected item"
        };
    }

    #[test]
    fn module_invalid_last_item() {
        check_parser_error! {
            source: r#"fn a() {} 1.0"#,
            fn: parse_module,
            expected: "expected item"
        };
    }

    #[test]
    fn module_invalid_root() {
        check_parser_error! {
            source: r#"1.0"#,
            fn: parse_module,
            expected: "expected item"
        };
    }
}
