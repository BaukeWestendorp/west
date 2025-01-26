use ast::File;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_file(&mut self) -> Result<File<'src>> {
        let mut items = Vec::new();

        while let Some(item) = self.parse_item()? {
            items.push(item);
        }

        self.expect_eof()?;

        Ok(File { items })
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, File, Fn, Ident, Item};

    use crate::{check_parser, check_parser_error};

    #[test]
    fn file_main_fn() {
        check_parser! {
            source: r#"fn main() {}"#,
            fn: parse_file,
            expected: File {
                items: vec![Item::Fn(Fn {
                    name: Ident("main"),
                    params: (),
                    body: Block { statements: vec![] },
                })]
            }
        };
    }

    #[test]
    fn empty_file() {
        check_parser! {
            source: r#""#,
            fn: parse_file,
            expected: File {
                items: vec![]
            }
        };
    }

    #[test]
    fn file_multiple_items() {
        check_parser! {
            source: r#"
                fn a() {}
                fn b() {}
            "#,
            fn: parse_file,
            expected: File {
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
    fn file_invalid_first_item() {
        check_parser_error! {
            source: r#"1.0; fn a() {}"#,
            fn: parse_file,
            expected: "expected item"
        };
    }

    #[test]
    fn file_invalid_last_item() {
        check_parser_error! {
            source: r#"fn a() {} 1.0"#,
            fn: parse_file,
            expected: "expected item"
        };
    }

    #[test]
    fn file_invalid_root() {
        check_parser_error! {
            source: r#"1.0"#,
            fn: parse_file,
            expected: "expected item"
        };
    }
}
