use ast::Mod;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_mod(&mut self, name: String) -> Result<Mod<'src>> {
        let mut items = Vec::new();

        while let Some(item) = self.parse_item()? {
            items.push(item);
        }

        self.expect_eof()?;

        Ok(Mod { name, items })
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Fn, Ident, Item, Mod};

    macro_rules! check_mod {
        {
            source: $src:expr,
            expected: $expected:expr
        } => {{
            let source = west_error::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::Parser::new(&source);

            let actual = parser.parse_mod("test_mod".to_string()).unwrap();

            assert_eq!(actual, $expected);
        }};
    }

    #[macro_export]
    macro_rules! check_mod_error {
        {
            source: $src:expr,
            expected: $expected:expr
        } => {{
            let source = west_error::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::Parser::new(&source);

            let actual = parser.parse_mod("test_mod".to_string()).unwrap_err();

            assert_eq!(actual.to_string(), $expected.to_string());
        }};
    }

    #[test]
    fn module_main_fn() {
        check_mod! {
            source: r#"fn main() {}"#,
            expected: Mod {
                name: "test_mod".to_string(),
                items: vec![Item::Fn(Fn {
                    name: Ident("main"),
                    params: (),
                    return_type: None,
                    body: Block { statements: vec![] },
                })]
            }
        };
    }

    #[test]
    fn module_empty() {
        check_mod! {
            source: r#""#,
            expected: Mod {
                name: "test_mod".to_string(),
                items: vec![]
            }
        };
    }

    #[test]
    fn module_multiple_items() {
        check_mod! {
            source: r#"
                fn a() {}
                fn b() {}
            "#,
            expected: Mod {
                name: "test_mod".to_string(),
                items: vec![
                    Item::Fn(Fn {
                        name: Ident("a"),
                        params: (),
                        return_type: None,
                        body: Block { statements: vec![] }
                    }),
                    Item::Fn(Fn {
                        name: Ident("b"),
                        params: (),
                        return_type: None,
                        body: Block { statements: vec![] }
                    }),
                ],
            }
        };
    }

    #[test]
    fn module_invalid_first_item() {
        check_mod_error! {
            source: r#"1.0; fn a() {}"#,
            expected: "expected item"
        };
    }

    #[test]
    fn module_invalid_last_item() {
        check_mod_error! {
            source: r#"fn a() {} 1.0"#,
            expected: "expected item"
        };
    }

    #[test]
    fn module_invalid_root() {
        check_mod_error! {
            source: r#"1.0"#,
            expected: "expected item"
        };
    }
}
