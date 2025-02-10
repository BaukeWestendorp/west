use crate::ast::Fn;
use crate::ast::{FnParam, Item, ItemKind};
use crate::lexer::token::{Keyword, TokenKind};
use crate::source::Spanned;

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    pub fn parse_item(&mut self) -> Result<Item<'src>, Spanned<ParserError>> {
        self.start_span();
        let fn_item = self.parse_item_fn()?;
        Ok(Item { kind: ItemKind::Fn(fn_item), span: self.end_span() })
    }

    pub fn parse_item_fn(&mut self) -> Result<Fn<'src>, Spanned<ParserError>> {
        if !self.try_eat_keyword(Keyword::Fn) {
            return Err(Spanned::new(ParserError::ExpectedItem, self.span()));
        }

        let name = self.parse_ident()?;

        let mut params = vec![];
        if self.try_eat_expected(TokenKind::ParenOpen) {
            params = self.parse_item_fn_params()?;
            self.eat_expected(TokenKind::ParenClose)?;
        };

        let mut return_type = None;
        if self.try_eat_expected(TokenKind::Colon) {
            return_type = Some(self.parse_ident()?);
        }

        let body = self.parse_block()?;

        Ok(Fn { name, params, return_type, body })
    }

    fn parse_item_fn_params(&mut self) -> Result<Vec<FnParam<'src>>, Spanned<ParserError>> {
        let mut params = vec![];

        loop {
            let Ok(name) = self.parse_ident() else { break };
            self.eat_expected(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            params.push(FnParam { name, ty });

            if !self.try_eat_expected(TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::ast::{Block, FnParam, Ident, Item, ItemKind};
//     use fout::{Error, span};

//     use crate::error::ErrorKind;
//     use crate::{check_parser, check_parser_error};

//     #[test]
//     fn fn_item() {
//         check_parser! {
//             source: r#"fn a() {}"#,
//             fn: parse_item,
//             expected: Some(Item {
//                 kind: ItemKind::Fn(ast::Fn {
//                     name: Ident { name: "a", span: span!(3, 4) },
//                     params: vec![],
//                     return_type: None,
//                     body: Block { statements: vec![], span: span!(7, 9) } }
//                 ),
//                 span: span!(0, 9)
//             })
//         };
//     }

//     #[test]
//     fn fn_item_without_parens() {
//         check_parser! {
//             source: r#"fn a {}"#,
//             fn: parse_item,
//             expected: Some(Item {
//                 kind: ItemKind::Fn(ast::Fn {
//                     name: Ident { name: "a", span: span!(3, 4) },
//                     params: vec![],
//                     return_type: None,
//                     body: Block { statements: vec![], span: span!(5, 7) } }
//                 ),
//                 span: span!(0, 7)
//             })
//         };
//     }

//     #[test]
//     fn fn_long_name() {
//         check_parser! {
//             source: r#"fn a_very_long_name_here() {}"#,
//             fn: parse_item,
//             expected: Some(Item {
//                 kind: ItemKind::Fn(ast::Fn {
//                     name: Ident { name: "a_very_long_name_here", span: span!(3, 24) },
//                     params: vec![],
//                     return_type: None,
//                     body: Block { statements: vec![], span: span!(27, 29) } }
//                 ),
//                 span: span!(0, 29),
//             })
//         };
//     }

//     #[test]
//     fn fn_return_type() {
//         check_parser! {
//             source: r#"fn a(): int {}"#,
//             fn: parse_item,
//             expected: Some(Item {
//                 kind: ItemKind::Fn(ast::Fn {
//                     name: Ident { name: "a", span: span!(3, 4) },
//                     params: vec![],
//                     return_type: Some(Ident { name: "int", span: span!(8, 11) }),
//                     body: Block { statements: vec![], span: span!(12, 14) } }
//                 ),
//                 span: span!(0, 14),
//             })
//         };
//     }

//     #[test]
//     fn fn_item_unexpected_eof() {
//         check_parser_error! {
//             source: r#"fn a() {"#,
//             fn: parse_item,
//             expected: Error { kind: ErrorKind::UnexpectedEof, span: span!(8, 8) }
//         };
//     }

//     #[test]
//     fn fn_no_name() {
//         check_parser_error! {
//             source: r#"fn () {}"#,
//             fn: parse_item,
//             expected: Error { kind: ErrorKind::ExpectedIdent, span: span!(3, 3) }
//         };
//     }

//     #[test]
//     fn fn_single_argument() {
//         check_parser! {
//             source: r#"fn a(x: int) {}"#,
//             fn: parse_item,
//             expected: Some(Item {
//                 kind: ItemKind::Fn(ast::Fn {
//                     name: Ident { name: "a", span: span!(3, 4) },
//                     params: vec![FnParam { name: Ident { name: "x", span: span!(5, 6) }, ty: Ident { name: "int", span: span!(8, 11) }}],
//                     return_type: None,
//                     body: Block { statements: vec![], span: span!(13, 15) } }
//                 ),
//                 span: span!(0, 15)
//             })
//         };
//     }

//     #[test]
//     fn fn_multiple_arguments() {
//         check_parser! {
//             source: r#"fn a(x: int, y: str) {}"#,
//             fn: parse_item,
//             expected: Some(Item {
//                 kind: ItemKind::Fn(ast::Fn {
//                     name: Ident { name: "a", span: span!(3, 4) },
//                     params: vec![
//                         FnParam { name: Ident { name: "x", span: span!(5, 6) }, ty: Ident { name: "int", span: span!(8, 11) }},
//                         FnParam { name: Ident { name: "y", span: span!(13, 14) }, ty: Ident { name: "str", span: span!(16, 19) }}
//                     ],
//                     return_type: None,
//                     body: Block { statements: vec![], span: span!(21, 23) } }
//                 ),
//                 span: span!(0, 23)
//             })
//         };
//     }
// }
