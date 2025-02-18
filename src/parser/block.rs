use crate::ast::Block;
use crate::lexer::token::TokenKind;
use crate::source::Spanned;

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn parse_block(&mut self) -> Result<Block<'src>, Spanned<ParserError>> {
        self.scope_depth += 1;
        let span_start = self.span_start();
        self.eat_expected(TokenKind::BraceOpen)?;
        let mut statements = Vec::new();
        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }
        self.eat_expected(TokenKind::BraceClose)?;
        self.scope_depth -= 1;
        Ok(Block { statements, span: self.end_span(span_start) })
    }
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use crate::{
        ast::{Block, Expr, ExprKind, Ident, Literal, LiteralKind, Statement, StatementKind},
        check_parser,
        parser::error::ParserError,
        source::Spanned,
        span,
    };

    #[test]
    fn block_empty() {
        check_parser! {
            source: r#"{   }"#,
            fn: parse_block,
            expected: Ok(Block{ statements:vec![], span: span!(0, 5) }),
            expected_errors: vec![]
        }
    }

    #[test]
    fn block_unexpected_eof() {
        check_parser! {
            source: r#"{"#,
            fn: parse_block,
            expected: Err(Spanned::new(ParserError::UnexpectedEof, span!(1, 1))),
            expected_errors: vec![]
        }
    }

    #[test]
    fn block_single_statement() {
        check_parser! {
            source: r#"{ let x = 1; }"#,
            fn: parse_block,
            expected: Ok(Block {
                statements: vec![Statement {
                    kind: StatementKind::Let {
                        name: Ident { name: "x", span: span!(6, 7) },
                        value: Expr {
                            kind: ExprKind::Literal(Literal {
                                kind: LiteralKind::Int(1),
                                span: span!(10, 11)
                            }),
                            span: span!(10, 11)
                        }
                    },
                    span: span!(2, 12)
                }],
                span: span!(0, 14)
            }),
            expected_errors: vec![]
        }
    }

    #[test]
    fn block_multiple_statements() {
        check_parser! {
            source: r#"{ let xx = 1; let yyy = 2; }"#,
            fn: parse_block,
            expected: Ok(Block {
                statements: vec![
                    Statement {
                        kind: StatementKind::Let {
                            name: Ident { name: "xx", span: span!(6, 8) },
                            value: Expr {
                                kind: ExprKind::Literal(Literal {
                                    kind: LiteralKind::Int(1),
                                    span: span!(11, 12)
                                }),
                                span: span!(11, 12)
                            }
                        },
                        span: span!(2, 13)
                    },
                    Statement {
                        kind: StatementKind::Let {
                            name: Ident { name: "yyy", span: span!(18, 21) },
                            value: Expr {
                                kind: ExprKind::Literal(Literal {
                                    kind: LiteralKind::Int(2),
                                    span: span!(24, 25)
                                }),
                                span: span!(24, 25)
                            }
                        },
                        span: span!(14, 26)
                    }
                ],
                span: span!(0, 28)
            }),
            expected_errors: vec![]
        }
    }
}
