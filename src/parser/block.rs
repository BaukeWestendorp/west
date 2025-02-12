use crate::ast::Block;
use crate::lexer::token::TokenKind;
use crate::source::Spanned;

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    pub fn parse_block(&mut self) -> Result<Block<'src>, Spanned<ParserError>> {
        self.start_span();
        self.eat_expected(TokenKind::BraceOpen)?;
        let mut statements = Vec::new();
        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }
        self.eat_expected(TokenKind::BraceClose)?;
        Ok(Block { statements, span: self.end_span() })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            Block, Expression, ExpressionKind, Ident, Literal, LiteralKind, Statement,
            StatementKind,
        },
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
                        value: Expression {
                            kind: ExpressionKind::Literal(Literal {
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
            source: r#"{ let x = 1; let y = 2; }"#,
            fn: parse_block,
            expected: Ok(Block {
                statements: vec![
                    Statement {
                        kind: StatementKind::Let {
                            name: Ident { name: "x", span: span!(6, 7) },
                            value: Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(1),
                                    span: span!(10, 11)
                                }),
                                span: span!(10, 11)
                            }
                        },
                        span: span!(2, 12)
                    },
                    Statement {
                        kind: StatementKind::Let {
                            name: Ident { name: "y", span: span!(17, 18) },
                            value: Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(2),
                                    span: span!(21, 22)
                                }),
                                span: span!(21, 22)
                            }
                        },
                        span: span!(13, 23)
                    }
                ],
                span: span!(0, 25)
            }),
            expected_errors: vec![]
        }
    }
}
