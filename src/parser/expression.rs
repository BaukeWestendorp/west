use crate::ast::{Expression, ExpressionKind, InfixOp, Op, PostfixOp, PrefixOp};
use crate::lexer::token::TokenKind;
use crate::source::{Span, Spanned};

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> Result<Option<Expression<'src>>, Spanned<ParserError>> {
        let span_start = self.current_span();
        if let Some(expression) = self.parse_expression_bp(0, span_start)? {
            Ok(Some(expression))
        } else {
            Ok(None)
        }
    }

    fn parse_expression_bp(
        &mut self,
        min_bp: u8,
        span_start: Span,
    ) -> Result<Option<Expression<'src>>, Spanned<ParserError>> {
        let mut lhs = if let Ok(ident) = self.parse_ident() {
            Expression { kind: ExpressionKind::Ident(ident), span: span_start }
        } else if let Some(literal) = self.parse_literal()? {
            Expression { kind: ExpressionKind::Literal(literal), span: span_start }
        } else if let Some(expr) = self.parse_prefix_expression()? {
            expr
        } else {
            let span_start = self.current_span();
            match self.lexer.peek() {
                Some(Ok(token)) if token.kind == TokenKind::ParenOpen => {
                    self.eat()?;
                    let expr = self.parse_expression_bp(0, span_start)?.ok_or(Spanned::new(
                        ParserError::ExpectedExpression,
                        self.current_span(),
                    ))?;

                    self.eat_expected(TokenKind::ParenClose)?;
                    expr
                }
                Some(Ok(_)) => return Ok(None),
                Some(Err(_)) => {
                    return Err(self.eat().expect_err("should be checked for Err in match"));
                }
                _ => {
                    return Err(Spanned::new(ParserError::UnexpectedEof, self.current_span()));
                }
            }
        };

        loop {
            let op = match self.lexer.peek() {
                Some(Ok(token)) => match token.kind {
                    TokenKind::Plus => Op::Infix(InfixOp::Add),
                    TokenKind::Minus => Op::Infix(InfixOp::Subtract),
                    TokenKind::Star => Op::Infix(InfixOp::Multiply),
                    TokenKind::Slash => Op::Infix(InfixOp::Divide),
                    TokenKind::EqEq => Op::Infix(InfixOp::Equals),
                    TokenKind::AmpAmp => Op::Infix(InfixOp::And),
                    TokenKind::PipePipe => Op::Infix(InfixOp::Or),
                    TokenKind::Lt => Op::Infix(InfixOp::LessThan),
                    TokenKind::Gt => Op::Infix(InfixOp::MoreThan),
                    TokenKind::LtEq => Op::Infix(InfixOp::LessThanEqual),
                    TokenKind::GtEq => Op::Infix(InfixOp::MoreThanEqual),
                    TokenKind::BangEq => Op::Infix(InfixOp::NotEqual),
                    TokenKind::ParenOpen => Op::Postfix(PostfixOp::FnCall),
                    _ => break,
                },
                Some(Err(_)) => {
                    return Err(self.eat().expect_err("should be checked for Err in match"));
                }
                _ => return Ok(Some(lhs)),
            };

            if let Op::Postfix(op) = op {
                let (l_bp, ()) = postfix_binding_power(&op);

                if l_bp < min_bp {
                    break;
                }

                lhs = match op {
                    PostfixOp::FnCall => self.parse_fn_call(lhs)?,
                };

                continue;
            }

            if let Op::Infix(op) = op {
                let (l_bp, r_bp) = infix_binding_power(&op);

                if l_bp < min_bp {
                    break;
                }

                self.eat()?;

                let span_start = self.current_span();
                let rhs = self
                    .parse_expression_bp(r_bp, span_start)?
                    .ok_or(Spanned::new(ParserError::ExpectedExpression, self.current_span()))?;

                lhs = Expression {
                    kind: ExpressionKind::BinaryOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) },
                    span: self.current_span(),
                };
                continue;
            }

            break;
        }

        Ok(Some(lhs))
    }

    fn parse_fn_call(
        &mut self,
        callee: Expression<'src>,
    ) -> Result<Expression<'src>, Spanned<ParserError>> {
        self.eat()?;
        let mut args = Vec::new();
        while let Some(expression) = self.parse_expression()? {
            args.push(Box::new(expression));
            if !self.try_eat_expected(TokenKind::Comma) {
                // A single trailing comma is allowed.
                break;
            }
        }
        self.eat_expected(TokenKind::ParenClose)?;

        Ok(Expression {
            kind: ExpressionKind::FnCall { callee: Box::new(callee), args },
            span: self.current_span(),
        })
    }

    fn parse_prefix_expression(
        &mut self,
    ) -> Result<Option<Expression<'src>>, Spanned<ParserError>> {
        let op = match self.lexer.peek() {
            Some(Ok(token)) => match token.kind {
                TokenKind::Minus => PrefixOp::Minus,
                TokenKind::Bang => PrefixOp::Negate,
                _ => return Ok(None),
            },
            _ => return Ok(None),
        };

        self.eat().expect("next token should be an operator");

        let ((), r_bp) = prefix_binding_power(&op);
        let span_start = self.current_span();
        let rhs = self
            .parse_expression_bp(r_bp, span_start)?
            .ok_or(Spanned::new(ParserError::ExpectedExpression, self.current_span()))?;
        Ok(Some(Expression {
            kind: ExpressionKind::UnaryOp { op, rhs: Box::new(rhs) },
            span: self.current_span(),
        }))
    }
}

fn prefix_binding_power(op: &PrefixOp) -> ((), u8) {
    match op {
        PrefixOp::Minus => ((), 1),
        PrefixOp::Negate => ((), 11),
    }
}

fn infix_binding_power(op: &InfixOp) -> (u8, u8) {
    match op {
        InfixOp::BitAnd | InfixOp::BitOr => (3, 4),

        InfixOp::Equals
        | InfixOp::LessThanEqual
        | InfixOp::MoreThanEqual
        | InfixOp::LessThan
        | InfixOp::MoreThan
        | InfixOp::NotEqual => (5, 6),

        InfixOp::Add | InfixOp::Subtract => (7, 8),

        InfixOp::Multiply | InfixOp::Divide => (9, 10),

        InfixOp::And | InfixOp::Or => (11, 12),
    }
}

fn postfix_binding_power(op: &PostfixOp) -> (u8, ()) {
    match op {
        PostfixOp::FnCall => (13, ()),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, ExpressionKind, Ident, InfixOp, Literal, LiteralKind, PrefixOp};
    use crate::span;

    fn check(source: &str, expected: Option<Expression>) {
        let source = crate::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::parser::Parser::new(&source);

        let actual = parser.parse_expression().unwrap();

        assert_eq!(actual, expected, "actual == expected");
    }

    #[test]
    fn expression_literal_int() {
        check(
            r#"1"#,
            Some(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Int(1),
                    span: span!(0, 1),
                }),
                span: span!(0, 1),
            }),
        )
    }

    #[test]
    fn expression_literal_str() {
        check(
            r#""hello""#,
            Some(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Str("hello"),
                    span: span!(0, 7),
                }),
                span: span!(0, 7),
            }),
        )
    }

    #[test]
    fn expression_ident() {
        check(
            r#"a"#,
            Some(Expression {
                kind: ExpressionKind::Ident(Ident { name: "a", span: span!(0, 1) }),
                span: span!(0, 1),
            }),
        )
    }

    #[test]
    fn invalid_expression() {
        check(r#"*"#, None)
    }

    #[test]
    fn prefix_minus() {
        check(
            r#"-1"#,
            Some(Expression {
                kind: ExpressionKind::UnaryOp {
                    op: PrefixOp::Minus,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(1, 2),
                        }),
                        span: span!(1, 2),
                    }),
                },
                span: span!(0, 2),
            }),
        )
    }

    #[test]
    fn prefix_negate() {
        check(
            r#"!true"#,
            Some(Expression {
                kind: ExpressionKind::UnaryOp {
                    op: PrefixOp::Negate,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Bool(true),
                            span: span!(1, 5),
                        }),
                        span: span!(1, 5),
                    }),
                },
                span: span!(0, 5),
            }),
        );
    }

    #[test]
    fn infix_add() {
        check(
            r#"1 + 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Add,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            }),
        );
    }

    #[test]
    fn infix_subtract() {
        check(
            r#"1 - 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Subtract,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            }),
        );
    }

    #[test]
    fn infix_multiply() {
        check(
            r#"1 * 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Multiply,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            }),
        );
    }

    #[test]
    fn infix_divide() {
        check(
            r#"1 / 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Divide,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            }),
        );
    }

    #[test]
    fn infix_less_than() {
        check(
            r#"1 < 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::LessThan,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            }),
        );
    }

    #[test]
    fn infix_more_than() {
        check(
            r#"1 > 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::MoreThan,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            }),
        );
    }

    #[test]
    fn infix_equals() {
        check(
            r#"1 == 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Equals,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            }),
        );
    }

    #[test]
    fn infix_and() {
        check(
            r#"1 && 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::And,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            }),
        );
    }

    #[test]
    fn infix_or() {
        check(
            r#"1 || 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Or,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            }),
        );
    }

    #[test]
    fn infix_less_than_equal() {
        check(
            r#"1 <= 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::LessThanEqual,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            }),
        );
    }

    #[test]
    fn infix_more_than_equal() {
        check(
            r#"1 >= 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::MoreThanEqual,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            }),
        );
    }

    #[test]
    fn infix_not_equal() {
        check(
            r#"1 != 2"#,
            Some(Expression {
                kind: ExpressionKind::BinaryOp {
                    lhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::NotEqual,
                    rhs: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            }),
        );
    }

    #[test]
    fn fn_call_no_args() {
        check(
            r#"test()"#,
            Some(Expression {
                kind: ExpressionKind::FnCall {
                    callee: Box::new(Expression {
                        kind: ExpressionKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![],
                },
                span: span!(0, 6),
            }),
        )
    }

    #[test]
    fn fn_call_single_arg() {
        check(
            r#"test(1)"#,
            Some(Expression {
                kind: ExpressionKind::FnCall {
                    callee: Box::new(Expression {
                        kind: ExpressionKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    })],
                },
                span: span!(0, 7),
            }),
        )
    }

    #[test]
    fn fn_call_multiple_args() {
        check(
            r#"test(1, 2,3)"#,
            Some(Expression {
                kind: ExpressionKind::FnCall {
                    callee: Box::new(Expression {
                        kind: ExpressionKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal {
                                kind: LiteralKind::Int(1),
                                span: span!(5, 6),
                            }),
                            span: span!(5, 6),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal {
                                kind: LiteralKind::Int(2),
                                span: span!(8, 9),
                            }),
                            span: span!(8, 9),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal {
                                kind: LiteralKind::Int(3),
                                span: span!(10, 11),
                            }),
                            span: span!(10, 11),
                        }),
                    ],
                },
                span: span!(0, 12),
            }),
        )
    }

    #[test]
    fn fn_call_trailing_comma() {
        check(
            r#"test(1, 2,3,  )"#,
            Some(Expression {
                kind: ExpressionKind::FnCall {
                    callee: Box::new(Expression {
                        kind: ExpressionKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal {
                                kind: LiteralKind::Int(1),
                                span: span!(5, 6),
                            }),
                            span: span!(5, 6),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal {
                                kind: LiteralKind::Int(2),
                                span: span!(8, 9),
                            }),
                            span: span!(8, 9),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal {
                                kind: LiteralKind::Int(3),
                                span: span!(10, 11),
                            }),
                            span: span!(10, 11),
                        }),
                    ],
                },
                span: span!(0, 12),
            }),
        )
    }

    #[test]
    fn fn_call_double_trailing_comma() {
        check(
            r#"test(1,,)"#,
            Some(Expression {
                kind: ExpressionKind::FnCall {
                    callee: Box::new(Expression {
                        kind: ExpressionKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    })],
                },
                span: span!(0, 12),
            }),
        )
    }
}
