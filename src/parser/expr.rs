use crate::ast::{Expr, ExprKind, InfixOp, Op, PostfixOp, PrefixOp};
use crate::lexer::token::TokenKind;
use crate::source::Spanned;

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    pub fn parse_expr(&mut self) -> Result<Option<Expr<'src>>, Spanned<ParserError>> {
        if let Some(expr) = self.parse_expr_bp(0)? { Ok(Some(expr)) } else { Ok(None) }
    }

    #[tracing::instrument(level = "trace", skip(self))]
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Option<Expr<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();

        let mut lhs = if let Ok(ident) = self.parse_ident() {
            Expr { kind: ExprKind::Ident(ident), span: self.end_span(span_start) }
        } else if let Some(literal) = self.parse_literal()? {
            Expr { kind: ExprKind::Literal(literal), span: self.end_span(span_start) }
        } else if let Some(expr) = self.parse_prefix_expr()? {
            expr
        } else {
            match self.lexer.peek() {
                Some(Ok(token)) if token.kind == TokenKind::ParenOpen => {
                    self.eat()?;
                    let expr = self
                        .parse_expr_bp(0)?
                        .ok_or(Spanned::new(ParserError::ExpectedExpr, self.current_span()))?;

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

                let rhs = self
                    .parse_expr_bp(r_bp)?
                    .ok_or(Spanned::new(ParserError::ExpectedExpr, self.end_span(span_start)))?;

                lhs = Expr {
                    kind: ExprKind::BinaryOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) },
                    span: self.end_span(span_start),
                };
                continue;
            }

            break;
        }

        Ok(Some(lhs))
    }

    fn parse_fn_call(&mut self, callee: Expr<'src>) -> Result<Expr<'src>, Spanned<ParserError>> {
        let span_start = callee.span.start();

        self.eat()?;
        let mut args = Vec::new();
        while let Some(expr) = self.parse_expr()? {
            args.push(Box::new(expr));
            if !self.try_eat_expected(TokenKind::Comma) {
                // A single trailing comma is allowed.
                break;
            }
        }
        self.eat_expected(TokenKind::ParenClose)?;

        Ok(Expr {
            kind: ExprKind::FnCall { callee: Box::new(callee), args },
            span: self.end_span(span_start),
        })
    }

    fn parse_prefix_expr(&mut self) -> Result<Option<Expr<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();
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
        let rhs = self
            .parse_expr_bp(r_bp)?
            .ok_or(Spanned::new(ParserError::ExpectedExpr, self.end_span(span_start)))?;
        Ok(Some(Expr {
            kind: ExprKind::UnaryOp { op, rhs: Box::new(rhs) },
            span: self.end_span(span_start),
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
    use test_log::test;

    use crate::ast::{Expr, ExprKind, Ident, InfixOp, Literal, LiteralKind, PrefixOp};
    use crate::lexer::token::TokenKind;
    use crate::parser::error::ParserError;
    use crate::source::Spanned;
    use crate::span;

    // FIXME: Can't this be `check_parser!`?
    fn check(source: &str, expected: Result<Option<Expr>, Spanned<ParserError>>) {
        let source = crate::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::parser::Parser::new(&source);

        let actual = parser.parse_expr();

        eprintln!("source:\n{:?}\n", source.as_str());

        assert_eq!(actual, expected, "actual == expected");
    }

    #[test]
    fn expr_literal_int() {
        check(
            r#"1"#,
            Ok(Some(Expr {
                kind: ExprKind::Literal(Literal { kind: LiteralKind::Int(1), span: span!(0, 1) }),
                span: span!(0, 1),
            })),
        )
    }

    #[test]
    fn expr_literal_str() {
        check(
            r#""hello""#,
            Ok(Some(Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Str("hello"),
                    span: span!(0, 7),
                }),
                span: span!(0, 7),
            })),
        )
    }

    #[test]
    fn expr_ident() {
        check(
            r#"a"#,
            Ok(Some(Expr {
                kind: ExprKind::Ident(Ident { name: "a", span: span!(0, 1) }),
                span: span!(0, 1),
            })),
        )
    }

    #[test]
    fn invalid_expr() {
        check(r#"*"#, Ok(None))
    }

    #[test]
    fn prefix_minus() {
        check(
            r#"-1"#,
            Ok(Some(Expr {
                kind: ExprKind::UnaryOp {
                    op: PrefixOp::Minus,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(1, 2),
                        }),
                        span: span!(1, 2),
                    }),
                },
                span: span!(0, 2),
            })),
        )
    }

    #[test]
    fn prefix_negate() {
        check(
            r#"!true"#,
            Ok(Some(Expr {
                kind: ExprKind::UnaryOp {
                    op: PrefixOp::Negate,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Bool(true),
                            span: span!(1, 5),
                        }),
                        span: span!(1, 5),
                    }),
                },
                span: span!(0, 5),
            })),
        );
    }

    #[test]
    fn infix_add() {
        check(
            r#"1 + 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Add,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            })),
        );
    }

    #[test]
    fn infix_subtract() {
        check(
            r#"1 - 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Subtract,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            })),
        );
    }

    #[test]
    fn infix_multiply() {
        check(
            r#"1 * 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Multiply,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            })),
        );
    }

    #[test]
    fn infix_divide() {
        check(
            r#"1 / 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Divide,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            })),
        );
    }

    #[test]
    fn infix_less_than() {
        check(
            r#"1 < 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::LessThan,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            })),
        );
    }

    #[test]
    fn infix_more_than() {
        check(
            r#"1 > 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::MoreThan,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(4, 5),
                        }),
                        span: span!(4, 5),
                    }),
                },
                span: span!(0, 5),
            })),
        );
    }

    #[test]
    fn infix_equals() {
        check(
            r#"1 == 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Equals,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            })),
        );
    }

    #[test]
    fn infix_and() {
        check(
            r#"1 && 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::And,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            })),
        );
    }

    #[test]
    fn infix_or() {
        check(
            r#"1 || 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::Or,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            })),
        );
    }

    #[test]
    fn infix_less_than_equal() {
        check(
            r#"1 <= 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::LessThanEqual,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            })),
        );
    }

    #[test]
    fn infix_more_than_equal() {
        check(
            r#"1 >= 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::MoreThanEqual,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            })),
        );
    }

    #[test]
    fn infix_not_equal() {
        check(
            r#"1 != 2"#,
            Ok(Some(Expr {
                kind: ExprKind::BinaryOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(0, 1),
                        }),
                        span: span!(0, 1),
                    }),
                    op: InfixOp::NotEqual,
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    }),
                },
                span: span!(0, 6),
            })),
        );
    }

    #[test]
    fn infix_incomplete() {
        check(r#"1 + "#, Err(Spanned::new(ParserError::UnexpectedEof, span!(4, 4))));
    }

    #[test]
    fn fn_call_no_args() {
        check(
            r#"test()"#,
            Ok(Some(Expr {
                kind: ExprKind::FnCall {
                    callee: Box::new(Expr {
                        kind: ExprKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![],
                },
                span: span!(0, 6),
            })),
        )
    }

    #[test]
    fn fn_call_single_arg() {
        check(
            r#"test(1)"#,
            Ok(Some(Expr {
                kind: ExprKind::FnCall {
                    callee: Box::new(Expr {
                        kind: ExprKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![Box::new(Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: span!(5, 6),
                        }),
                        span: span!(5, 6),
                    })],
                },
                span: span!(0, 7),
            })),
        )
    }

    #[test]
    fn fn_call_multiple_args() {
        check(
            r#"test(1, 2,3)"#,
            Ok(Some(Expr {
                kind: ExprKind::FnCall {
                    callee: Box::new(Expr {
                        kind: ExprKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![
                        Box::new(Expr {
                            kind: ExprKind::Literal(Literal {
                                kind: LiteralKind::Int(1),
                                span: span!(5, 6),
                            }),
                            span: span!(5, 6),
                        }),
                        Box::new(Expr {
                            kind: ExprKind::Literal(Literal {
                                kind: LiteralKind::Int(2),
                                span: span!(8, 9),
                            }),
                            span: span!(8, 9),
                        }),
                        Box::new(Expr {
                            kind: ExprKind::Literal(Literal {
                                kind: LiteralKind::Int(3),
                                span: span!(10, 11),
                            }),
                            span: span!(10, 11),
                        }),
                    ],
                },
                span: span!(0, 12),
            })),
        )
    }

    #[test]
    fn fn_call_trailing_comma() {
        check(
            r#"test(1, 2,3,  )"#,
            Ok(Some(Expr {
                kind: ExprKind::FnCall {
                    callee: Box::new(Expr {
                        kind: ExprKind::Ident(Ident { name: "test", span: span!(0, 4) }),
                        span: span!(0, 4),
                    }),
                    args: vec![
                        Box::new(Expr {
                            kind: ExprKind::Literal(Literal {
                                kind: LiteralKind::Int(1),
                                span: span!(5, 6),
                            }),
                            span: span!(5, 6),
                        }),
                        Box::new(Expr {
                            kind: ExprKind::Literal(Literal {
                                kind: LiteralKind::Int(2),
                                span: span!(8, 9),
                            }),
                            span: span!(8, 9),
                        }),
                        Box::new(Expr {
                            kind: ExprKind::Literal(Literal {
                                kind: LiteralKind::Int(3),
                                span: span!(10, 11),
                            }),
                            span: span!(10, 11),
                        }),
                    ],
                },
                span: span!(0, 15),
            })),
        )
    }

    #[test]
    fn fn_call_double_trailing_comma() {
        check(
            r#"test(1,,)"#,
            Err(Spanned::new(
                ParserError::ExpectedToken {
                    expected: TokenKind::ParenClose,
                    found: TokenKind::Comma,
                },
                span!(7, 8),
            )),
        )
    }
}
