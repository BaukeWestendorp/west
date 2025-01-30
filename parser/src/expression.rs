use ast::{Expression, ExpressionId, ExpressionKind, InfixOp, Op, PostfixOp, PrefixOp};
use lexer::token::TokenKind;
use miette::{Context, Result};
use west_error::ErrorProducer;

use crate::Parser;
use crate::error::ErrorKind;

impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> Result<Option<ExpressionId>> {
        if let Some(expression) = self.parse_expression_bp(0)? {
            Ok(Some(self.ast.add_expression(expression)))
        } else {
            Ok(None)
        }
    }

    fn parse_expression_bp(&mut self, min_bp: u8) -> Result<Option<Expression<'src>>> {
        let mut lhs = if let Some(ident) = self.parse_ident()? {
            let span = ident.span.clone();
            Expression { kind: ExpressionKind::Ident(ident), span }
        } else if let Some(literal) = self.parse_literal()? {
            let span = literal.span.clone();
            Expression { kind: ExpressionKind::Literal(literal), span }
        } else if let Some(expr) = self.parse_prefix_expression()? {
            expr
        } else {
            match self.lexer.peek() {
                Some(Ok(token)) if token.kind == TokenKind::ParenOpen => {
                    self.eat()?;
                    let expr = self
                        .parse_expression_bp(0)
                        .wrap_err("in parentheses")?
                        .wrap_err("expected an expression")?;
                    self.eat_expected(TokenKind::ParenClose)?;
                    expr
                }
                Some(Ok(_)) => return Ok(None),
                Some(Err(_)) => {
                    return Err(self.eat().expect_err("should be checked for Err in match"));
                }
                _ => return Err(self.err_here(ErrorKind::UnexpectedEof)),
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
                    .parse_expression_bp(r_bp)
                    .wrap_err("on the right-hand side")?
                    .wrap_err("expected an expression")?;

                let span = lhs.span.start..self.current_span().end;
                lhs = Expression {
                    kind: ExpressionKind::BinaryOp {
                        lhs: self.ast.add_expression(lhs),
                        op,
                        rhs: self.ast.add_expression(rhs),
                    },
                    span,
                };
                continue;
            }

            break;
        }

        Ok(Some(lhs))
    }

    fn parse_fn_call(&mut self, callee: Expression<'src>) -> Result<Expression<'src>> {
        self.eat()?;
        let mut args = Vec::new();
        while let Some(expression) = self.parse_expression()? {
            args.push(expression);
            if self.try_eat(TokenKind::Comma).is_none() {
                // A single trailing comma is allowed.
                break;
            }
        }
        self.eat_expected(TokenKind::ParenClose)?;

        let span = callee.span.start..self.current_span().end;
        Ok(Expression {
            kind: ExpressionKind::FnCall { callee: self.ast.add_expression(callee), args },
            span,
        })
    }

    fn parse_prefix_expression(&mut self) -> Result<Option<Expression<'src>>> {
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
            .parse_expression_bp(r_bp)
            .wrap_err("in right-hand side")?
            .wrap_err("expected an expression")?;
        Ok(Some(Expression {
            kind: ExpressionKind::UnaryOp { op, rhs: self.ast.add_expression(rhs) },
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
    use ast::{Expression, ExpressionKind, Ident, InfixOp, Literal, LiteralKind, PrefixOp};

    fn check_simple(source: &str, expected: Option<&Expression>) {
        let source = west_error::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();

        let actual = expr_id.map(|id| parser.ast.get_expression(&id));
        assert_eq!(actual, expected);
    }

    #[test]
    fn expression_literal_int() {
        check_simple(
            r#"1"#,
            Some(&Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            }),
        )
    }

    #[test]
    fn expression_literal_str() {
        check_simple(
            r#""hello""#,
            Some(&Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Str("hello"),
                    span: 0..7,
                }),
                span: 0..7,
            }),
        )
    }

    #[test]
    fn expression_ident() {
        check_simple(
            r#"a"#,
            Some(&Expression {
                kind: ExpressionKind::Ident(Ident { name: "a", span: 0..1 }),
                span: 0..1,
            }),
        )
    }

    #[test]
    fn invalid_expression() {
        check_simple(r#"*"#, None)
    }

    fn check_prefix_op(source: &str, op: PrefixOp, rhs: &Expression) {
        let source = west_error::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();
        let expr = expr_id.map(|id| parser.ast.get_expression(&id)).unwrap();

        match &expr.kind {
            ExpressionKind::UnaryOp { op: actual_op, rhs: actual_rhs } => {
                assert_eq!(*actual_op, op);

                let actual_rhs = parser.ast.get_expression(actual_rhs);
                assert_eq!(actual_rhs, rhs);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn prefix_minus() {
        check_prefix_op(r#"-1"#, PrefixOp::Minus, &Expression {
            kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 1..2 }),
            span: 1..2,
        })
    }

    #[test]
    fn prefix_negate() {
        check_prefix_op(r#"!true"#, PrefixOp::Negate, &Expression {
            kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Bool(true), span: 1..5 }),
            span: 1..5,
        })
    }

    fn check_infix_op(source: &str, op: InfixOp, lhs: &Expression, rhs: &Expression) {
        let source = west_error::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();
        let expr = expr_id.map(|id| parser.ast.get_expression(&id)).unwrap();

        match &expr.kind {
            ExpressionKind::BinaryOp { op: actual_op, lhs: actual_lhs, rhs: actual_rhs } => {
                assert_eq!(*actual_op, op);

                let actual_lhs = parser.ast.get_expression(actual_lhs);
                assert_eq!(actual_lhs, lhs);

                let actual_rhs = parser.ast.get_expression(actual_rhs);
                assert_eq!(actual_rhs, rhs);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn infix_add() {
        check_infix_op(
            r#"1 + 2"#,
            InfixOp::Add,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 4..5 }),
                span: 4..5,
            },
        );
    }

    #[test]
    fn infix_subtract() {
        check_infix_op(
            r#"1 - 2"#,
            InfixOp::Subtract,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 4..5 }),
                span: 4..5,
            },
        );
    }

    #[test]
    fn infix_multiply() {
        check_infix_op(
            r#"1 * 2"#,
            InfixOp::Multiply,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 4..5 }),
                span: 4..5,
            },
        );
    }

    #[test]
    fn infix_divide() {
        check_infix_op(
            r#"1 / 2"#,
            InfixOp::Divide,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 4..5 }),
                span: 4..5,
            },
        );
    }

    #[test]
    fn infix_equals() {
        check_infix_op(
            r#"1 == 2"#,
            InfixOp::Equals,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 5..6 }),
                span: 5..6,
            },
        );
    }

    #[test]
    fn infix_and() {
        check_infix_op(
            r#"1 && 2"#,
            InfixOp::And,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 5..6 }),
                span: 5..6,
            },
        );
    }

    #[test]
    fn infix_or() {
        check_infix_op(
            r#"1 || 2"#,
            InfixOp::Or,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 5..6 }),
                span: 5..6,
            },
        );
    }

    #[test]
    fn infix_less_than_equal() {
        check_infix_op(
            r#"1 <= 2"#,
            InfixOp::LessThanEqual,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 5..6 }),
                span: 5..6,
            },
        );
    }

    #[test]
    fn infix_more_than_equal() {
        check_infix_op(
            r#"1 >= 2"#,
            InfixOp::MoreThanEqual,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 5..6 }),
                span: 5..6,
            },
        );
    }

    #[test]
    fn infix_not_equal() {
        check_infix_op(
            r#"1 != 2"#,
            InfixOp::NotEqual,
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 0..1 }),
                span: 0..1,
            },
            &Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: 5..6 }),
                span: 5..6,
            },
        );
    }

    fn check_call(source: &str, callee: &Expression, args: Vec<&Expression>) {
        let source = west_error::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();
        let expr = expr_id.map(|id| parser.ast.get_expression(&id)).unwrap();

        match &expr.kind {
            ExpressionKind::FnCall { callee: actual_callee, args: actual_args } => {
                let actual_callee = parser.ast.get_expression(actual_callee);
                assert_eq!(actual_callee, callee);

                let actual_args =
                    actual_args.iter().map(|id| parser.ast.get_expression(id)).collect::<Vec<_>>();
                assert_eq!(actual_args, args);
            }
            expr => panic!("expected a function call, found {:?}", expr),
        }
    }

    #[test]
    fn fn_call_no_args() {
        check_call(
            r#"test()"#,
            &Expression {
                kind: ExpressionKind::Ident(Ident { name: "test", span: 0..4 }),
                span: 0..4,
            },
            vec![],
        )
    }

    #[test]
    fn fn_call_single_arg() {
        check_call(
            r#"test(1)"#,
            &Expression {
                kind: ExpressionKind::Ident(Ident { name: "test", span: 0..4 }),
                span: 0..4,
            },
            vec![&Expression {
                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 5..6 }),
                span: 5..6,
            }],
        )
    }

    #[test]
    fn fn_call_multiple_args() {
        check_call(
            r#"test(1, 2, 3)"#,
            &Expression {
                kind: ExpressionKind::Ident(Ident { name: "test", span: 0..4 }),
                span: 0..4,
            },
            vec![
                &Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::Int(1),
                        span: 5..6,
                    }),
                    span: 5..6,
                },
                &Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::Int(2),
                        span: 8..9,
                    }),
                    span: 8..9,
                },
                &Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::Int(3),
                        span: 11..12,
                    }),
                    span: 11..12,
                },
            ],
        )
    }

    #[test]
    fn fn_call_trailing_comma() {
        check_call(
            r#"test(1, 2, 3,)"#,
            &Expression {
                kind: ExpressionKind::Ident(Ident { name: "test", span: 0..4 }),
                span: 0..4,
            },
            vec![
                &Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::Int(1),
                        span: 5..6,
                    }),
                    span: 5..6,
                },
                &Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::Int(2),
                        span: 8..9,
                    }),
                    span: 8..9,
                },
                &Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::Int(3),
                        span: 11..12,
                    }),
                    span: 11..12,
                },
            ],
        )
    }

    #[test]
    fn fn_call_double_trailing_comma() {
        let source = west_error::source::SourceFile::new("tests".to_string(), r#"add(1, 2, 3,,)"#);
        let mut parser = crate::Parser::new(&source);

        let actual = parser.parse_expression().unwrap_err();
        let expected = miette::miette!("expected ), found ,");

        assert_eq!(actual.to_string(), expected.to_string());
    }
}
