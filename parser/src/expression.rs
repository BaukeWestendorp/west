use ast::{Expression, Operator};
use lexer::token::TokenKind;
use miette::{Context, Result};

use crate::Parser;
use crate::error::ErrorKind;

impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> Result<Option<Expression<'src>>> {
        self.parse_expression_bp(0)
    }

    fn parse_expression_bp(&mut self, min_bp: u8) -> Result<Option<Expression<'src>>> {
        let mut lhs = if let Some(ident) = self.parse_ident()? {
            Expression::Ident(ident)
        } else if let Some(literal) = self.parse_literal()? {
            Expression::Literal(literal)
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
                _ => return Err(self.err_here(ErrorKind::UnexpectedEof, None)),
            }
        };

        match self.lexer.peek() {
            Some(Ok(token)) if token.kind == TokenKind::ParenOpen => {
                lhs = self.parse_fn_call(lhs)?;
            }
            _ => {}
        }

        loop {
            let op = match self.lexer.peek() {
                Some(Ok(token)) => match token.kind {
                    TokenKind::Plus => Operator::Add,
                    TokenKind::Minus => Operator::Subtract,
                    TokenKind::Star => Operator::Multiply,
                    TokenKind::Slash => Operator::Divide,
                    TokenKind::EqualsEquals => Operator::Equals,
                    TokenKind::AmpAmp => Operator::And,
                    TokenKind::PipePipe => Operator::Or,
                    TokenKind::LessThanEquals => Operator::LessThanEqual,
                    TokenKind::MoreThanEquals => Operator::MoreThanEqual,
                    TokenKind::BangEquals => Operator::NotEqual,
                    _ => break,
                },
                Some(Err(_)) => {
                    return Err(self.eat().expect_err("should be checked for Err in match"));
                }
                _ => return Ok(Some(lhs)),
            };

            if let Some((l_bp, r_bp)) = infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }

                self.eat()?;

                let rhs = self
                    .parse_expression_bp(r_bp)
                    .wrap_err("on the right-hand side")?
                    .wrap_err("expected an expression")?;

                lhs = Expression::BinaryOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) };
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
            } else {
            }
        }
        self.eat_expected(TokenKind::ParenClose)?;

        Ok(Expression::FnCall { callee: Box::new(callee), args })
    }

    fn parse_prefix_expression(&mut self) -> Result<Option<Expression<'src>>> {
        let op = match self.lexer.peek() {
            Some(Ok(token)) => match token.kind {
                TokenKind::Minus => Operator::Minus,
                TokenKind::Bang => Operator::Negate,
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
        Ok(Some(Expression::UnaryOp { op, rhs: Box::new(rhs) }))
    }
}

fn prefix_binding_power(op: &Operator) -> ((), u8) {
    match op {
        Operator::Minus => ((), 1),
        Operator::Negate => ((), 11),
        _ => panic!("unexpected prefix operator: {:?}", op),
    }
}

fn infix_binding_power(op: &Operator) -> Option<(u8, u8)> {
    let bp = match op {
        Operator::Assign
        | Operator::AddAssign
        | Operator::SubtractAssign
        | Operator::MultiplyAssign
        | Operator::DivideAssign
        | Operator::BitAndAssign
        | Operator::BitOrAssign => (2, 1),

        Operator::BitAnd | Operator::BitOr => (3, 4),

        Operator::Equals
        | Operator::LessThanEqual
        | Operator::MoreThanEqual
        | Operator::LessThan
        | Operator::MoreThan
        | Operator::NotEqual => (5, 6),

        Operator::Add | Operator::Subtract => (7, 8),

        Operator::Multiply | Operator::Divide => (9, 10),

        Operator::And | Operator::Or => (11, 12),

        _ => return None,
    };

    Some(bp)
}

#[cfg(test)]
mod tests {
    use ast::{Expression, Ident, Literal, Operator};

    use crate::tests::new_parser;

    #[test]
    fn expression_literal_int() {
        let actual = new_parser("1").parse_expression().unwrap();
        let expected = Some(Expression::Literal(Literal::Int(1)));
        assert_eq!(actual, expected);
    }

    #[test]
    fn expression_literal_str() {
        let actual = new_parser(r#""hello""#).parse_expression().unwrap();
        let expected = Some(Expression::Literal(Literal::Str("hello")));
        assert_eq!(actual, expected);
    }

    #[test]
    fn expression_ident() {
        let actual = new_parser("a").parse_expression().unwrap();
        let expected = Some(Expression::Ident(Ident("a")));
        assert_eq!(actual, expected);
    }

    #[test]
    fn invalid_expression() {
        let actual = new_parser("*").parse_expression().unwrap();
        assert_eq!(actual, None);
    }

    #[test]
    fn prefix_minus() {
        let actual = new_parser("-1").parse_expression().unwrap();
        let expected = Some(Expression::UnaryOp {
            op: Operator::Minus,
            rhs: Box::new(Expression::Literal(Literal::Int(1))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn prefix_negate() {
        let actual = new_parser("!true").parse_expression().unwrap();
        let expected = Some(Expression::UnaryOp {
            op: Operator::Negate,
            rhs: Box::new(Expression::Literal(Literal::Bool(true))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_add() {
        let actual = new_parser("1 + 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::Add,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_subtract() {
        let actual = new_parser("1 - 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::Subtract,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_multiply() {
        let actual = new_parser("1 * 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::Multiply,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_divide() {
        let actual = new_parser("1 / 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::Divide,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_equals() {
        let actual = new_parser("1 == 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::Equals,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_and() {
        let actual = new_parser("1 && 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::And,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_or() {
        let actual = new_parser("1 || 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::Or,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_less_than_equal() {
        let actual = new_parser("1 <= 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::LessThanEqual,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_more_than_equal() {
        let actual = new_parser("1 >= 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::MoreThanEqual,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_not_equal() {
        let actual = new_parser("1 != 2").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            op: Operator::NotEqual,
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_complicated() {
        let actual = new_parser("1 + 2 * 3 - 4 / 5").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::BinaryOp {
                lhs: Box::new(Expression::Literal(Literal::Int(1))),
                op: Operator::Add,
                rhs: Box::new(Expression::BinaryOp {
                    lhs: Box::new(Expression::Literal(Literal::Int(2))),
                    op: Operator::Multiply,
                    rhs: Box::new(Expression::Literal(Literal::Int(3))),
                }),
            }),
            op: Operator::Subtract,
            rhs: Box::new(Expression::BinaryOp {
                lhs: Box::new(Expression::Literal(Literal::Int(4))),
                op: Operator::Divide,
                rhs: Box::new(Expression::Literal(Literal::Int(5))),
            }),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn infix_complicated_parens() {
        let actual = new_parser("(1 + 2) * (3 - 4)").parse_expression().unwrap();
        let expected = Some(Expression::BinaryOp {
            lhs: Box::new(Expression::BinaryOp {
                lhs: Box::new(Expression::Literal(Literal::Int(1))),
                op: Operator::Add,
                rhs: Box::new(Expression::Literal(Literal::Int(2))),
            }),
            op: Operator::Multiply,
            rhs: Box::new(Expression::BinaryOp {
                lhs: Box::new(Expression::Literal(Literal::Int(3))),
                op: Operator::Subtract,
                rhs: Box::new(Expression::Literal(Literal::Int(4))),
            }),
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn fn_call_no_args() {
        let actual = new_parser("add()").parse_expression().unwrap();
        let expected = Some(Expression::FnCall {
            callee: Box::new(Expression::Ident(Ident("add"))),
            args: vec![],
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn fn_call_single_arg() {
        let actual = new_parser("add(1)").parse_expression().unwrap();
        let expected = Some(Expression::FnCall {
            callee: Box::new(Expression::Ident(Ident("add"))),
            args: vec![Expression::Literal(Literal::Int(1))],
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn fn_call_multiple_args() {
        let actual = new_parser("add(1, 2, 3)").parse_expression().unwrap();
        let expected = Some(Expression::FnCall {
            callee: Box::new(Expression::Ident(Ident("add"))),
            args: vec![
                Expression::Literal(Literal::Int(1)),
                Expression::Literal(Literal::Int(2)),
                Expression::Literal(Literal::Int(3)),
            ],
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn fn_call_trailing_comma() {
        let actual = new_parser("add(1, 2, 3,)").parse_expression().unwrap();
        let expected = Some(Expression::FnCall {
            callee: Box::new(Expression::Ident(Ident("add"))),
            args: vec![
                Expression::Literal(Literal::Int(1)),
                Expression::Literal(Literal::Int(2)),
                Expression::Literal(Literal::Int(3)),
            ],
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn fn_call_double_trailing_comma() {
        let actual = new_parser("add(1, 2, 3,,)").parse_expression().unwrap_err();
        let expected = miette::miette!("expected ), found ,");
        assert_eq!(actual.to_string(), expected.to_string());
    }

    #[test]
    fn fn_call_nested() {
        let actual = new_parser("add(sub(1, 2), 3)").parse_expression().unwrap();
        let expected = Some(Expression::FnCall {
            callee: Box::new(Expression::Ident(Ident("add"))),
            args: vec![
                Expression::FnCall {
                    callee: Box::new(Expression::Ident(Ident("sub"))),
                    args: vec![
                        Expression::Literal(Literal::Int(1)),
                        Expression::Literal(Literal::Int(2)),
                    ],
                },
                Expression::Literal(Literal::Int(3)),
            ],
        });
        assert_eq!(actual, expected);
    }
}
