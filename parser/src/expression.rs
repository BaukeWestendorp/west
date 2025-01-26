use ast::{Expression, ExpressionId, Operator};
use lexer::token::TokenKind;
use miette::{Context, Result};

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

                lhs = Expression::BinaryOp {
                    lhs: self.ast.add_expression(lhs),
                    op,
                    rhs: self.ast.add_expression(rhs),
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

        Ok(Expression::FnCall { callee: self.ast.add_expression(callee), args })
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
        Ok(Some(Expression::UnaryOp { op, rhs: self.ast.add_expression(rhs) }))
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

    fn check_simple(source: &str, expected: Option<&Expression>) {
        let source = lexer::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();

        let expr = expr_id.map(|id| parser.ast.get_expression(&id));
        assert_eq!(expr, expected);
    }

    #[test]
    fn expression_literal_int() {
        check_simple(r#"1"#, Some(&Expression::Literal(Literal::Int(1))))
    }

    #[test]
    fn expression_literal_str() {
        check_simple(r#""hello""#, Some(&Expression::Literal(Literal::Str("hello"))))
    }

    #[test]
    fn expression_ident() {
        check_simple(r#"a"#, Some(&Expression::Ident(Ident("a"))))
    }

    #[test]
    fn invalid_expression() {
        check_simple(r#"*"#, None)
    }

    fn check_prefix_op(source: &str, op: Operator, rhs: &Expression) {
        let source = lexer::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();
        let expr = expr_id.map(|id| parser.ast.get_expression(&id)).unwrap();

        match expr {
            Expression::UnaryOp { op: actual_op, rhs: actual_rhs } => {
                assert_eq!(op, *actual_op);

                let actual_rhs = parser.ast.get_expression(actual_rhs);
                assert_eq!(rhs, actual_rhs);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn prefix_minus() {
        check_prefix_op(r#"-1"#, Operator::Minus, &Expression::Literal(Literal::Int(1)))
    }

    #[test]
    fn prefix_negate() {
        check_prefix_op(r#"!true"#, Operator::Negate, &Expression::Literal(Literal::Bool(true)))
    }

    fn check_infix_op(source: &str, op: Operator, lhs: &Expression, rhs: &Expression) {
        let source = lexer::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();
        let expr = expr_id.map(|id| parser.ast.get_expression(&id)).unwrap();

        match expr {
            Expression::BinaryOp { op: actual_op, lhs: actual_lhs, rhs: actual_rhs } => {
                assert_eq!(op, *actual_op);

                let actual_lhs = parser.ast.get_expression(actual_lhs);
                assert_eq!(lhs, actual_lhs);

                let actual_rhs = parser.ast.get_expression(actual_rhs);
                assert_eq!(rhs, actual_rhs);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn infix_add() {
        check_infix_op(
            r#"1 + 2"#,
            Operator::Add,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_subtract() {
        check_infix_op(
            r#"1 - 2"#,
            Operator::Subtract,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_multiply() {
        check_infix_op(
            r#"1 * 2"#,
            Operator::Multiply,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_divide() {
        check_infix_op(
            r#"1 / 2"#,
            Operator::Divide,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_equals() {
        check_infix_op(
            r#"1 == 2"#,
            Operator::Equals,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_and() {
        check_infix_op(
            r#"1 && 2"#,
            Operator::And,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_or() {
        check_infix_op(
            r#"1 || 2"#,
            Operator::Or,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_less_than_equal() {
        check_infix_op(
            r#"1 <= 2"#,
            Operator::LessThanEqual,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_more_than_equal() {
        check_infix_op(
            r#"1 >= 2"#,
            Operator::MoreThanEqual,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    #[test]
    fn infix_not_equal() {
        check_infix_op(
            r#"1 != 2"#,
            Operator::NotEqual,
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
        );
    }

    fn check_call(source: &str, callee: &Expression, args: Vec<&Expression>) {
        let source = lexer::source::SourceFile::new("tests".to_string(), source);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();
        let expr = expr_id.map(|id| parser.ast.get_expression(&id)).unwrap();

        match expr {
            Expression::FnCall { callee: actual_callee, args: actual_args } => {
                let actual_callee = parser.ast.get_expression(actual_callee);
                assert_eq!(callee, actual_callee);

                let actual_args =
                    actual_args.iter().map(|id| parser.ast.get_expression(id)).collect::<Vec<_>>();
                assert_eq!(args, actual_args);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn fn_call_no_args() {
        check_call(r#"test()"#, &Expression::Ident(Ident("test")), vec![])
    }

    #[test]
    fn fn_call_single_arg() {
        check_call(r#"test(1)"#, &Expression::Ident(Ident("test")), vec![&Expression::Literal(
            Literal::Int(1),
        )])
    }

    #[test]
    fn fn_call_multiple_args() {
        check_call(r#"test(1, 2, 3)"#, &Expression::Ident(Ident("test")), vec![
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
            &Expression::Literal(Literal::Int(3)),
        ])
    }

    #[test]
    fn fn_call_trailing_comma() {
        check_call(r#"test(1, 2, 3,)"#, &Expression::Ident(Ident("test")), vec![
            &Expression::Literal(Literal::Int(1)),
            &Expression::Literal(Literal::Int(2)),
            &Expression::Literal(Literal::Int(3)),
        ])
    }

    #[test]
    fn fn_call_double_trailing_comma() {
        let source = lexer::source::SourceFile::new("tests".to_string(), r#"add(1, 2, 3,,)"#);
        let mut parser = crate::Parser::new(&source);

        let actual = parser.parse_expression().unwrap_err();
        let expected = miette::miette!("expected ), found ,");

        assert_eq!(actual.to_string(), expected.to_string());
    }
}
