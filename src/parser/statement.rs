use crate::ast::{Statement, StatementKind};
use crate::lexer::token::{Keyword, TokenKind};
use crate::source::Spanned;

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Option<Statement<'src>>, Spanned<ParserError>> {
        if let Some(expr_statement) = self.parse_statement_expression()? {
            Ok(Some(expr_statement))
        } else if let Some(let_statement) = self.parse_statement_let()? {
            Ok(Some(let_statement))
        } else if let Some(if_else_statement) = self.parse_statement_if_else()? {
            Ok(Some(if_else_statement))
        } else if let Some(print_statement) = self.parse_statement_print()? {
            Ok(Some(print_statement))
        } else if let Some(return_statement) = self.parse_statement_return()? {
            Ok(Some(return_statement))
        } else if let Some(loop_statement) = self.parse_statement_loop()? {
            Ok(Some(loop_statement))
        } else if let Some(while_statement) = self.parse_statement_while()? {
            Ok(Some(while_statement))
        } else {
            Ok(None)
        }
    }

    pub fn parse_statement_expression(
        &mut self,
    ) -> Result<Option<Statement<'src>>, Spanned<ParserError>> {
        self.start_span();
        let expression = self.parse_expression()?;
        if let Some(expression) = expression {
            self.eat_expected(TokenKind::Semi)?;
            Ok(Some(Statement {
                kind: StatementKind::Expression(expression),
                span: self.end_span(),
            }))
        } else {
            self.end_span();
            Ok(None)
        }
    }

    pub fn parse_statement_let(&mut self) -> Result<Option<Statement<'src>>, Spanned<ParserError>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::Let) {
            self.end_span();
            return Ok(None);
        }

        let name = self.parse_ident()?;
        self.eat_expected(TokenKind::Eq)?;
        let value = self
            .parse_expression()?
            .ok_or(Spanned::new(ParserError::ExpectedExpression, self.current_span()))?;

        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Statement { kind: StatementKind::Let { name, value }, span: self.end_span() }))
    }

    pub fn parse_statement_if_else(
        &mut self,
    ) -> Result<Option<Statement<'src>>, Spanned<ParserError>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::If) {
            self.end_span();
            return Ok(None);
        }

        let condition = self
            .parse_expression()?
            .ok_or(Spanned::new(ParserError::ExpectedExpression, self.current_span()))?;

        let then_block = self.parse_block()?;

        let mut else_block = None;
        if self.try_eat_keyword(Keyword::Else) {
            else_block = Some(self.parse_block()?);
        }

        Ok(Some(Statement {
            kind: StatementKind::IfElse { condition, then_block, else_block },
            span: self.end_span(),
        }))
    }

    pub fn parse_statement_print(
        &mut self,
    ) -> Result<Option<Statement<'src>>, Spanned<ParserError>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::Print) {
            self.end_span();
            return Ok(None);
        }

        let value = self
            .parse_expression()?
            .ok_or(Spanned::new(ParserError::ExpectedExpression, self.current_span()))?;

        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Statement { kind: StatementKind::Print { value }, span: self.end_span() }))
    }

    pub fn parse_statement_return(
        &mut self,
    ) -> Result<Option<Statement<'src>>, Spanned<ParserError>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::Return) {
            self.end_span();
            return Ok(None);
        }

        let value = self.parse_expression()?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Statement { kind: StatementKind::Return { value }, span: self.end_span() }))
    }

    pub fn parse_statement_loop(
        &mut self,
    ) -> Result<Option<Statement<'src>>, Spanned<ParserError>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::Loop) {
            self.end_span();
            return Ok(None);
        }

        let body = self.parse_block()?;
        Ok(Some(Statement { kind: StatementKind::Loop { body }, span: self.end_span() }))
    }

    pub fn parse_statement_while(
        &mut self,
    ) -> Result<Option<Statement<'src>>, Spanned<ParserError>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::While) {
            self.end_span();
            return Ok(None);
        }

        let condition = self
            .parse_expression()?
            .ok_or(Spanned::new(ParserError::ExpectedExpression, self.current_span()))?;
        let body = self.parse_block()?;
        Ok(Some(Statement {
            kind: StatementKind::While { condition, body },
            span: self.end_span(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        Block, Expression, ExpressionKind, Ident, Literal, LiteralKind, Statement, StatementKind,
    };

    use crate::{check_parser, span};

    #[test]
    fn semicolon_only() {
        check_parser! {
            source: ";",
            fn: parse_statement,
            expected: Ok(None),
            expected_errors: vec![]
        };
    }

    #[test]
    fn expression() {
        check_parser! {
            source: "1.0;",
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::Expression(Expression {
                    kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Float(1.0), span: span!(0, 3)}),
                    span: span!(0, 3),
                }),
                span: span!(0, 4),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn r#let() {
        check_parser! {
            source: r#"let x = 1.0;"#,
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::Let { name: Ident { name: "x", span: span!(4, 5) } , value: Expression {
                    kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Float(1.0), span: span!(8, 11)}),
                    span: span!(9, 11),
                } },
                span: span!(0, 12),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn print() {
        check_parser! {
            source: r#"print 1;"#,
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::Print { value: Expression {
                    kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: span!(6, 7)}),
                    span: span!(6, 7),
                } },
                span: span!(0, 8),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn return_empty() {
        check_parser! {
            source: r#"return;"#,
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::Return { value: None },
                span: span!(0, 7),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn return_expr() {
        check_parser! {
            source: r#"return 1;"#,
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::Return { value: Some(Expression {
                    kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: span!(7, 8)}),
                    span: span!(7, 8),
                }) },
                span: span!(0, 9),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn r#loop() {
        check_parser! {
            source: r#"loop {}"#,
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::Loop { body: Block { statements: vec![], span: span!(5, 7) } },
                span: span!(0, 8),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn r#while() {
        check_parser! {
            source: r#"while true {}"#,
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::While {
                    condition: Expression {
                        kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Bool(true), span: span!(6, 10)}),
                        span: span!(6, 10),
                    },
                    body: Block { statements: vec![], span: span!(11, 13) },
                },
                span: span!(0, 13),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn r#if() {
        check_parser! {
            source: r#"if true {}"#,
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::IfElse {
                    condition: Expression {
                        kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Bool(true), span: span!(3, 7)}),
                        span: span!(3, 7),
                    },
                    then_block: Block { statements: vec![], span: span!(8, 10) },
                    else_block: None,
                },
                span: span!(0, 10),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn if_else() {
        check_parser! {
            source: r#"if true {} else {}"#,
            fn: parse_statement,
            expected: Ok(Some(Statement {
                kind: StatementKind::IfElse {
                    condition: Expression {
                        kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Bool(true), span: span!(3, 7)}),
                        span: span!(3, 7),
                    },
                    then_block: Block { statements: vec![], span: span!(8, 10) },
                    else_block: Some(Block { statements: vec![], span: span!(16, 18) }),
                },
                span: span!(0, 18),
            })),
            expected_errors: vec![]
        };
    }
}
