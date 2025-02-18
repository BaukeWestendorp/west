use crate::ast::{Stmt, StmtKind};
use crate::lexer::token::{Keyword, TokenKind};
use crate::source::Spanned;

use super::Parser;
use super::error::ParserError;

impl<'src> Parser<'src> {
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn parse_stmts(&mut self) -> Result<Option<Stmt<'src>>, Spanned<ParserError>> {
        if let Some(expr_stmt) = self.parse_stmt_expr()? {
            Ok(Some(expr_stmt))
        } else if let Some(let_stmt) = self.parse_stmt_let()? {
            Ok(Some(let_stmt))
        } else if let Some(if_else_stmt) = self.parse_stmt_if_else()? {
            Ok(Some(if_else_stmt))
        } else if let Some(print_stmt) = self.parse_stmt_print()? {
            Ok(Some(print_stmt))
        } else if let Some(return_stmt) = self.parse_stmt_return()? {
            Ok(Some(return_stmt))
        } else if let Some(loop_stmt) = self.parse_stmt_loop()? {
            Ok(Some(loop_stmt))
        } else if let Some(while_stmt) = self.parse_stmt_while()? {
            Ok(Some(while_stmt))
        } else {
            Ok(None)
        }
    }

    pub fn parse_stmt_expr(&mut self) -> Result<Option<Stmt<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();
        let expr = self.parse_expr()?;
        if let Some(expr) = expr {
            self.eat_expected(TokenKind::Semi)?;
            Ok(Some(Stmt { kind: StmtKind::Expr(expr), span: self.end_span(span_start) }))
        } else {
            Ok(None)
        }
    }

    pub fn parse_stmt_let(&mut self) -> Result<Option<Stmt<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();
        if !self.try_eat_keyword(Keyword::Let) {
            self.end_span(span_start);
            return Ok(None);
        }

        let name = self.parse_ident()?;
        self.eat_expected(TokenKind::Eq)?;
        let value = self
            .parse_expr()?
            .ok_or(Spanned::new(ParserError::ExpectedExpr, self.current_span()))?;

        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Stmt { kind: StmtKind::Let { name, value }, span: self.end_span(span_start) }))
    }

    pub fn parse_stmt_if_else(&mut self) -> Result<Option<Stmt<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();
        if !self.try_eat_keyword(Keyword::If) {
            self.end_span(span_start);
            return Ok(None);
        }

        let condition = self
            .parse_expr()?
            .ok_or(Spanned::new(ParserError::ExpectedExpr, self.current_span()))?;

        let then_block = self.parse_block()?;

        let mut else_block = None;
        if self.try_eat_keyword(Keyword::Else) {
            else_block = Some(self.parse_block()?);
        }

        Ok(Some(Stmt {
            kind: StmtKind::IfElse { condition, then_block, else_block },
            span: self.end_span(span_start),
        }))
    }

    pub fn parse_stmt_print(&mut self) -> Result<Option<Stmt<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();
        if !self.try_eat_keyword(Keyword::Print) {
            self.end_span(span_start);
            return Ok(None);
        }

        let value = self
            .parse_expr()?
            .ok_or(Spanned::new(ParserError::ExpectedExpr, self.current_span()))?;

        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Stmt { kind: StmtKind::Print { value }, span: self.end_span(span_start) }))
    }

    pub fn parse_stmt_return(&mut self) -> Result<Option<Stmt<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();
        if !self.try_eat_keyword(Keyword::Return) {
            self.end_span(span_start);
            return Ok(None);
        }

        let value = self.parse_expr()?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Stmt { kind: StmtKind::Return { value }, span: self.end_span(span_start) }))
    }

    pub fn parse_stmt_loop(&mut self) -> Result<Option<Stmt<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();
        if !self.try_eat_keyword(Keyword::Loop) {
            self.end_span(span_start);
            return Ok(None);
        }

        let body = self.parse_block()?;
        Ok(Some(Stmt { kind: StmtKind::Loop { body }, span: self.end_span(span_start) }))
    }

    pub fn parse_stmt_while(&mut self) -> Result<Option<Stmt<'src>>, Spanned<ParserError>> {
        let span_start = self.span_start();
        if !self.try_eat_keyword(Keyword::While) {
            self.end_span(span_start);
            return Ok(None);
        }

        let condition = self
            .parse_expr()?
            .ok_or(Spanned::new(ParserError::ExpectedExpr, self.current_span()))?;
        let body = self.parse_block()?;
        Ok(Some(Stmt {
            kind: StmtKind::While { condition, body },
            span: self.end_span(span_start),
        }))
    }
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use crate::ast::{Block, Expr, ExprKind, Ident, Literal, LiteralKind, Stmt, StmtKind};

    use crate::{check_parser, span};

    #[test]
    fn semicolon_only() {
        check_parser! {
            source: ";",
            fn: parse_stmts,
            expected: Ok(None),
            expected_errors: vec![]
        };
    }

    #[test]
    fn expr() {
        check_parser! {
            source: "1.0;",
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::Expr(Expr {
                    kind: ExprKind::Literal(Literal { kind: LiteralKind::Float(1.0), span: span!(0, 3)}),
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
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::Let { name: Ident { name: "x", span: span!(4, 5) } , value: Expr {
                    kind: ExprKind::Literal(Literal { kind: LiteralKind::Float(1.0), span: span!(8, 11)}),
                    span: span!(8, 11),
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
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::Print { value: Expr {
                    kind: ExprKind::Literal(Literal { kind: LiteralKind::Int(1), span: span!(6, 7)}),
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
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::Return { value: None },
                span: span!(0, 7),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn return_expr() {
        check_parser! {
            source: r#"return 1;"#,
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::Return { value: Some(Expr {
                    kind: ExprKind::Literal(Literal { kind: LiteralKind::Int(1), span: span!(7, 8)}),
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
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::Loop { body: Block { stmts: vec![], span: span!(5, 7) } },
                span: span!(0, 7),
            })),
            expected_errors: vec![]
        };
    }

    #[test]
    fn r#while() {
        check_parser! {
            source: r#"while true {}"#,
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::While {
                    condition: Expr {
                        kind: ExprKind::Literal(Literal { kind: LiteralKind::Bool(true), span: span!(6, 10)}),
                        span: span!(6, 10),
                    },
                    body: Block { stmts: vec![], span: span!(11, 13) },
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
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::IfElse {
                    condition: Expr {
                        kind: ExprKind::Literal(Literal { kind: LiteralKind::Bool(true), span: span!(3, 7)}),
                        span: span!(3, 7),
                    },
                    then_block: Block { stmts: vec![], span: span!(8, 10) },
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
            fn: parse_stmts,
            expected: Ok(Some(Stmt {
                kind: StmtKind::IfElse {
                    condition: Expr {
                        kind: ExprKind::Literal(Literal { kind: LiteralKind::Bool(true), span: span!(3, 7)}),
                        span: span!(3, 7),
                    },
                    then_block: Block { stmts: vec![], span: span!(8, 10) },
                    else_block: Some(Block { stmts: vec![], span: span!(16, 18) }),
                },
                span: span!(0, 18),
            })),
            expected_errors: vec![]
        };
    }
}
