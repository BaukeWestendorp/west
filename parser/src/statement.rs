use ast::{Statement, StatementKind};
use lexer::token::{Keyword, TokenKind};
use miette::{Context, Result};

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Option<Statement<'src>>> {
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
        } else {
            Ok(None)
        }
    }

    pub fn parse_statement_expression(&mut self) -> Result<Option<Statement<'src>>> {
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

    pub fn parse_statement_let(&mut self) -> Result<Option<Statement<'src>>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::Let) {
            self.end_span();
            return Ok(None);
        }

        let name = self.eat_ident(TokenKind::Ident)?;
        self.eat_expected(TokenKind::Eq)?;
        let value = self.parse_expression()?.wrap_err("expected expression")?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Statement { kind: StatementKind::Let { name, value }, span: self.end_span() }))
    }

    pub fn parse_statement_if_else(&mut self) -> Result<Option<Statement<'src>>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::If) {
            self.end_span();
            return Ok(None);
        }

        let condition = self.parse_expression()?.wrap_err("expected expression")?;

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

    pub fn parse_statement_print(&mut self) -> Result<Option<Statement<'src>>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::Print) {
            self.end_span();
            return Ok(None);
        }

        let value = self.parse_expression()?.wrap_err("expected expression")?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Statement { kind: StatementKind::Print { value }, span: self.end_span() }))
    }

    pub fn parse_statement_return(&mut self) -> Result<Option<Statement<'src>>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::Return) {
            self.end_span();
            return Ok(None);
        }

        let value = self.parse_expression()?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Statement { kind: StatementKind::Return { value }, span: self.end_span() }))
    }

    pub fn parse_statement_loop(&mut self) -> Result<Option<Statement<'src>>> {
        self.start_span();
        if !self.try_eat_keyword(Keyword::Loop) {
            self.end_span();
            return Ok(None);
        }

        let body = self.parse_block()?;
        Ok(Some(Statement { kind: StatementKind::Loop { body }, span: self.end_span() }))
    }
}

#[cfg(test)]
mod tests {
    use ast::{Expression, ExpressionKind, Ident, Literal, LiteralKind, StatementKind};
    use west_error::source::SourceFile;

    use crate::check_parser;

    #[test]
    fn semicolon_only() {
        check_parser! {
            source: ";",
            fn: parse_statement,
            expected: None
        };
    }

    #[test]
    fn expression() {
        let source = SourceFile::new("tests".to_string(), r#"1.0;"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let StatementKind::Expression(expression) = statement.kind else {
            panic!();
        };
        let value = parser.ast.get_expression(&expression);

        assert_eq!(value, &Expression {
            kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Float(1.0), span: 0..3 }),
            span: 0..3,
        });
    }

    #[test]
    fn r#let() {
        let source = SourceFile::new("tests".to_string(), r#"let x = 1;"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let StatementKind::Let { name, value } = statement.kind else {
            panic!();
        };
        let value = parser.ast.get_expression(&value);

        assert_eq!(name, Ident { name: "x", span: 4..5 });
        assert_eq!(value, &Expression {
            kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 8..9 }),
            span: 8..9,
        });
    }

    #[test]
    fn print() {
        let source = SourceFile::new("tests".to_string(), r#"print 1;"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let StatementKind::Print { value } = statement.kind else {
            panic!();
        };
        let value = parser.ast.get_expression(&value);

        assert_eq!(value, &Expression {
            kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 6..7 }),
            span: 6..7,
        });
    }

    #[test]
    fn return_empty() {
        let source = SourceFile::new("tests".to_string(), r#"return;"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let StatementKind::Return { value } = statement.kind else {
            panic!();
        };

        assert_eq!(value, None);
    }

    #[test]
    fn return_expr() {
        let source = SourceFile::new("tests".to_string(), r#"return 1;"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let StatementKind::Return { value: Some(value) } = statement.kind else {
            panic!();
        };

        let value = parser.ast.get_expression(&value);
        assert_eq!(value, &Expression {
            kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: 7..8 }),
            span: 7..8,
        });
    }

    #[test]
    fn r#loop() {
        let source = SourceFile::new("tests".to_string(), r#"loop {}"#);

        let StatementKind::Loop { body } = statement.kind else {
            panic!();
        };

        assert_eq!(body.span, 5..7);
    }
    
    fn if_else() {
        let source = SourceFile::new("tests".to_string(), r#"if true {}"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let StatementKind::IfElse { condition, then_block, .. } = statement.kind else {
            panic!();
        };

        let condition = parser.ast.get_expression(&condition);
        assert_eq!(condition, &Expression {
            kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Bool(true), span: 3..7 }),
            span: 3..7,
        });

        assert_eq!(then_block.statements.len(), 0);
    }

    #[test]
    fn if_else_else() {
        let source = SourceFile::new("tests".to_string(), r#"if true {} else {}"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let StatementKind::IfElse { condition, then_block, else_block } = statement.kind else {
            panic!();
        };

        let condition = parser.ast.get_expression(&condition);
        assert_eq!(condition, &Expression {
            kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Bool(true), span: 3..7 }),
            span: 3..7,
        });

        assert_eq!(then_block.statements.len(), 0);
        assert_eq!(else_block.unwrap().statements.len(), 0);
    }
}
