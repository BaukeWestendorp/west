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
        } else if let Some(print_statement) = self.parse_statement_print()? {
            Ok(Some(print_statement))
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
}
