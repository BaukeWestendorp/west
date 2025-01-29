use ast::Statement;
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
        let expression = self.parse_expression()?;
        if let Some(expression) = expression {
            self.eat_expected(TokenKind::Semi)?;
            Ok(Some(Statement::Expression { expression }))
        } else {
            Ok(None)
        }
    }

    pub fn parse_statement_let(&mut self) -> Result<Option<Statement<'src>>> {
        if !self.try_eat_keyword(Keyword::Let) {
            return Ok(None);
        }

        let name = self.eat_ident(TokenKind::Ident)?;
        self.eat_expected(TokenKind::Eq)?;
        let value = self.parse_expression()?.wrap_err("expected expression")?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Statement::Let { name, value }))
    }

    pub fn parse_statement_print(&mut self) -> Result<Option<Statement<'src>>> {
        if !self.try_eat_keyword(Keyword::Print) {
            return Ok(None);
        }

        let value = self.parse_expression()?.wrap_err("expected expression")?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Some(Statement::Print { value }))
    }
}

#[cfg(test)]
mod tests {
    use ast::{Expression, Ident, Literal, Statement};
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

        let Statement::Expression { expression } = statement else {
            panic!();
        };
        let value = parser.ast.get_expression(&expression);

        assert_eq!(value, &Expression::Literal(Literal::Float(1.0)));
    }

    #[test]
    fn r#let() {
        let source = SourceFile::new("tests".to_string(), r#"let x = 1;"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let Statement::Let { name, value } = statement else {
            panic!();
        };
        let value = parser.ast.get_expression(&value);

        assert_eq!(name, Ident("x"));
        assert_eq!(value, &Expression::Literal(Literal::Int(1)));
    }

    #[test]
    fn print() {
        let source = SourceFile::new("tests".to_string(), r#"print 1;"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let Statement::Print { value } = statement else {
            panic!();
        };
        let value = parser.ast.get_expression(&value);

        assert_eq!(value, &Expression::Literal(Literal::Int(1)));
    }
}
