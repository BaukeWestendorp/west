use ast::Statement;
use lexer::token::{Keyword, TokenKind};
use miette::{Context, Result};

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Option<Statement<'src>>> {
        if let Some(let_statement) = self.parse_statement_let()? {
            return Ok(Some(let_statement));
        } else {
            return Ok(None);
        }
    }

    pub fn parse_statement_let(&mut self) -> Result<Option<Statement<'src>>> {
        if !self.try_eat_keyword(Keyword::Let) {
            return Ok(None);
        }

        let name = self.eat_ident(TokenKind::Ident)?;
        self.eat_expected(TokenKind::Equals)?;
        let value = self.parse_expression()?.wrap_err("expected expression")?;
        self.eat_expected(TokenKind::Semicolon)?;
        Ok(Some(Statement::Let { name, value }))
    }
}

#[cfg(test)]
mod tests {
    use ast::{Expression, Ident, Literal, Statement};
    use west_error::source::SourceFile;

    use crate::{check_parser, check_parser_error};

    #[test]
    fn semicolon_only() {
        check_parser! {
            source: ";",
            fn: parse_statement,
            expected: None
        };
    }

    #[test]
    fn statement_no_semicolon() {
        check_parser_error! {
            source: "1",
            fn: parse_statement,
            expected: "unexpected EOF"
        };
    }

    #[test]
    fn let_statement() {
        let source = SourceFile::new("tests".to_string(), r#"let x = 1;"#);
        let mut parser = crate::Parser::new(&source);

        let statement = parser.parse_statement().unwrap().unwrap();

        let Statement::Let { name, value } = statement;
        let value = parser.ast.get_expression(&value);

        assert_eq!(name, Ident("x"));
        assert_eq!(value, &Expression::Literal(Literal::Int(1)));
    }
}
