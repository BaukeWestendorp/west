use ast::Statement;
use lexer::token::TokenKind;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Option<Statement>> {
        let Some(expression) = self.parse_expression()? else {
            return Ok(None);
        };

        self.eat_expected(TokenKind::Semicolon)?;
        Ok(Some(Statement::Expression(expression)))
    }
}

#[cfg(test)]
mod tests {
    use ast::{Expression, Literal};

    use crate::{check_parser, check_parser_error};

    #[test]
    fn statement() {
        let source = west_error::source::SourceFile::new("tests".to_string(), r#"1;"#);
        let mut parser = crate::Parser::new(&source);

        let expr_id = parser.parse_expression().unwrap();
        let expr = expr_id.map(|id| parser.ast.get_expression(&id));

        let expected = Some(&Expression::Literal(Literal::Int(1)));

        assert_eq!(expr, expected);
    }

    #[test]
    fn statement_no_expression() {
        check_parser! {
            source: ";",
            fn: parse_statement,
            expected: None
        };
    }

    #[test]
    fn statement_no_semi() {
        check_parser_error! {
            source: "1",
            fn: parse_statement,
            expected: "unexpected EOF"
        };
    }
}
