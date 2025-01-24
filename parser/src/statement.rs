use ast::Statement;
use lexer::token::TokenKind;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Option<Statement<'src>>> {
        let Some(expression) = self.parse_expression()? else {
            return Ok(None);
        };

        self.eat_expected(TokenKind::Semicolon)?;
        Ok(Some(Statement::Expression(expression)))
    }
}

#[cfg(test)]
mod tests {
    use ast::{Expression, Literal, Statement};

    use crate::tests::new_parser;

    #[test]
    fn statement() {
        let actual = new_parser("1;").parse_statement().unwrap();
        let expected = Some(Statement::Expression(Expression::Literal(Literal::Int(1))));
        assert_eq!(actual, expected);
    }

    #[test]
    fn statement_no_expression() {
        let actual = new_parser(";").parse_statement().unwrap();
        assert_eq!(actual, None);
    }

    #[test]
    fn statement_no_semi() {
        let actual = new_parser("1").parse_statement().unwrap_err();
        let expected = miette::miette!("unexpected EOF");
        assert_eq!(actual.to_string(), expected.to_string());
    }
}
