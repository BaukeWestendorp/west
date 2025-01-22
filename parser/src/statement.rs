use crate::Parser;

use ast::Statement;
use lexer::token::TokenKind;
use miette::{Context, Result};

impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Statement<'src>> {
        let expression = self.parse_expression().wrap_err("no expression found before ';'")?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Statement::Expression(expression))
    }

    pub fn can_parse_statement(&mut self) -> bool {
        self.can_parse_expression()
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::new_parser;
    use ast::{Expression, Literal, Statement};

    #[test]
    fn statement() {
        let actual = new_parser("1;").parse_statement().unwrap();
        let expected = Statement::Expression(Expression::Literal(Literal::Int(1)));
        assert_eq!(actual, expected);
    }

    #[test]
    fn statement_no_expression() {
        let actual = new_parser(";").parse_statement().unwrap_err();
        let expected = miette::miette!("no expression found before ';'");
        assert_eq!(actual.to_string(), expected.to_string());
    }

    #[test]
    fn statement_no_semi() {
        let actual = new_parser("1").parse_statement().unwrap_err();
        let expected = miette::miette!("unexpected EOF");
        assert_eq!(actual.to_string(), expected.to_string());
    }
}
