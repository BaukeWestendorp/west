use ast::Expression;
use miette::Result;

use crate::{Parser, error::ErrorKind};

impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> Result<Expression<'src>> {
        if self.can_parse_ident() {
            let ident = self.parse_ident()?;
            Ok(Expression::Ident(ident))
        } else if self.can_parse_literal() {
            let literal = self.parse_literal()?;
            Ok(Expression::Literal(literal))
        } else {
            Err(self.err_here(ErrorKind::ExpectedExpression, None))
        }
    }

    pub fn can_parse_expression(&mut self) -> bool {
        self.can_parse_ident() || self.can_parse_literal()
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::new_parser;
    use ast::{Expression, Ident, Literal};

    #[test]
    fn expression_literal_int() {
        let actual = new_parser("1").parse_expression().unwrap();
        let expected = Expression::Literal(Literal::Int(1));
        assert_eq!(actual, expected);
    }

    #[test]
    fn expression_literal_str() {
        let actual = new_parser(r#""hello""#).parse_expression().unwrap();
        let expected = Expression::Literal(Literal::Str("hello"));
        assert_eq!(actual, expected);
    }

    #[test]
    fn expression_ident() {
        let actual = new_parser("a").parse_expression().unwrap();
        let expected = Expression::Ident(Ident("a"));
        assert_eq!(actual, expected);
    }

    #[test]
    fn invalid_expression() {
        let actual = new_parser("*").parse_expression().unwrap_err();
        let expected = miette::miette!("expected expression");
        assert_eq!(actual.to_string(), expected.to_string());
    }
}
