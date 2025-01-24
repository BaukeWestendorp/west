use ast::Literal;
use lexer::token::{Token, TokenKind};
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_literal(&mut self) -> Result<Option<Literal<'src>>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Literal(literal), .. })) => {
                let literal = *literal;
                let span = self.eat()?.span;

                let origin = &self.ses.source[span];
                let literal = match literal {
                    lexer::token::Literal::Int => {
                        let int = origin.parse().unwrap();
                        Literal::Int(int)
                    }
                    lexer::token::Literal::Float => {
                        let float = origin.parse().unwrap();
                        Literal::Float(float)
                    }
                    lexer::token::Literal::Str => {
                        // FIXME: We should properly unescape the string.
                        let str = &origin[1..origin.len() - 1];
                        Literal::Str(str)
                    }
                    lexer::token::Literal::Bool => {
                        let bool = origin.parse().unwrap();
                        Literal::Bool(bool)
                    }
                };

                Ok(Some(literal))
            }
            _ => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::Literal;

    use crate::tests::new_parser;

    #[test]
    fn literal_int() {
        let test_int = |input: &str, expected: i64| {
            let actual = new_parser(input).parse_literal().unwrap();
            let expected = Some(Literal::Int(expected));
            assert_eq!(actual, expected);
        };

        test_int("1", 1);
        test_int("0", 0);
        test_int("1234567890", 1234567890);
        test_int("123456", 123456);
        test_int("123456789", 123456789);
        test_int("123456789", 123456789);
    }

    #[test]
    fn literal_float() {
        let test_float = |input: &str, expected: f64| {
            let actual = new_parser(input).parse_literal().unwrap();
            let expected = Some(Literal::Float(expected));
            assert_eq!(actual, expected);
        };

        test_float("1.0", 1.0);
        test_float("0.0", 0.0);
        test_float("1234567890.0", 1234567890.0);
        test_float("123456.0", 123456.0);
        test_float("123456789.0", 123456789.0);
        test_float("123456789.0", 123456789.0);
        test_float("123.456", 123.456);
        test_float("123.456789", 123.456789);
    }

    #[test]
    fn literal_str() {
        let actual = new_parser(r#""hello""#).parse_literal().unwrap();
        let expected = Some(Literal::Str("hello"));
        assert_eq!(actual, expected);
    }

    #[test]
    fn literal_bool() {
        let actual = new_parser("true").parse_literal().unwrap();
        let expected = Some(Literal::Bool(true));
        assert_eq!(actual, expected);

        let actual = new_parser("false").parse_literal().unwrap();
        let expected = Some(Literal::Bool(false));
        assert_eq!(actual, expected);
    }

    #[test]
    fn literal_invalid() {
        let actual = new_parser("hello").parse_literal().unwrap();
        assert_eq!(actual, None);
    }
}
