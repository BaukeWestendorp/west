use ast::{Literal, LiteralKind};
use lexer::token::{Token, TokenKind};

use crate::Parser;
use crate::error::Result;

impl<'src> Parser<'src> {
    pub fn parse_literal(&mut self) -> Result<Option<Literal<'src>>> {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Literal(literal), .. })) => {
                let literal = *literal;
                let span = self.eat()?.span;

                let origin = &self.source.as_str()[span.to_range()];
                let literal = match literal {
                    lexer::token::Literal::Int => {
                        let int = origin.parse().unwrap();
                        Literal { kind: LiteralKind::Int(int), span }
                    }
                    lexer::token::Literal::Float => {
                        let float = origin.parse().unwrap();
                        Literal { kind: LiteralKind::Float(float), span }
                    }
                    lexer::token::Literal::Str => {
                        // FIXME: We should properly unescape the string.
                        let str = &origin[1..origin.len() - 1];
                        Literal { kind: LiteralKind::Str(str), span }
                    }
                    lexer::token::Literal::Bool => {
                        let bool = origin.parse().unwrap();
                        Literal { kind: LiteralKind::Bool(bool), span }
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
    use ast::{Literal, LiteralKind};
    use fout::span;

    use crate::check_parser;

    #[test]
    fn literal_int() {
        let test_int = |input: &str, expected: i64| {
            check_parser! {
                source: input,
                fn: parse_literal,
                expected: Some(Literal { kind: LiteralKind::Int(expected), span: span!(0, input.len())  })
            };
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
            check_parser! {
                source: input,
                fn: parse_literal,
                expected: Some(Literal { kind: LiteralKind::Float(expected), span: span!(0, input.len()) })
            };
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
        check_parser! {
            source: r#""hello""#,
            fn: parse_literal,
            expected: Some(Literal { kind: LiteralKind::Str("hello"), span: span!(0, 7) })
        }
    }

    #[test]
    fn literal_bool() {
        check_parser! {
            source: "true",
            fn: parse_literal,
            expected: Some(Literal { kind: LiteralKind::Bool(true), span: span!(0, 4) })
        }

        check_parser! {
            source: "false",
            fn: parse_literal,
            expected: Some(Literal { kind: LiteralKind::Bool(false), span: span!(0, 5) })
        }
    }

    #[test]
    fn literal_invalid() {
        check_parser! {
            source: "hello",
            fn: parse_literal,
            expected: None
        }
    }
}
