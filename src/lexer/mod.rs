use std::str::FromStr;

use cursor::Cursor;
use error::LexerError;
use token::{Keyword, Literal, Token, TokenKind};

use crate::source::{SourceFile, Spanned};

mod cursor;
pub mod error;
pub mod token;

#[derive(Clone)]
pub struct Lexer<'src> {
    cursor: Cursor<'src>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src SourceFile) -> Lexer<'src> {
        Lexer { cursor: Cursor::new(source) }
    }
}

impl Cursor<'_> {
    pub fn advance_token(&mut self) -> Option<Result<Token, Spanned<LexerError>>> {
        self.reset_span_start();
        let first_char = self.consume()?;

        let mut maybe_eq_kind = |single_kind, double_kind| match self.first() {
            '=' => {
                self.consume();
                double_kind
            }
            _ => single_kind,
        };

        let token_kind = match first_char {
            c if is_whitespace(c) => {
                self.consume_while(is_whitespace);
                TokenKind::Whitespace
            }

            '=' => maybe_eq_kind(TokenKind::Eq, TokenKind::EqEq),
            '&' => match self.first() {
                '=' => {
                    self.consume();
                    TokenKind::AmpEq
                }
                '&' => {
                    self.consume();
                    TokenKind::AmpAmp
                }
                _ => TokenKind::Amp,
            },
            '|' => match self.first() {
                '=' => {
                    self.consume();
                    TokenKind::PipeEq
                }
                '|' => {
                    self.consume();
                    TokenKind::PipePipe
                }
                _ => TokenKind::Pipe,
            },
            '<' => maybe_eq_kind(TokenKind::Lt, TokenKind::LtEq),
            '>' => maybe_eq_kind(TokenKind::Gt, TokenKind::GtEq),
            '!' => maybe_eq_kind(TokenKind::Bang, TokenKind::BangEq),
            '+' => maybe_eq_kind(TokenKind::Plus, TokenKind::PlusEq),
            '-' => maybe_eq_kind(TokenKind::Minus, TokenKind::MinusEq),
            '*' => maybe_eq_kind(TokenKind::Star, TokenKind::StarEq),
            '/' => maybe_eq_kind(TokenKind::Slash, TokenKind::SlashEq),

            ';' => TokenKind::Semi,
            '(' => TokenKind::ParenOpen,
            ')' => TokenKind::ParenClose,
            '{' => TokenKind::BraceOpen,
            '}' => TokenKind::BraceClose,
            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,

            c if is_ident_start(c) => {
                self.consume_while(is_ident_continue);

                let ident = self.current_token_str();

                match Keyword::from_str(ident) {
                    Ok(keyword) => TokenKind::Keyword(keyword),
                    Err(_) if is_bool_ident(ident) => TokenKind::Literal(Literal::Bool),
                    Err(_) => TokenKind::Ident,
                }
            }

            c if is_str_lit_start(c) => {
                self.consume_while(|c| !is_str_lit_end(c));
                self.consume(); // consume the closing quote
                TokenKind::Literal(Literal::Str)
            }

            c if is_number_lit_start(c) => {
                let mut lit = Literal::Int;
                self.consume_while(|c| match c {
                    c if is_number_lit_continue(c) => true,
                    '.' if lit == Literal::Int => {
                        lit = Literal::Float;
                        true
                    }
                    '.' if lit == Literal::Float => false,
                    _ => false,
                });
                TokenKind::Literal(lit)
            }

            _ => {
                let error = LexerError::InvalidCharacter(first_char);
                return Some(Err(Spanned::new(error, self.span())));
            }
        };

        let token = Token::new(token_kind, self.span());
        Some(Ok(token))
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, Spanned<LexerError>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(token) = self.cursor.advance_token() {
            if !matches!(token, Ok(Token { kind: TokenKind::Whitespace, .. })) {
                return Some(token);
            }
        }
        None
    }
}

fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

fn is_ident_continue(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

fn is_str_lit_start(c: char) -> bool {
    c == '"'
}

fn is_str_lit_end(c: char) -> bool {
    c == '"'
}

fn is_number_lit_start(c: char) -> bool {
    c.is_ascii_digit()
}

fn is_number_lit_continue(c: char) -> bool {
    c.is_ascii_digit() || c == '_'
}

fn is_bool_ident(ident: &str) -> bool {
    matches!(ident, "true" | "false")
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use crate::lexer::Lexer;
    use crate::lexer::error::LexerError;
    use crate::lexer::token::{Keyword, Literal, Token, TokenKind};
    use crate::source::{SourceFile, Spanned};
    use crate::span;

    fn check_lexer(src: &str, expect: Vec<Token>) {
        let source = SourceFile::new("tests".to_string(), src);
        let actual = Lexer::new(&source).map(Result::unwrap).collect::<Vec<_>>();
        assert_eq!(actual, expect, "actual == expected");
    }

    fn check_lexer_with_reports(src: &str, expect: Vec<Result<Token, Spanned<LexerError>>>) {
        let source = SourceFile::new("tests".to_string(), src);
        let actual = Lexer::new(&source).collect::<Vec<_>>();
        assert_eq!(actual, expect, "actual == expected");
    }

    #[test]
    #[rustfmt::skip]
    fn smoke_test() {
        let source = r#"fn main() { }"#;

        check_lexer(source, vec![
            Token { kind: TokenKind::Keyword(Keyword::Fn), span: span!(0, 2)   },
            Token { kind: TokenKind::Ident,                span: span!(3, 7)   },
            Token { kind: TokenKind::ParenOpen,            span: span!(7, 8)   },
            Token { kind: TokenKind::ParenClose,           span: span!(8, 9)   },
            Token { kind: TokenKind::BraceOpen,            span: span!(10, 11) },
            Token { kind: TokenKind::BraceClose,           span: span!(12, 13) },
        ]);
    }

    #[test]
    fn test_empty_input() {
        check_lexer("", vec![]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_whitespace() {
        #[rustfmt::skip]
        check_lexer(
            "\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}",
            vec![],
        );
    }

    #[test]
    #[rustfmt::skip]
    fn test_symbols() {
        check_lexer("==", vec![Token { kind: TokenKind::EqEq,      span: span!(0, 2) }]);
        check_lexer("&&", vec![Token { kind: TokenKind::AmpAmp,    span: span!(0, 2) }]);
        check_lexer("||", vec![Token { kind: TokenKind::PipePipe,  span: span!(0, 2) }]);
        check_lexer("&=", vec![Token { kind: TokenKind::AmpEq,     span: span!(0, 2) }]);
        check_lexer("|=", vec![Token { kind: TokenKind::PipeEq,    span: span!(0, 2) }]);
        check_lexer("<=", vec![Token { kind: TokenKind::LtEq,      span: span!(0, 2) }]);
        check_lexer(">=", vec![Token { kind: TokenKind::GtEq,      span: span!(0, 2) }]);
        check_lexer("!=", vec![Token { kind: TokenKind::BangEq,    span: span!(0, 2) }]);
        check_lexer("+=", vec![Token { kind: TokenKind::PlusEq,    span: span!(0, 2) }]);
        check_lexer("-=", vec![Token { kind: TokenKind::MinusEq,   span: span!(0, 2) }]);
        check_lexer("*=", vec![Token { kind: TokenKind::StarEq,    span: span!(0, 2) }]);
        check_lexer("/=", vec![Token { kind: TokenKind::SlashEq,   span: span!(0, 2) }]);

        check_lexer("(", vec![Token { kind: TokenKind::ParenOpen,  span: span!(0, 1) }]);
        check_lexer(")", vec![Token { kind: TokenKind::ParenClose, span: span!(0, 1) }]);
        check_lexer("{", vec![Token { kind: TokenKind::BraceOpen,  span: span!(0, 1) }]);
        check_lexer("}", vec![Token { kind: TokenKind::BraceClose, span: span!(0, 1) }]);
        check_lexer(".", vec![Token { kind: TokenKind::Dot,        span: span!(0, 1) }]);
        check_lexer(",", vec![Token { kind: TokenKind::Comma,      span: span!(0, 1) }]);
        check_lexer(":", vec![Token { kind: TokenKind::Colon,      span: span!(0, 1) }]);
        check_lexer(";", vec![Token { kind: TokenKind::Semi,       span: span!(0, 1) }]);
        check_lexer("=", vec![Token { kind: TokenKind::Eq,         span: span!(0, 1) }]);
        check_lexer("&", vec![Token { kind: TokenKind::Amp,        span: span!(0, 1) }]);
        check_lexer("|", vec![Token { kind: TokenKind::Pipe,       span: span!(0, 1) }]);
        check_lexer("<", vec![Token { kind: TokenKind::Lt,         span: span!(0, 1) }]);
        check_lexer(">", vec![Token { kind: TokenKind::Gt,         span: span!(0, 1) }]);
        check_lexer("!", vec![Token { kind: TokenKind::Bang,       span: span!(0, 1) }]);
        check_lexer("+", vec![Token { kind: TokenKind::Plus,       span: span!(0, 1) }]);
        check_lexer("-", vec![Token { kind: TokenKind::Minus,      span: span!(0, 1) }]);
        check_lexer("*", vec![Token { kind: TokenKind::Star,       span: span!(0, 1) }]);
        check_lexer("/", vec![Token { kind: TokenKind::Slash,      span: span!(0, 1) }]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_ident() {
        check_lexer("a",                     vec![Token { kind: TokenKind::Ident, span: span!(0, 1)  }]);
        check_lexer("_",                     vec![Token { kind: TokenKind::Ident, span: span!(0, 1)  }]);
        check_lexer("aa",                    vec![Token { kind: TokenKind::Ident, span: span!(0, 2)  }]);
        check_lexer("__",                    vec![Token { kind: TokenKind::Ident, span: span!(0, 2)  }]);
        check_lexer("_a",                    vec![Token { kind: TokenKind::Ident, span: span!(0, 2)  }]);
        check_lexer("a_",                    vec![Token { kind: TokenKind::Ident, span: span!(0, 2)  }]);
        check_lexer("long_ident_right_here", vec![Token { kind: TokenKind::Ident, span: span!(0, 21) }]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_keywords() {
        check_lexer("fn",     vec![Token { kind: TokenKind::Keyword(Keyword::Fn),    span: span!(0, 2) }]);
        check_lexer("let",    vec![Token { kind: TokenKind::Keyword(Keyword::Let),   span: span!(0, 3) }]);
        check_lexer("return", vec![Token { kind: TokenKind::Keyword(Keyword::Return),span: span!(0, 6) }]);
        check_lexer("print",  vec![Token { kind: TokenKind::Keyword(Keyword::Print), span: span!(0, 5) }]);
        check_lexer("return", vec![Token { kind: TokenKind::Keyword(Keyword::Return),span: span!(0, 6) }]);
        check_lexer("loop",   vec![Token { kind: TokenKind::Keyword(Keyword::Loop),  span: span!(0, 4) }]);
        check_lexer("while",  vec![Token { kind: TokenKind::Keyword(Keyword::While), span: span!(0, 5) }]);
        check_lexer("if",     vec![Token { kind: TokenKind::Keyword(Keyword::If),    span: span!(0, 2) }]);
        check_lexer("else",   vec![Token { kind: TokenKind::Keyword(Keyword::Else),  span: span!(0, 4) }]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_string_literal() {
        check_lexer("\"\"",      vec![Token { kind: TokenKind::Literal(Literal::Str), span: span!(0, 2) }]);
        check_lexer("\"hello\"", vec![Token { kind: TokenKind::Literal(Literal::Str), span: span!(0, 7) }]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_integer_literal() {
        check_lexer("0",           vec![Token { kind: TokenKind::Literal(Literal::Int), span: span!(0, 1)  }]);
        check_lexer("123",         vec![Token { kind: TokenKind::Literal(Literal::Int), span: span!(0, 3)  }]);
        check_lexer("123_456",     vec![Token { kind: TokenKind::Literal(Literal::Int), span: span!(0, 7)  }]);
        check_lexer("123_456_789", vec![Token { kind: TokenKind::Literal(Literal::Int), span: span!(0, 11) }]);
        check_lexer("123_456__",   vec![Token { kind: TokenKind::Literal(Literal::Int), span: span!(0, 9)  }]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_float_literal() {
        check_lexer("0.0",     vec![Token { kind: TokenKind::Literal(Literal::Float), span: span!(0, 3) }]);
        check_lexer("123.456", vec![Token { kind: TokenKind::Literal(Literal::Float), span: span!(0, 7) }]);
        check_lexer(
            "123.456.789",
            vec![
                Token { kind: TokenKind::Literal(Literal::Float), span: span!(0, 7) },
                Token { kind: TokenKind::Dot,                     span: span!(7, 8) },
                Token { kind: TokenKind::Literal(Literal::Int),   span: span!(8, 11) },
            ]
        );
        check_lexer(
            ".123.345",
            vec![
                Token { kind: TokenKind::Dot,                     span: span!(0, 1) },
                Token { kind: TokenKind::Literal(Literal::Float), span: span!(1, 8) },
            ]
        );
    }

    #[test]
    #[rustfmt::skip]
    fn test_bool_literal() {
        check_lexer("true",  vec![Token { kind: TokenKind::Literal(Literal::Bool), span: span!(0, 4) }]);
        check_lexer("false", vec![Token { kind: TokenKind::Literal(Literal::Bool), span: span!(0, 5) }]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_invalid_token() {
        check_lexer_with_reports(";()#", vec![
            Ok(Token { kind: TokenKind::Semi,       span: span!(0, 1) }),
            Ok(Token { kind: TokenKind::ParenOpen,  span: span!(1, 2) }),
            Ok(Token { kind: TokenKind::ParenClose, span: span!(2, 3) }),
            Err(Spanned::new(LexerError::InvalidCharacter('#'), span!(3, 4))),
        ]);
    }
}
