use std::str::FromStr;

use cursor::Cursor;
use miette::{LabeledSpan, Result};
use token::{Keyword, Literal, Token, TokenKind};

mod cursor;
pub mod token;

pub struct Lexer<'src> {
    cursor: Cursor<'src>,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer { cursor: Cursor::new(source) }
    }
}

impl<'src> Cursor<'src> {
    pub fn advance_token(&mut self) -> Option<Result<Token>> {
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

            '=' => maybe_eq_kind(TokenKind::Equals, TokenKind::EqualsEquals),
            '&' => match self.first() {
                '=' => {
                    self.consume();
                    TokenKind::AndEquals
                }
                '&' => {
                    self.consume();
                    TokenKind::AndAnd
                }
                _ => TokenKind::And,
            },
            '|' => match self.first() {
                '=' => {
                    self.consume();
                    TokenKind::OrEquals
                }
                '|' => {
                    self.consume();
                    TokenKind::OrOr
                }
                _ => TokenKind::Or,
            },
            '<' => maybe_eq_kind(TokenKind::LessThan, TokenKind::LessThanEquals),
            '>' => maybe_eq_kind(TokenKind::MoreThan, TokenKind::MoreThanEquals),
            '!' => maybe_eq_kind(TokenKind::Bang, TokenKind::BangEquals),
            '+' => maybe_eq_kind(TokenKind::Plus, TokenKind::PlusEquals),
            '-' => maybe_eq_kind(TokenKind::Minus, TokenKind::MinusEquals),
            '*' => maybe_eq_kind(TokenKind::Star, TokenKind::StarEquals),
            '/' => maybe_eq_kind(TokenKind::Slash, TokenKind::SlashEquals),

            ';' => TokenKind::Semi,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,

            c if is_ident_start(c) => {
                self.consume_while(|c| is_ident_continue(c));

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
                let span = self.current_span();
                return Some(Err(miette::miette! {
                    labels = vec![LabeledSpan::at(span, format!("this '{first_char}'"))],
                    "unknown token: '{first_char}'",
                }
                .with_source_code(self.whole().to_string())));
            }
        };

        let token = Token::new(token_kind, self.current_span());
        return Some(Ok(token));
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token>;

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
