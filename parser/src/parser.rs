use ast::File;
use lexer::Lexer;
use lexer::token::{Keyword, Token, TokenKind};
use miette::{LabeledSpan, NamedSource, Result, SourceSpan};

use crate::error::ErrorKind;
use crate::session::ParserSession;

pub struct Parser<'src> {
    pub ses: ParserSession<'src>,
    pub(crate) lexer: std::iter::Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(ses: ParserSession) -> Parser {
        Parser { lexer: Lexer::new(ses.source).peekable(), ses }
    }

    pub fn parse(mut self) -> Result<File<'src>> {
        let mut items = Vec::new();
        loop {
            match self.parse_item()? {
                Some(item) => items.push(item),
                _ => break,
            }
        }

        self.expect_eof()?;

        Ok(File { items })
    }

    pub(crate) fn eat(&mut self) -> Result<Token> {
        match self.lexer.next() {
            Some(token) => token,
            _ => Err(self.err_unexpected_eof(None)),
        }
    }

    pub(crate) fn try_eat(&mut self, expected: TokenKind) -> Option<Token> {
        match self.lexer.peek() {
            Some(Ok(Token { kind, .. })) if kind == &expected => self.eat().ok(),
            _ => None,
        }
    }

    pub(crate) fn eat_expected(&mut self, expected: TokenKind) -> Result<Token> {
        match self.eat()? {
            token @ Token { kind, .. } if kind == expected => Ok(token),
            Token { kind, span } => Err(self.err_here(
                ErrorKind::ExpectedToken { expected, found: &kind.to_string() },
                Some(span.into()),
            )),
        }
    }

    pub(crate) fn eat_keyword(&mut self, keyword: Keyword) -> bool {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Keyword(k), .. })) if k == &keyword => {
                self.eat().expect("checked if token exist in match");
                true
            }
            _ => false,
        }
    }

    pub(crate) fn eat_or_eof(&mut self) -> Result<Option<Token>> {
        match self.lexer.next() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(err)) => Err(err),
            _ => Ok(None),
        }
    }

    pub(crate) fn err_here(
        &mut self,
        kind: crate::error::ErrorKind,
        span: Option<SourceSpan>,
    ) -> miette::Error {
        let span = match span {
            Some(span) => span,
            _ => match self.lexer.peek() {
                Some(Ok(Token { span, .. })) => span.clone().into(),
                _ => {
                    let end = self.ses.source.len().saturating_sub(1);
                    end.into()
                }
            },
        };
        miette::Error::from(
            miette::MietteDiagnostic::new(kind.to_string())
                .with_label(LabeledSpan::at(span, "here")),
        )
        .with_source_code(NamedSource::new(self.ses.file_name.clone(), self.ses.source.to_string()))
    }

    fn expect_eof(&mut self) -> Result<()> {
        if self.at_eof() {
            return Ok(());
        }

        let token = self.eat()?;
        Err(self.err_here(ErrorKind::ExpectedEof, Some(token.span.into())))
    }

    fn err_unexpected_eof(&self, token: Option<&Token>) -> miette::Error {
        let span = match token {
            Some(token) => token.span.clone(),
            _ => {
                let end = self.ses.source.len().saturating_sub(1);
                end..end
            }
        };
        miette::miette!(labels = vec![LabeledSpan::at(span, "here")], "unexpected EOF")
            .with_source_code(self.ses.source.to_string())
    }

    fn at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, File, Fn, Ident, Item};

    use crate::tests::new_parser;

    #[test]
    fn file_main_fn() {
        let actual = new_parser(r#"fn main() {}"#).parse().unwrap();
        let expected = File {
            items: vec![Item::Fn(Fn {
                name: Ident("main"),
                params: (),
                body: Block { statements: vec![] },
            })],
        };
        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_file() {
        let actual = new_parser("").parse().unwrap();
        let expected = File { items: vec![] };
        assert_eq!(actual, expected);
    }

    #[test]
    fn file_multiple_items() {
        let actual = new_parser(
            r#"
                fn a() {}
                fn b() {}
            "#,
        )
        .parse()
        .unwrap();
        let expected = File {
            items: vec![
                Item::Fn(Fn { name: Ident("a"), params: (), body: Block { statements: vec![] } }),
                Item::Fn(Fn { name: Ident("b"), params: (), body: Block { statements: vec![] } }),
            ],
        };
        assert_eq!(actual, expected);
    }

    #[test]
    fn file_invalid_first_item() {
        let actual = new_parser("1.0; fn a() {}").parse().unwrap_err();
        let expected = miette::miette!("expected item");

        assert_eq!(actual.to_string(), expected.to_string());
    }

    #[test]
    fn file_invalid_last_item() {
        let actual = new_parser("fn a() {} 1.0").parse().unwrap_err();
        let expected = miette::miette!("expected item");

        assert_eq!(actual.to_string(), expected.to_string());
    }

    #[test]
    fn file_invalid_root() {
        let actual = new_parser("1.0").parse().unwrap_err();
        let expected = miette::miette!("expected item");

        assert_eq!(actual.to_string(), expected.to_string());
    }
}
