mod block;
mod expression;
mod file;
mod ident;
mod item;
mod literal;
mod statement;

mod error;

use std::ops::Range;

use ast::Ast;
use lexer::Lexer;
use lexer::token::{Keyword, Token, TokenKind};
use miette::{LabeledSpan, NamedSource, Result};
use west_error::ErrorProducer;
use west_error::source::SourceFile;

use crate::error::ErrorKind;

pub struct Parser<'src> {
    pub(crate) source: &'src SourceFile<'src>,

    pub(crate) lexer: std::iter::Peekable<Lexer<'src>>,

    ast: Ast<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src SourceFile<'src>) -> Parser<'src> {
        Parser { source, lexer: Lexer::new(source).peekable(), ast: Ast::new() }
    }

    pub fn parse(mut self) -> Result<Ast<'src>> {
        let file = self.parse_file()?;

        self.ast.files.push(file);

        Ok(self.ast)
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
            Token { kind, .. } => {
                Err(self.err_here(ErrorKind::ExpectedToken { expected, found: kind.to_string() }))
            }
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

    fn expect_eof(&mut self) -> Result<()> {
        if self.at_eof() {
            return Ok(());
        }

        Err(self.err_here(ErrorKind::ExpectedEof))
    }

    fn err_unexpected_eof(&self, token: Option<&Token>) -> miette::Error {
        let span = match token {
            Some(token) => token.span.clone(),
            _ => {
                let end = self.source.as_str().len().saturating_sub(1);
                end..end
            }
        };
        miette::miette!(labels = vec![LabeledSpan::at(span, "here")], "unexpected EOF")
            .with_source_code(NamedSource::from(self.source))
    }

    fn at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
}

impl ErrorProducer for Parser<'_> {
    type ErrorKind = ErrorKind;

    fn name(&self) -> &str {
        "parser"
    }

    fn source(&self) -> &SourceFile {
        self.source
    }

    fn current_span(&mut self) -> Range<usize> {
        match self.lexer.peek() {
            Some(Ok(Token { span, .. })) => span.clone().into(),
            _ => {
                let end = self.source().as_str().len().saturating_sub(1);
                end..end
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[macro_export]
    macro_rules! check_parser {
        {
            source: $src:expr,
            fn: $parse_fn:ident,
            expected: $expected:expr
        } => {{
            let source = west_error::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::Parser::new(&source);

            let actual = parser.$parse_fn().unwrap();

            assert_eq!(actual, $expected);
        }};
    }

    #[macro_export]
    macro_rules! check_parser_error {
        {
            source: $src:expr,
            fn: $parse_fn:ident,
            expected: $expected:expr
        } => {{
            let source = west_error::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::Parser::new(&source);

            let actual = parser.$parse_fn().unwrap_err();

            assert_eq!(actual.to_string(), $expected.to_string());
        }};
    }
}
