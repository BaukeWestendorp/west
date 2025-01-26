mod block;
mod expression;
mod file;
mod ident;
mod item;
mod literal;
mod statement;

pub mod error;

use ast::Ast;
use lexer::Lexer;
use lexer::source::SourceFile;
use lexer::token::{Keyword, Token, TokenKind};
use miette::{LabeledSpan, NamedSource, Result, SourceSpan};

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
                _ => self.source.as_str().len().saturating_sub(1).into(),
            },
        };
        miette::Error::from(
            miette::MietteDiagnostic::new(kind.to_string())
                .with_label(LabeledSpan::at(span, "here")),
        )
        .with_source_code(NamedSource::from(self.source))
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

#[cfg(test)]
mod tests {
    #[macro_export]
    macro_rules! check_parser {
        {
            source: $src:expr,
            fn: $parse_fn:ident,
            expected: $expected:expr
        } => {{
            let source = lexer::source::SourceFile::new("tests".to_string(), $src);
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
            let source = lexer::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::Parser::new(&source);

            let actual = parser.$parse_fn().unwrap_err();

            assert_eq!(actual.to_string(), $expected.to_string());
        }};
    }
}
