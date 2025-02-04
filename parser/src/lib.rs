mod block;
mod expression;
mod ident;
mod item;
mod literal;
mod module;
mod statement;

mod error;

use ast::{Ast, Ident};
use fout::source::{SourceFile, Span};
use fout::{Error, ErrorProducer, span};
use lexer::Lexer;
use lexer::token::{Keyword, Token, TokenKind};

use crate::error::{ErrorKind, Result};

pub struct Parser<'src> {
    pub(crate) source: &'src SourceFile<'src>,

    pub(crate) lexer: std::iter::Peekable<Lexer<'src>>,

    ast: Ast<'src>,

    span_stack: Vec<Span>,
    prev_span: Span,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src SourceFile<'src>) -> Parser<'src> {
        Parser {
            source,
            lexer: Lexer::new(source).peekable(),
            ast: Ast::new(),
            span_stack: Vec::new(),
            prev_span: span!(0, 0),
        }
    }

    pub fn parse(mut self) -> Result<Ast<'src>> {
        let module = self.parse_module()?;

        self.ast.modules.push(module);

        Ok(self.ast)
    }

    pub(crate) fn eat(&mut self) -> Result<Token> {
        self.prev_span = self.span();
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

    pub(crate) fn try_eat_keyword(&mut self, keyword: Keyword) -> bool {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Keyword(k), .. })) if k == &keyword => {
                self.eat().expect("checked if token exist in match");
                true
            }
            _ => false,
        }
    }

    pub(crate) fn eat_expected(&mut self, expected: TokenKind) -> Result<Token> {
        match self.eat()? {
            token @ Token { kind, .. } if kind == expected => Ok(token),
            Token { kind, .. } => {
                Err(self.error_here(ErrorKind::ExpectedToken { expected, found: kind.to_string() }))
            }
        }
    }

    pub(crate) fn eat_ident(&mut self, expected: TokenKind) -> Result<Ident<'src>> {
        match self.eat()? {
            Token { kind: TokenKind::Ident, span } => {
                Ok(Ident { name: &self.source.as_str()[span.to_range()], span })
            }
            Token { kind, .. } => {
                Err(self.error_here(ErrorKind::ExpectedToken { expected, found: kind.to_string() }))
            }
        }
    }

    pub(crate) fn eat_or_eof(&mut self) -> Result<Option<Token>> {
        self.prev_span = self.span();
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

        Err(self.error_here(ErrorKind::ExpectedEof))
    }

    fn err_unexpected_eof(&self, token: Option<&Token>) -> Error<ErrorKind> {
        let span = match token {
            Some(token) => token.span.clone(),
            _ => {
                let end = self.source.as_str().len().saturating_sub(1);
                span!(end)
            }
        };
        Error { kind: ErrorKind::UnexpectedEof, span }
    }

    fn at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }

    pub(crate) fn start_span(&mut self) {
        let start = self.span().start();
        self.span_stack.push(span!(start));
    }

    pub(crate) fn end_span(&mut self) -> Span {
        let initial = self.span_stack.pop().expect("span stack should not be empty");
        let end = self.prev_span.end();
        span!(initial.start(), end)
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

    fn span(&mut self) -> Span {
        match self.lexer.peek() {
            Some(Ok(Token { span, .. })) => span.clone(),
            _ => {
                let end = self.source().as_str().len().saturating_sub(1);
                span!(end)
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
            let source = fout::source::SourceFile::new("tests".to_string(), $src);
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
            let source = fout::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::Parser::new(&source);

            let actual = parser.$parse_fn().unwrap_err();

            assert_eq!(actual, $expected);
        }};
    }
}
