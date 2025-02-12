use ariadne::Report;
use error::ParserError;

use crate::{
    ast::Ast,
    lexer::{
        Lexer,
        token::{Keyword, Token, TokenKind},
    },
    source::{SourceFile, Span, Spanned},
    span,
};

mod block;
mod expression;
mod ident;
mod item;
mod literal;
mod module;
mod statement;

pub mod error;

pub struct Parser<'src> {
    pub source: &'src SourceFile<'src>,

    pub lexer: std::iter::Peekable<Lexer<'src>>,

    ast: Ast<'src>,

    span_stack: Vec<Span>,
    prev_span: Span,

    errors: Vec<Spanned<ParserError>>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src SourceFile<'src>) -> Parser<'src> {
        Parser {
            source,
            lexer: Lexer::new(source).peekable(),
            ast: Ast::new(),
            span_stack: Vec::new(),
            prev_span: Span::new(0, 0),

            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Ast<'src>, Vec<Report<'static, Span>>> {
        let module = self.parse_module();

        self.ast.modules.push(module);

        if !self.errors.is_empty() {
            return Err(self.errors.into_iter().map(|error| error.into()).collect());
        }

        Ok(self.ast)
    }

    pub fn eat(&mut self) -> Result<Token, Spanned<ParserError>> {
        self.prev_span = self.current_span();
        match self.lexer.next() {
            Some(token) => token.map_err(|err| err.into()),
            _ => Err(Spanned::new(ParserError::UnexpectedEof, self.prev_span)),
        }
    }

    pub fn eat_expected(&mut self, expected: TokenKind) -> Result<Token, Spanned<ParserError>> {
        match self.eat()? {
            token @ Token { kind, .. } if kind == expected => Ok(token),
            Token { kind, span } => {
                Err(Spanned::new(ParserError::ExpectedToken { expected, found: kind }, span))
            }
        }
    }

    pub fn try_eat_expected(&mut self, expected: TokenKind) -> bool {
        match self.lexer.peek() {
            Some(Ok(Token { kind, .. })) if kind == &expected => {
                self.eat().expect("checked if token exist in match");
                true
            }
            _ => false,
        }
    }

    pub fn try_eat_keyword(&mut self, keyword: Keyword) -> bool {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Keyword(k), .. })) if k == &keyword => {
                self.eat().expect("checked if token exist in match");
                true
            }
            _ => false,
        }
    }

    pub fn eat_or_expect_eof(&mut self) -> Option<Token> {
        self.prev_span = self.current_span();
        match self.lexer.next() {
            Some(Ok(token)) => Some(token),
            Some(Err(error)) => {
                self.errors.push(error.into());
                None
            }
            _ => None,
        }
    }

    fn at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }

    pub fn start_span(&mut self) {
        let start = self.current_span().start();
        self.span_stack.push(span!(start));
    }

    pub fn end_span(&mut self) -> Span {
        let initial = self.span_stack.pop().expect("span stack should not be empty");
        let end = self.prev_span.end();
        span!(initial.start(), end)
    }

    fn current_span(&mut self) -> Span {
        match self.lexer.peek() {
            Some(Ok(Token { span, .. })) => span.clone(),
            _ => {
                let end = self.source.as_str().len();
                span!(end)
            }
        }
    }

    fn error(&mut self, error: ParserError, span: Span) {
        self.errors.push(Spanned::new(error, span));
    }

    fn error_here(&mut self, error: ParserError) {
        let span = self.current_span();
        self.error(error, span);
    }
}

#[cfg(test)]
mod tests {
    #[macro_export]
    macro_rules! check_parser {
        {
            source: $src:expr,
            fn: $parse_fn:ident,
            expected: $expected:expr,
            expected_errors: $expected_errors:expr
        } => {{
            let source = crate::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::parser::Parser::new(&source);

            eprintln!("source:\n{:?}\n", source.as_str());

            let actual = parser.$parse_fn();
            assert_eq!(actual, $expected, "actual == expected");

            let actual_errors = parser.errors;
            assert_eq!(actual_errors, $expected_errors, "actual_errors == expected_errors");
        }};
    }
}
