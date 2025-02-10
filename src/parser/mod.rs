use ariadne::Report;
use error::ParserError;

use crate::{
    ast::{Ast, Ident},
    lexer::{
        Lexer,
        token::{Keyword, Token, TokenKind},
    },
    source::{SourceFile, Span, Spanned},
    span,
};

// mod block;
// mod expression;
mod ident;
mod item;
// mod literal;
mod module;
// mod statement;

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

        Ok(self.ast)
    }

    pub fn eat(&mut self) -> Option<Token> {
        self.prev_span = self.span();
        match self.lexer.next() {
            Some(token) => match token {
                Ok(token) => Some(token),
                Err(error) => {
                    self.errors.push(error.into());
                    None
                }
            },
            _ => None,
        }
    }

    pub fn eat_expected(&mut self, expected: TokenKind) -> Option<Token> {
        match self.eat()? {
            token @ Token { kind, .. } if kind == expected => Some(token),
            Token { kind, span } => {
                self.error(ParserError::ExpectedToken { expected, found: kind }, span);
                // Recover.
                self.eat();
                None
            }
        }
    }

    pub fn try_eat_expected(&mut self, expected: TokenKind) -> Option<Token> {
        match self.lexer.peek() {
            Some(Ok(Token { kind, .. })) if kind == &expected => self.eat(),
            _ => None,
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

    pub fn eat_ident(&mut self, expected: TokenKind) -> Option<Ident<'src>> {
        match self.eat()? {
            Token { kind: TokenKind::Ident, span } => {
                Some(Ident { name: &self.source.as_str()[span.to_range()], span })
            }
            Token { kind, span } => {
                self.error(ParserError::ExpectedToken { expected, found: kind }, span);
                None
            }
        }
    }

    pub fn eat_or_expect_eof(&mut self) -> Option<Token> {
        self.prev_span = self.span();
        match self.lexer.next() {
            Some(Ok(token)) => Some(token),
            Some(Err(error)) => {
                self.errors.push(error.into());
                None
            }
            _ => None,
        }
    }

    pub fn expect_eof(&mut self) {
        if !self.at_eof() {
            self.error_here(ParserError::ExpectedEof);
        }
    }

    // fn err_unexpected_eof(&mut self, token: Option<&Token>) {
    //     let span = match token {
    //         Some(token) => token.span.clone(),
    //         _ => {
    //             let end = self.source.as_str().len().saturating_sub(1);
    //             span!(end)
    //         }
    //     };
    //     self.error(ParserError::UnexpectedEof, span);
    // }

    fn at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }

    pub fn start_span(&mut self) {
        let start = self.span().start();
        self.span_stack.push(span!(start));
    }

    pub fn end_span(&mut self) -> Span {
        let initial = self.span_stack.pop().expect("span stack should not be empty");
        let end = self.prev_span.end();
        span!(initial.start(), end)
    }

    fn span(&mut self) -> Span {
        match self.lexer.peek() {
            Some(Ok(Token { span, .. })) => span.clone(),
            _ => {
                let end = self.source.as_str().len().saturating_sub(1);
                span!(end)
            }
        }
    }

    fn error(&mut self, error: ParserError, span: Span) {
        self.errors.push(Spanned::new(error, span));
    }

    fn error_here(&mut self, error: ParserError) {
        let span = self.span();
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
            expected: $expected:expr
        } => {{
            let source = crate::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::parser::Parser::new(&source);

            let actual = parser.$parse_fn();

            assert_eq!(actual, $expected);
        }};
    }

    #[macro_export]
    macro_rules! check_parser_errors {
        {
            source: $src:expr,
            fn: $parse_fn:ident,
            expected: $expected:expr
        } => {{
            let source = crate::source::SourceFile::new("tests".to_string(), $src);
            let mut parser = crate::parser::Parser::new(&source);

            parser.$parse_fn();

            let actual = parser.errors;

            assert_eq!(actual, $expected);
        }};
    }
}
