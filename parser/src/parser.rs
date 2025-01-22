use ast::File;
use lexer::{
    Lexer,
    token::{Keyword, Token, TokenKind},
};
use miette::{Error, LabeledSpan, Result};

use crate::session::ParserSession;

pub struct Parser<'src> {
    pub ses: ParserSession<'src>,
    pub(crate) lexer: std::iter::Peekable<Lexer<'src>>,
    pub(crate) token: Token,
}

impl<'src> Parser<'src> {
    pub fn new(ses: ParserSession) -> Parser {
        Parser { lexer: Lexer::new(ses.source).peekable(), ses, token: Token::dummy() }
    }

    pub fn parse(mut self) -> Result<File<'src>, Error> {
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
            Some(token) => {
                self.token = token?;
                Ok(self.token.clone())
            }
            _ => Err(self.err_unexpected_eof(None)),
        }
    }

    pub(crate) fn eat_expected(&mut self, expected: TokenKind) -> Result<Token> {
        match self.eat()? {
            token @ Token { kind, .. } if kind == expected => Ok(token),
            Token { kind, span } => Err(miette::miette!(
                labels = vec![LabeledSpan::at(span, format!("here"))],
                "expected {}, found '{}'",
                expected,
                kind
            )
            .with_source_code(self.ses.source.to_string())),
        }
    }

    pub(crate) fn eat_keyword(&mut self, keyword: Keyword) -> bool {
        match self.token {
            Token { kind: TokenKind::Keyword(k), .. } if k == keyword => {
                self.eat().expect("checked if token exist in match");
                true
            }
            _ => false,
        }
    }

    pub(crate) fn eat_or_eof(&mut self) -> Result<Option<Token>> {
        match self.lexer.next() {
            Some(token) => {
                self.token = token?;
                Ok(Some(self.token.clone()))
            }
            _ => Ok(None),
        }
    }

    fn expect_eof(&mut self) -> Result<()> {
        if self.at_eof() {
            return Ok(());
        }

        let token = self.eat()?;
        Err(miette::miette!(
            labels = vec![LabeledSpan::at(token.span.clone(), format!("here"))],
            "expected EOF, found {}",
            token.kind
        )
        .with_source_code(self.ses.source.to_string()))
    }

    fn err_unexpected_eof(&self, token: Option<&Token>) -> Error {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Equals,
    And,
    Or,
    LessThan,
    MoreThan,
    Bang,
    Plus,
    Minus,
    Star,
    Slash,
    EqualsEquals,
    AndAnd,
    OrOr,
    LessThanEquals,
    MoreThanEquals,
    BangEquals,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    AndEquals,
    OrEquals,
}
