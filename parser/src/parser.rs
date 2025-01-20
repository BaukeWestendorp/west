use lexer::{
    Lexer,
    token::{Keyword, Token, TokenKind},
};
use miette::{Diagnostic, Error, LabeledSpan, Result};
use thiserror::Error;

use crate::session::ParserSession;

#[derive(Diagnostic, Debug, Error)]
#[error("unexpected EOF")]
pub struct Eof;

pub struct Parser<'src> {
    pub ses: ParserSession<'src>,
    lexer: std::iter::Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(ses: ParserSession) -> Parser {
        Parser { lexer: Lexer::new(ses.source).peekable(), ses }
    }

    pub fn parse(mut self) -> Result<ast::File<'src>, Error> {
        let mut items = Vec::new();
        loop {
            match self.parse_item()? {
                Some(item) => items.push(item),
                None => break,
            }
        }

        self.expect_eof()?;

        Ok(ast::File { items })
    }

    pub fn parse_item(&mut self) -> Result<Option<ast::Item<'src>>> {
        if self.consume_keyword(&Keyword::Fn) {
            Ok(Some(ast::Item::Fn(self.try_parse_fn()?)))
        } else {
            match self.lexer.next() {
                Some(Ok(token)) => miette::bail!(miette::miette!(
                    labels = vec![LabeledSpan::at(token.span, format!("here"))],
                    "invalid item: '{}'",
                    token.kind
                )),
                Some(Err(err)) => Err(err),
                None => Ok(None),
            }
        }
    }

    pub fn try_parse_fn(&mut self) -> Result<ast::Fn<'src>> {
        let name = self.try_parse_ident()?;
        self.try_consume_expected(TokenKind::OpenParen)?;
        let params = ();
        self.try_consume_expected(TokenKind::CloseParen)?;
        let body = self.try_parse_block()?;
        Ok(ast::Fn { name, params, body })
    }

    pub fn try_parse_ident(&mut self) -> Result<ast::Ident<'src>> {
        let ident = self.try_consume_expected(TokenKind::Ident)?;
        let origin = &self.ses.source[ident.span];
        Ok(ast::Ident(origin))
    }

    pub fn try_parse_block(&mut self) -> Result<ast::Block> {
        self.try_consume_expected(TokenKind::OpenBrace)?;
        self.try_consume_expected(TokenKind::CloseBrace)?;
        Ok(ast::Block {})
    }

    fn try_consume_expected(&mut self, expected: TokenKind) -> Result<Token> {
        let token = self.lexer.next();
        match token {
            Some(Ok(token @ Token { kind, .. })) if kind == expected => Ok(token),
            Some(Ok(Token { kind, span })) => Err(miette::miette!(
                labels = vec![LabeledSpan::at(span, format!("here"))],
                "expected '{}', found '{}'",
                expected,
                kind
            )),
            Some(Err(err)) => Err(err),
            None => Err(Eof.into()),
        }
    }

    fn consume_keyword(&mut self, keyword: &Keyword) -> bool {
        match self.lexer.peek() {
            Some(Ok(Token { kind: TokenKind::Keyword(k), .. })) if k == keyword => {
                self.lexer.next();
                true
            }
            _ => false,
        }
    }

    fn expect_eof(&mut self) -> Result<()> {
        match self.lexer.next() {
            Some(Ok(token)) => Err(miette::miette!(
                labels = vec![LabeledSpan::at(token.span, format!("here"))],
                "expected EOF, found '{}'",
                token.kind
            )),
            Some(Err(err)) => Err(err),
            None => Ok(()),
        }
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
