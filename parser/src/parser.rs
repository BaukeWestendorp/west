use ast::{Block, Expression, File, Ident, Item, Literal, Statement};
use lexer::{
    Lexer,
    token::{Keyword, Token, TokenKind},
};
use miette::{Error, LabeledSpan, Result};

use crate::session::ParserSession;

pub struct Parser<'src> {
    pub ses: ParserSession<'src>,
    lexer: std::iter::Peekable<Lexer<'src>>,
    token: Token,
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

    pub fn parse_item(&mut self) -> Result<Option<Item<'src>>> {
        if self.eat_keyword(Keyword::Fn) {
            Ok(Some(Item::Fn(self.parse_fn()?)))
        } else {
            match self.eat_or_eof()? {
                Some(token) => Err(miette::miette!(
                    labels = vec![LabeledSpan::at(token.span, format!("here"))],
                    "expected an item, but found {}: '{}'",
                    token.kind,
                    &self.ses.source[token.clone().span]
                )
                .with_source_code(self.ses.source.to_string())),
                _ => Ok(None),
            }
        }
    }

    pub fn parse_fn(&mut self) -> Result<ast::Fn<'src>> {
        // FIXME: Make error better.
        let name = self.parse_ident()?;
        self.eat_expected(TokenKind::OpenParen)?;
        let params = ();
        self.eat_expected(TokenKind::CloseParen)?;
        let body = self.parse_block()?;
        Ok(ast::Fn { name, params, body })
    }

    pub fn parse_block(&mut self) -> Result<Block> {
        self.eat_expected(TokenKind::OpenBrace)?;
        let mut statements = Vec::new();
        while !matches!(self.token, Token { kind: TokenKind::CloseBrace, .. }) {
            if let Ok(statement) = self.parse_statement() {
                statements.push(statement);
            }
        }
        self.eat_expected(TokenKind::CloseBrace)?;
        Ok(Block {})
    }

    pub fn parse_statement(&mut self) -> Result<Statement<'src>> {
        let Ok(statement) = self.statement_or_err() else {
            miette::bail!("no expression found before ';'");
        };
        // FIXME: Eat until end of expression, and not just one token.
        self.eat()?;
        Ok(statement)
    }

    fn statement_or_err(&mut self) -> Result<Statement<'src>> {
        // FIXME: Eat until end of expression, and not just one token.
        self.eat()?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Statement::Expression(expression))
    }

    pub fn parse_expression(&mut self) -> Result<Expression<'src>> {
        let expression = self.expression_or_err()?;
        // FIXME: Eat until end of expression, and not just one token.
        self.eat()?;
        Ok(expression)
        // if let Ok(ident) = self.ident_or_err() {
        //     self.eat()?;
        //     Ok(Expression::Ident(ident))
        // } else if let Some(literal) = self.try_parse_literal()? {
        //     Ok(Expression::Literal(literal))
        // } else {
        //     // FIXME: Make error better.
        //     Err(miette::miette!("failed to parse expression")
        //         .with_source_code(self.ses.source.to_string()))
        // }
    }

    fn expression_or_err(&mut self) -> Result<Expression<'src>> {
        if let Ok(ident) = self.ident_or_err() {
            self.eat()?;
            Ok(Expression::Ident(ident))
        } else if let Some(literal) = self.literal_or_err()? {
            Ok(Expression::Literal(literal))
        } else {
            // FIXME: Make error better.
            Err(miette::miette!("failed to parse expression")
                .with_source_code(self.ses.source.to_string()))
        }
    }

    pub fn try_parse_literal(&mut self) -> Result<Option<Literal<'src>>> {
        match self.eat()? {
            Token { kind: TokenKind::Literal(literal), span } => {
                let origin = &self.ses.source[span.clone()];
                let literal = match literal {
                    lexer::token::Literal::Int => {
                        let int = origin.parse().unwrap();
                        Literal::Int(int)
                    }
                    lexer::token::Literal::Float => {
                        let float = origin.parse().unwrap();
                        Literal::Float(float)
                    }
                    lexer::token::Literal::Str => {
                        // FIXME: We should properly unescape the string.
                        let str = &origin[1..origin.len() - 1];
                        Literal::Str(str)
                    }
                    lexer::token::Literal::Bool => {
                        let bool = origin.parse().unwrap();
                        Literal::Bool(bool)
                    }
                };

                Ok(Some(literal))
            }
            Token { kind, span } => {
                let span = span.clone();
                Err(miette::miette!(
                    labels = vec![LabeledSpan::at(span, format!("here"))],
                    "expected a literal, but found {} '{}'",
                    kind,
                    &self.ses.source[span.clone()]
                ))
            }
        }
    }

    pub fn parse_ident(&mut self) -> Result<Ident<'src>> {
        let ident = self.ident_or_err()?;
        self.eat()?;
        Ok(ident)
    }

    fn ident_or_err(&self) -> Result<Ident<'src>> {
        match &self.token {
            Token { kind: TokenKind::Ident, span } => Ok(Ident(&self.ses.source[span.clone()])),
            Token { kind, span, .. } => Err(miette::miette!(
                labels = vec![LabeledSpan::at(span.clone(), format!("here"))],
                "expected an ident, found '{}'",
                kind
            )
            .with_source_code(self.ses.source.to_string())),
        }
    }

    fn eat(&mut self) -> Result<Token> {
        match self.lexer.next() {
            Some(token) => {
                self.token = token?;
                Ok(self.token.clone())
            }
            _ => Err(self.err_unexpected_eof(None)),
        }
    }

    fn eat_or_eof(&mut self) -> Result<Option<Token>> {
        match self.lexer.next() {
            Some(token) => {
                self.token = token?;
                Ok(Some(self.token.clone()))
            }
            _ => Ok(None),
        }
    }

    fn eat_expected(&mut self, expected: TokenKind) -> Result<Token> {
        match self.eat()? {
            token @ Token { kind, .. } if kind == expected => Ok(token),
            Token { kind, span } => Err(miette::miette!(
                labels = vec![LabeledSpan::at(span, format!("here"))],
                "expected '{}', found '{}'",
                expected,
                kind
            )
            .with_source_code(self.ses.source.to_string())),
        }
    }

    fn eat_keyword(&mut self, keyword: Keyword) -> bool {
        match self.token {
            Token { kind: TokenKind::Keyword(k), .. } if k == keyword => {
                self.eat().expect("checked if token exist in match");
                true
            }
            _ => false,
        }
    }

    fn expect_eof(&mut self) -> Result<()> {
        if self.at_eof() {
            return Ok(());
        }

        let token = self.eat()?;
        Err(miette::miette!(
            labels = vec![LabeledSpan::at(token.span.clone(), format!("here"))],
            "expected EOF, found '{}'",
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
