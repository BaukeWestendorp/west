use ast::Literal;
use lexer::token::{Token, TokenKind};
use miette::{LabeledSpan, Result};

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_literal(&mut self) -> Result<Literal<'src>> {
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

                Ok(literal)
            }
            Token { kind, span } => {
                let span = span.clone();
                Err(miette::miette!(
                    labels = vec![LabeledSpan::at(span, format!("here"))],
                    "expected literal, found {}: '{}'",
                    kind,
                    &self.ses.source[span.clone()]
                ))
            }
        }
    }

    pub fn can_parse_literal(&mut self) -> bool {
        matches!(self.lexer.peek(), Some(Ok(Token { kind: TokenKind::Literal(_), .. })))
    }
}
