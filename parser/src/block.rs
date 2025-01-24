use ast::Block;
use lexer::token::TokenKind;
use miette::{Context, Result};

use crate::Parser;
use crate::error::ErrorKind;

impl<'src> Parser<'src> {
    pub fn parse_block(&mut self) -> Result<Block<'src>> {
        self.eat_expected(TokenKind::BraceOpen)?;
        let mut statements = Vec::new();
        while self.can_parse_statement() {
            let statement = self.parse_statement().wrap_err(ErrorKind::ExpectedStatement)?;
            statements.push(statement);
        }
        self.eat_expected(TokenKind::BraceClose)?;
        Ok(Block { statements })
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Expression, Literal, Statement};

    use crate::tests::new_parser;

    #[test]
    fn block_empty() {
        let actual = new_parser("{}").parse_block().unwrap();
        let expected = Block { statements: vec![] };
        assert_eq!(actual, expected);
    }

    #[test]
    fn block_unexpected_eof() {
        let actual = new_parser("{").parse_block().unwrap_err();
        let expected = miette::miette!("unexpected EOF");
        assert_eq!(actual.to_string(), expected.to_string());
    }

    #[test]
    fn block_single_statement() {
        let actual = new_parser("{ 1; }").parse_block().unwrap();
        let expected =
            Block { statements: vec![Statement::Expression(Expression::Literal(Literal::Int(1)))] };
        assert_eq!(actual, expected);
    }

    #[test]
    fn block_multiple_statements() {
        let actual = new_parser("{ 1; 2; }").parse_block().unwrap();
        let expected = Block {
            statements: vec![
                Statement::Expression(Expression::Literal(Literal::Int(1))),
                Statement::Expression(Expression::Literal(Literal::Int(2))),
            ],
        };
        assert_eq!(actual, expected);
    }
}
