use ast::Block;
use lexer::token::TokenKind;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_block(&mut self) -> Result<Block> {
        self.eat_expected(TokenKind::OpenBrace)?;
        let mut statements = Vec::new();
        while self.can_parse_statement() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        self.eat_expected(TokenKind::CloseBrace)?;
        Ok(Block {})
    }
}
