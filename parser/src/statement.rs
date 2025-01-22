use crate::Parser;

use ast::Statement;
use lexer::token::TokenKind;
use miette::{Context, Result};

impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Statement<'src>> {
        let expression = self.parse_expression().wrap_err("no expression found before ';'")?;
        self.eat_expected(TokenKind::Semi)?;
        Ok(Statement::Expression(expression))
    }

    pub fn can_parse_statement(&mut self) -> bool {
        self.can_parse_expression()
    }
}
