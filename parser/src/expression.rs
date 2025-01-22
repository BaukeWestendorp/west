use ast::Expression;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> Result<Expression<'src>> {
        if self.can_parse_ident() {
            let ident = self.parse_ident()?;
            return Ok(Expression::Ident(ident));
        }

        if self.can_parse_literal() {
            let literal = self.parse_literal()?;
            return Ok(Expression::Literal(literal));
        }

        // FIXME: Improve error.
        miette::bail!("failed to parse expression")
    }

    pub fn can_parse_expression(&mut self) -> bool {
        self.can_parse_ident() || self.can_parse_literal()
    }
}
