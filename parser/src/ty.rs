use ast::ParsedType;
use miette::{Context, Result};

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_type(&mut self) -> Result<ParsedType<'src>> {
        let ident = self.parse_ident()?.wrap_err("expected type")?;
        Ok(ParsedType { ident, id: self.ast.next_type_id() })
    }
}
