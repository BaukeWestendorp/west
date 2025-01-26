use ast::Block;
use lexer::token::TokenKind;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_block(&mut self) -> Result<Block<'src>> {
        self.eat_expected(TokenKind::BraceOpen)?;
        let mut statements = Vec::new();
        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }
        self.eat_expected(TokenKind::BraceClose)?;
        Ok(Block { statements })
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Expression, Literal, Statement};
    use west_error::source::SourceFile;

    use crate::{check_parser, check_parser_error};

    #[test]
    fn block_empty() {
        check_parser! {
            source: r#"{}"#,
            fn: parse_block,
            expected: Block { statements: vec![] }
        }
    }

    #[test]
    fn block_unexpected_eof() {
        check_parser_error! {
            source: r#"{"#,
            fn: parse_block,
            expected: "unexpected EOF"
        }
    }

    #[test]
    fn block_single_statement() {
        let source = SourceFile::new("tests".to_string(), r#"{ let x = 1; }"#);
        let mut parser = crate::Parser::new(&source);

        let block = parser.parse_block().unwrap();

        let Statement::Let { value, .. } = &block.statements[0] else {
            panic!();
        };
        let value = parser.ast.get_expression(&value);

        assert_eq!(value, &Expression::Literal(Literal::Int(1)));
    }

    #[test]
    fn block_multiple_statements() {
        let source = SourceFile::new("tests".to_string(), r#"{ let x = 1; let y = 2; }"#);
        let mut parser = crate::Parser::new(&source);

        let block = parser.parse_block().unwrap();

        let Statement::Let { value: value_1, .. } = &block.statements[0] else {
            panic!();
        };
        let value_1 = parser.ast.get_expression(&value_1);
        assert_eq!(value_1, &Expression::Literal(Literal::Int(1)));

        let Statement::Let { value: value_2, .. } = &block.statements[1] else {
            panic!();
        };
        let value_2 = parser.ast.get_expression(&value_2);
        assert_eq!(value_2, &Expression::Literal(Literal::Int(2)));
    }
}
