use ast::Block;
use lexer::token::TokenKind;
use miette::Result;

use crate::Parser;

impl<'src> Parser<'src> {
    pub fn parse_block(&mut self) -> Result<Block> {
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
        let source = lexer::source::SourceFile::new("tests".to_string(), r#"{ 1; }"#);
        let mut parser = crate::Parser::new(&source);

        let block = parser.parse_block().unwrap();

        let Statement::Expression(expr_id) = block.statements[0];
        let stmt_expr = parser.ast.get_expression(&expr_id);

        assert_eq!(stmt_expr, &Expression::Literal(Literal::Int(1)));
    }

    #[test]
    fn block_multiple_statements() {
        let source = lexer::source::SourceFile::new("tests".to_string(), r#"{ 1; 2; }"#);
        let mut parser = crate::Parser::new(&source);

        let block = parser.parse_block().unwrap();

        let Statement::Expression(expr_id_1) = block.statements[0];
        let stmt_expr_1 = parser.ast.get_expression(&expr_id_1);
        assert_eq!(stmt_expr_1, &Expression::Literal(Literal::Int(1)));

        let Statement::Expression(expr_id_2) = block.statements[1];
        let stmt_expr_2 = parser.ast.get_expression(&expr_id_2);
        assert_eq!(stmt_expr_2, &Expression::Literal(Literal::Int(2)));
    }
}
