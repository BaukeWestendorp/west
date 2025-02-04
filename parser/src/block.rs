use ast::Block;
use lexer::token::TokenKind;

use crate::Parser;
use crate::error::Result;

impl<'src> Parser<'src> {
    pub fn parse_block(&mut self) -> Result<Block<'src>> {
        self.start_span();
        self.eat_expected(TokenKind::BraceOpen)?;
        let mut statements = Vec::new();
        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }
        self.eat_expected(TokenKind::BraceClose)?;
        Ok(Block { statements, span: self.end_span() })
    }
}

#[cfg(test)]
mod tests {
    use ast::{Block, Expression, ExpressionKind, Literal, LiteralKind, StatementKind};
    use fout::source::SourceFile;
    use fout::{Error, span};

    use crate::error::ErrorKind;
    use crate::{check_parser, check_parser_error};

    #[test]
    fn block_empty() {
        check_parser! {
            source: r#"{   }"#,
            fn: parse_block,
            expected: Block { statements: vec![], span: span!(0, 5) }
        }
    }

    #[test]
    fn block_unexpected_eof() {
        check_parser_error! {
            source: r#"{"#,
            fn: parse_block,
            expected: Error { kind: ErrorKind::UnexpectedEof, span: span!(1, 1) }
        }
    }

    #[test]
    fn block_single_statement() {
        let source = SourceFile::new("tests".to_string(), r#"{ let x = 1; }"#);
        let mut parser = crate::Parser::new(&source);

        let block = parser.parse_block().unwrap();

        assert_eq!(block.span, span!(0, 14));

        let StatementKind::Let { value, .. } = &block.statements[0].kind else {
            panic!();
        };
        let value = parser.ast.get_expression(&value);

        assert_eq!(value, &Expression {
            kind: ExpressionKind::Literal(Literal {
                kind: LiteralKind::Int(1),
                span: span!(10, 11)
            }),
            span: span!(10, 11)
        });
    }

    #[test]
    fn block_multiple_statements() {
        let source = SourceFile::new("tests".to_string(), r#"{ let x = 1; let y = 2; }"#);
        let mut parser = crate::Parser::new(&source);

        let block = parser.parse_block().unwrap();

        assert_eq!(block.span, span!(0, 25));

        let StatementKind::Let { value: value_1, .. } = &block.statements[0].kind else {
            panic!();
        };
        let value_1 = parser.ast.get_expression(&value_1);
        assert_eq!(value_1, &Expression {
            kind: ExpressionKind::Literal(Literal {
                kind: LiteralKind::Int(1),
                span: span!(10, 11)
            }),
            span: span!(10, 11)
        });

        let StatementKind::Let { value: value_2, .. } = &block.statements[1].kind else {
            panic!();
        };
        let value_2 = parser.ast.get_expression(&value_2);
        assert_eq!(value_2, &Expression {
            kind: ExpressionKind::Literal(Literal {
                kind: LiteralKind::Int(2),
                span: span!(21, 22)
            }),
            span: span!(21, 22)
        });
    }
}
