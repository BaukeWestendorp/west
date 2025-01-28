use lexer::Lexer;
use lexer::token::{Keyword, Literal, Token, TokenKind};
use west_error::source::SourceFile;

fn check_lexer(src: &str, expect: Vec<Token>) {
    let source = SourceFile::new("tests".to_string(), src);
    let actual = Lexer::new(&source).map(Result::unwrap).collect::<Vec<_>>();
    assert_eq!(actual, expect);
}

fn check_lexer_with_errors(src: &str, expect: Vec<Result<Token, String>>) {
    let source = SourceFile::new("tests".to_string(), src);
    let actual = Lexer::new(&source)
        .map(|token| match token {
            Ok(token) => Ok(token),
            Err(err) => Err(err.to_string()),
        })
        .collect::<Vec<_>>();
    assert_eq!(actual, expect);
}

#[test]
#[rustfmt::skip]
fn smoke_test() {
    let source = r#"
        fn main() {

        }
    "#;

    check_lexer(source, vec![
        Token { kind: TokenKind::Keyword(Keyword::Fn), span: 9..11  },
        Token { kind: TokenKind::Ident,                span: 12..16 },
        Token { kind: TokenKind::ParenOpen,            span: 16..17 },
        Token { kind: TokenKind::ParenClose,           span: 17..18 },
        Token { kind: TokenKind::BraceOpen,            span: 19..20 },
        Token { kind: TokenKind::BraceClose,           span: 30..31 },
    ]);
}

#[test]
fn test_empty_input() {
    check_lexer("", vec![]);
}

#[test]
#[rustfmt::skip]
fn test_whitespace() {
    #[rustfmt::skip]
    check_lexer(
        "\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}",
        vec![],
    );
}

#[test]
#[rustfmt::skip]
fn test_symbols() {
    check_lexer("==", vec![Token { kind: TokenKind::EqualsEquals,   span: 0..2 }]);
    check_lexer("&&", vec![Token { kind: TokenKind::AmpAmp,         span: 0..2 }]);
    check_lexer("||", vec![Token { kind: TokenKind::PipePipe,       span: 0..2 }]);
    check_lexer("&=", vec![Token { kind: TokenKind::AmpEquals,      span: 0..2 }]);
    check_lexer("|=", vec![Token { kind: TokenKind::PipeEquals,     span: 0..2 }]);
    check_lexer("<=", vec![Token { kind: TokenKind::LessThanEquals, span: 0..2 }]);
    check_lexer(">=", vec![Token { kind: TokenKind::MoreThanEquals, span: 0..2 }]);
    check_lexer("!=", vec![Token { kind: TokenKind::BangEquals,     span: 0..2 }]);
    check_lexer("+=", vec![Token { kind: TokenKind::PlusEquals,     span: 0..2 }]);
    check_lexer("-=", vec![Token { kind: TokenKind::MinusEquals,    span: 0..2 }]);
    check_lexer("*=", vec![Token { kind: TokenKind::StarEquals,     span: 0..2 }]);
    check_lexer("/=", vec![Token { kind: TokenKind::SlashEquals,    span: 0..2 }]);

    check_lexer("(", vec![Token { kind: TokenKind::ParenOpen,  span: 0..1 }]);
    check_lexer(")", vec![Token { kind: TokenKind::ParenClose, span: 0..1 }]);
    check_lexer("{", vec![Token { kind: TokenKind::BraceOpen,  span: 0..1 }]);
    check_lexer("}", vec![Token { kind: TokenKind::BraceClose, span: 0..1 }]);
    check_lexer(".", vec![Token { kind: TokenKind::Dot,        span: 0..1 }]);
    check_lexer(",", vec![Token { kind: TokenKind::Comma,      span: 0..1 }]);
    check_lexer(":", vec![Token { kind: TokenKind::Colon,      span: 0..1 }]);
    check_lexer(";", vec![Token { kind: TokenKind::Semicolon,  span: 0..1 }]);
    check_lexer("=", vec![Token { kind: TokenKind::Equals,     span: 0..1 }]);
    check_lexer("&", vec![Token { kind: TokenKind::Amp,        span: 0..1 }]);
    check_lexer("|", vec![Token { kind: TokenKind::Pipe,       span: 0..1 }]);
    check_lexer("<", vec![Token { kind: TokenKind::LessThan,   span: 0..1 }]);
    check_lexer(">", vec![Token { kind: TokenKind::MoreThan,   span: 0..1 }]);
    check_lexer("!", vec![Token { kind: TokenKind::Bang,       span: 0..1 }]);
    check_lexer("+", vec![Token { kind: TokenKind::Plus,       span: 0..1 }]);
    check_lexer("-", vec![Token { kind: TokenKind::Minus,      span: 0..1 }]);
    check_lexer("*", vec![Token { kind: TokenKind::Star,       span: 0..1 }]);
    check_lexer("/", vec![Token { kind: TokenKind::Slash,      span: 0..1 }]);
}

#[test]
#[rustfmt::skip]
fn test_ident() {
    check_lexer("a",                     vec![Token { kind: TokenKind::Ident, span: 0..1  }]);
    check_lexer("_",                     vec![Token { kind: TokenKind::Ident, span: 0..1  }]);
    check_lexer("aa",                    vec![Token { kind: TokenKind::Ident, span: 0..2  }]);
    check_lexer("__",                    vec![Token { kind: TokenKind::Ident, span: 0..2  }]);
    check_lexer("_a",                    vec![Token { kind: TokenKind::Ident, span: 0..2  }]);
    check_lexer("a_",                    vec![Token { kind: TokenKind::Ident, span: 0..2  }]);
    check_lexer("long_ident_right_here", vec![Token { kind: TokenKind::Ident, span: 0..21 }]);
}

#[test]
#[rustfmt::skip]
fn test_keywords() {
    check_lexer("fn",    vec![Token { kind: TokenKind::Keyword(Keyword::Fn),    span: 0..2 }]);
    check_lexer("let",   vec![Token { kind: TokenKind::Keyword(Keyword::Let),   span: 0..3 }]);
    check_lexer("print", vec![Token { kind: TokenKind::Keyword(Keyword::Print), span: 0..5 }]);
}

#[test]
#[rustfmt::skip]
fn test_string_literal() {
    check_lexer("\"\"",      vec![Token { kind: TokenKind::Literal(Literal::Str), span: 0..2 }]);
    check_lexer("\"hello\"", vec![Token { kind: TokenKind::Literal(Literal::Str), span: 0..7 }]);
}

#[test]
#[rustfmt::skip]
fn test_integer_literal() {
    check_lexer("0",           vec![Token { kind: TokenKind::Literal(Literal::Int), span: 0..1  }]);
    check_lexer("123",         vec![Token { kind: TokenKind::Literal(Literal::Int), span: 0..3  }]);
    check_lexer("123_456",     vec![Token { kind: TokenKind::Literal(Literal::Int), span: 0..7  }]);
    check_lexer("123_456_789", vec![Token { kind: TokenKind::Literal(Literal::Int), span: 0..11 }]);
    check_lexer("123_456__",   vec![Token { kind: TokenKind::Literal(Literal::Int), span: 0..9  }]);
}

#[test]
#[rustfmt::skip]
fn test_float_literal() {
    check_lexer("0.0",     vec![Token { kind: TokenKind::Literal(Literal::Float), span: 0..3 }]);
    check_lexer("123.456", vec![Token { kind: TokenKind::Literal(Literal::Float), span: 0..7 }]);
    check_lexer(
        "123.456.789",
        vec![
            Token { kind: TokenKind::Literal(Literal::Float), span: 0..7 },
            Token { kind: TokenKind::Dot, span: 7..8 },
            Token { kind: TokenKind::Literal(Literal::Int), span: 8..11 },
        ]
    );
    check_lexer(
        ".123.345",
        vec![
            Token { kind: TokenKind::Dot, span: 0..1 },
            Token { kind: TokenKind::Literal(Literal::Float), span: 1..8 },
        ]
    );
}

#[test]
#[rustfmt::skip]
fn test_bool_literal() {
    check_lexer("true",  vec![Token { kind: TokenKind::Literal(Literal::Bool), span: 0..4 }]);
    check_lexer("false", vec![Token { kind: TokenKind::Literal(Literal::Bool), span: 0..5 }]);
}

#[test]
#[rustfmt::skip]
fn test_invalid_token() {
    check_lexer_with_errors(";()#", vec![
        Ok(Token { kind: TokenKind::Semicolon,  span: 0..1 }),
        Ok(Token { kind: TokenKind::ParenOpen,  span: 1..2 }),
        Ok(Token { kind: TokenKind::ParenClose, span: 2..3 }),
        Err("unknown character: '#'".to_string()),
    ]);
}
