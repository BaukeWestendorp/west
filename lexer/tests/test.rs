use lexer::{
    lex,
    token::{Token, TokenKind},
};

fn check_lexer(src: &str, expect: Vec<Token>) {
    let actual = lex(src).collect::<Vec<_>>();
    assert_eq!(actual, expect);
}

#[test]
fn smoke_test() {
    #[rustfmt::skip]
    check_lexer(r#"fn main() {}"#, vec![
        Token { kind: TokenKind::Ident, len: 2 },
        Token { kind: TokenKind::Whitespace, len: 1 },
        Token { kind: TokenKind::Ident, len: 4 },
        Token { kind: TokenKind::OpenParen, len: 1 },
        Token { kind: TokenKind::CloseParen, len: 1 },
        Token { kind: TokenKind::Whitespace, len: 1 },
        Token { kind: TokenKind::OpenBrace, len: 1 },
        Token { kind: TokenKind::CloseBrace, len: 1 },
    ]);
}

#[test]
fn test_empty_input() {
    check_lexer("", vec![]);
}

#[test]
fn test_whitespace() {
    #[rustfmt::skip]
    check_lexer(
        "\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}",
        vec![
            Token { kind: TokenKind::Whitespace, len: 20 }
        ],
    );
}

#[test]
#[rustfmt::skip]
fn test_ident() {
    check_lexer("a",                     vec![Token { kind: TokenKind::Ident, len: 1  }]);
    check_lexer("_",                     vec![Token { kind: TokenKind::Ident, len: 1  }]);
    check_lexer("aa",                    vec![Token { kind: TokenKind::Ident, len: 2  }]);
    check_lexer("__",                    vec![Token { kind: TokenKind::Ident, len: 2  }]);
    check_lexer("_a",                    vec![Token { kind: TokenKind::Ident, len: 2  }]);
    check_lexer("a_",                    vec![Token { kind: TokenKind::Ident, len: 2  }]);
    check_lexer("long_ident_right_here", vec![Token { kind: TokenKind::Ident, len: 21 }]);
}

#[test]
#[rustfmt::skip]
fn test_symbols() {
    check_lexer(";", vec![Token { kind: TokenKind::Semi, len: 1 }]);
    check_lexer("(", vec![Token { kind: TokenKind::OpenParen, len: 1 }]);
    check_lexer(")", vec![Token { kind: TokenKind::CloseParen, len: 1 }]);
    check_lexer("{", vec![Token { kind: TokenKind::OpenBrace, len: 1 }]);
    check_lexer("}", vec![Token { kind: TokenKind::CloseBrace, len: 1 }]);
}
