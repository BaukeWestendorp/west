use ast::{Block, Expression, File, Fn, Ident, Item, Literal, Statement};
use parser::{Parser, session::ParserSession};

fn new_parser(source: &str) -> Parser {
    let session = ParserSession::new(source);
    parser::Parser::new(session)
}

#[test]
fn file_main_fn() {
    let actual = new_parser(r#"fn main() {}"#).parse().unwrap();
    let expected =
        File { items: vec![Item::Fn(Fn { name: Ident("main"), params: (), body: Block {} })] };
    assert_eq!(actual, expected);
}

#[test]
fn empty_file() {
    let actual = new_parser("").parse().unwrap();
    let expected = File { items: vec![] };
    assert_eq!(actual, expected);
}

#[test]
fn file_multiple_items() {
    let actual = new_parser(
        r#"
            fn a() {}
            fn b() {}
        "#,
    )
    .parse()
    .unwrap();
    let expected = File {
        items: vec![
            Item::Fn(Fn { name: Ident("a"), params: (), body: Block {} }),
            Item::Fn(Fn { name: Ident("b"), params: (), body: Block {} }),
        ],
    };
    assert_eq!(actual, expected);
}

#[test]
fn file_invalid_first_item() {
    let actual = new_parser("1.0; fn a() {}").parse().unwrap_err();
    let expected = miette::miette!("expected item, found float: '1.0'");

    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn file_invalid_last_item() {
    let actual = new_parser("fn a() {} 1.0").parse().unwrap_err();
    let expected = miette::miette!("expected item, found float: '1.0'");

    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn file_invalid_root() {
    let actual = new_parser("1.0").parse().unwrap_err();
    let expected = miette::miette!("expected item, found float: '1.0'");

    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn fn_item() {
    let actual = new_parser(r#"fn a() {}"#).parse_item().unwrap().unwrap();
    let expected = Item::Fn(Fn { name: Ident("a"), params: (), body: Block {} });
    assert_eq!(actual, expected);
}

#[test]
fn fn_item_unexpected_eof() {
    let actual = new_parser(r#"fn a() {"#).parse_item().unwrap_err();
    let expected = miette::miette!("unexpected EOF");
    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn fn_long_name() {
    let actual = new_parser(r#"fn a_very_long_name_here() {}"#).parse_item().unwrap().unwrap();
    let expected =
        Item::Fn(Fn { name: Ident("a_very_long_name_here"), params: (), body: Block {} });
    assert_eq!(actual, expected);
}

#[test]
fn block_empty() {
    let actual = new_parser("{}").parse_block().unwrap();
    let expected = Block {};
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
    let expected = Block {};
    assert_eq!(actual, expected);
}

#[test]
fn block_multiple_statements() {
    let actual = new_parser("{ 1; 2; }").parse_block().unwrap();
    let expected = Block {};
    assert_eq!(actual, expected);
}

#[test]
fn statement() {
    let actual = new_parser("1;").parse_statement().unwrap();
    let expected = Statement::Expression(Expression::Literal(Literal::Int(1)));
    assert_eq!(actual, expected);
}

#[test]
fn statement_no_expression() {
    let actual = new_parser(";").parse_statement().unwrap_err();
    let expected = miette::miette!("no expression found before ';'");
    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn statement_no_semi() {
    let actual = new_parser("1").parse_statement().unwrap_err();
    let expected = miette::miette!("unexpected EOF");
    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn expression_literal_int() {
    let actual = new_parser("1").parse_expression().unwrap();
    let expected = Expression::Literal(Literal::Int(1));
    assert_eq!(actual, expected);
}

#[test]
fn expression_literal_str() {
    let actual = new_parser(r#""hello""#).parse_expression().unwrap();
    let expected = Expression::Literal(Literal::Str("hello"));
    assert_eq!(actual, expected);
}

#[test]
fn expression_ident() {
    let actual = new_parser("a").parse_expression().unwrap();
    let expected = Expression::Ident(Ident("a"));
    assert_eq!(actual, expected);
}

#[test]
fn literal_int() {
    let test_int = |input: &str, expected: i64| {
        let actual = new_parser(input).parse_literal().unwrap();
        let expected = Literal::Int(expected);
        assert_eq!(actual, expected);
    };

    test_int("1", 1);
    test_int("0", 0);
    test_int("1234567890", 1234567890);
    test_int("123456", 123456);
    test_int("123456789", 123456789);
    test_int("123456789", 123456789);
}

#[test]
fn literal_float() {
    let test_float = |input: &str, expected: f64| {
        let actual = new_parser(input).parse_literal().unwrap();
        let expected = Literal::Float(expected);
        assert_eq!(actual, expected);
    };

    test_float("1.0", 1.0);
    test_float("0.0", 0.0);
    test_float("1234567890.0", 1234567890.0);
    test_float("123456.0", 123456.0);
    test_float("123456789.0", 123456789.0);
    test_float("123456789.0", 123456789.0);
    test_float("123.456", 123.456);
    test_float("123.456789", 123.456789);
}

#[test]
fn literal_str() {
    let actual = new_parser(r#""hello""#).parse_literal().unwrap();
    let expected = Literal::Str("hello");
    assert_eq!(actual, expected);
}

#[test]
fn literal_bool() {
    let actual = new_parser("true").parse_literal().unwrap();
    let expected = Literal::Bool(true);
    assert_eq!(actual, expected);

    let actual = new_parser("false").parse_literal().unwrap();
    let expected = Literal::Bool(false);
    assert_eq!(actual, expected);
}

#[test]
fn literal_invalid() {
    let actual = new_parser("hello").parse_literal().unwrap_err();
    let expected = miette::miette!("expected literal, found ident: 'hello'");
    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn ident() {
    let actual = new_parser("a").parse_ident().unwrap();
    let expected = Ident("a");
    assert_eq!(actual, expected);
}

#[test]
fn ident_invalid() {
    let actual = new_parser("1").parse_ident().unwrap_err();
    let expected = miette::miette!("expected ident, found int");
    assert_eq!(actual.to_string(), expected.to_string());
}
