use parser::{Parser, session::ParserSession};

fn new_parser(source: &str) -> Parser {
    let session = ParserSession::new(source);
    parser::Parser::new(session)
}

#[test]
fn file_main_fn() {
    let actual = new_parser(r#"fn main() {}"#).parse().unwrap();
    let expected = ast::File {
        items: vec![ast::Item::Fn(ast::Fn {
            name: ast::Ident("main"),
            params: (),
            body: ast::Block {},
        })],
    };
    assert_eq!(actual, expected);
}

#[test]
fn empty_file() {
    let actual = new_parser("").parse().unwrap();
    let expected = ast::File { items: vec![] };
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
    let expected = ast::File {
        items: vec![
            ast::Item::Fn(ast::Fn { name: ast::Ident("a"), params: (), body: ast::Block {} }),
            ast::Item::Fn(ast::Fn { name: ast::Ident("b"), params: (), body: ast::Block {} }),
        ],
    };
    assert_eq!(actual, expected);
}

#[test]
fn file_invalid_first_item() {
    let actual = new_parser("1.0; fn a() {}").parse().unwrap_err();
    let expected = miette::miette!("expected an item, but found a float: '1.0'");

    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn file_invalid_last_item() {
    let actual = new_parser("fn a() {} 1.0").parse().unwrap_err();
    let expected = miette::miette!("expected an item, but found a float: '1.0'");

    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn file_invalid_root() {
    let actual = new_parser("1.0").parse().unwrap_err();
    let expected = miette::miette!("expected an item, but found a float: '1.0'");

    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn fn_item() {
    let actual = new_parser(r#"fn a() {}"#).parse_item().unwrap().unwrap();
    let expected =
        ast::Item::Fn(ast::Fn { name: ast::Ident("a"), params: (), body: ast::Block {} });
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
    let expected = ast::Item::Fn(ast::Fn {
        name: ast::Ident("a_very_long_name_here"),
        params: (),
        body: ast::Block {},
    });
    assert_eq!(actual, expected);
}

#[test]
fn block_empty() {
    let actual = new_parser("{}").parse_block().unwrap();
    let expected = ast::Block {};
    assert_eq!(actual, expected);
}

#[test]
fn block_unexpected_eof() {
    let actual = new_parser("{").parse_block().unwrap_err();
    let expected = miette::miette!("unexpected EOF");
    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn ident() {
    let actual = new_parser("a").parse_ident().unwrap();
    let expected = ast::Ident("a");
    assert_eq!(actual, expected);
}

#[test]
fn ident_invalid() {
    let actual = new_parser("1").parse_ident().unwrap_err();
    let expected = miette::miette!("expected 'ident', found 'int'");
    assert_eq!(actual.to_string(), expected.to_string());
}
