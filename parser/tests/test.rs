#[test]
fn test_parser() {
    let source = r#"fn main() {}"#;
    let session = parser::session::ParserSession { source };
    let parser = parser::Parser::new(session);

    assert_eq!(parser.parse().unwrap(), vec![ast::Fn {
        name: ast::Ident("main"),
        params: (),
        body: ()
    }]);
}
