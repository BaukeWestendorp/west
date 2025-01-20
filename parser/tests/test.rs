#[test]
fn smoke_test() {
    let source = r#"fn main() {}"#;
    let session = parser::session::ParserSession { source };
    let parser = parser::Parser::new(session);

    assert_eq!(parser.parse().unwrap(), ast::File {
        items: vec![ast::Item::Fn(ast::Fn {
            name: ast::Ident("main"),
            params: (),
            body: ast::Block {}
        })]
    });
}
