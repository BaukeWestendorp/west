macro_rules! expect_output_for {
    {
        source: $src:expr,
        expected: $expected:expr,
    } => {{
        let source = west_error::source::SourceFile::new("tests".to_string(), $src);
        let ast = parser::Parser::new(&source).parse().expect("should parse file");
        typechecker::Typechecker::new(&ast, &source).check().expect("should typecheck");
        let mut compiler = compiler::Compiler::new(&ast);
        let bytecode_modules = compiler.compile();
        let mut out: Vec<u8> = Vec::new();
        vm::Vm::new(bytecode_modules, &mut out).run();
        let result = String::from_utf8(out).expect("should be utf-8");
        assert_eq!(result, $expected);
    }};
}

#[test]
fn print_statement() {
    expect_output_for! {
        source: r#"
            fn main() {
                print 1.0 + 2.0;
            }
        "#,
        expected: "3\n",
    };
}

#[test]
fn variable_declaration() {
    expect_output_for! {
        source: r#"
            fn main() {
                let x = 1.0;
                print x;
            }
        "#,
        expected: "1\n",
    };
}

#[test]
fn variable_redeclaration() {
    expect_output_for! {
        source: r#"
            fn main() {
                let x = 1.0;
                let x = 2.0;
                print x;
            }
        "#,
        expected: "2\n",
    };
}
