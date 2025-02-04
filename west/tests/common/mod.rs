#[macro_export]
macro_rules! expect_output_for {
    {
        source: $src:expr,
        expected: $expected:expr,
    } => {{
        let source = fout::source::SourceFile::new("tests".to_string(), $src);
        let ast = parser::Parser::new(&source).parse().expect("should parse file");
        typechecker::Typechecker::new(&ast, &source).check().expect("should typecheck");
        let mut compiler = compiler::Compiler::new(&ast);
        let mut bytecode_modules = compiler.compile();
        let mut out: Vec<u8> = Vec::new();
        vm::Vm::new(bytecode_modules.remove(0), &mut out).run();
        let result = String::from_utf8(out).expect("should be utf-8");
        assert_eq!(result, $expected);
    }};
}
