use parser::Parser;
use typechecker::Typechecker;
use west_error::source::SourceFile;

macro_rules! expect_output_for {
    {
        source: $src:expr,
        expected: $expected:expr,
    } => {{
        let source = SourceFile::new("tests".to_string(), $src);
        let ast = Parser::new(&source).parse().expect("should parse file");
        Typechecker::new(&ast, &source).check().expect("should typecheck");
        // let mut compiler = Compiler::new(&ast);
        // let bytecode_module = compiler.compile().expect("should compile");
        // let mut out: Vec<u8> = Vec::new();
        // let mut vm = vm::Vm::new(&mut out);
        // vm.push_bytecode_module(bytecode_module.clone());
        // vm.run().expect("should run");
        // let result = String::from_utf8(out).expect("should be utf-8");
        // assert_eq!(result, $expected);

        todo!();


    }};
}

#[test]
fn redeclaration() {
    expect_output_for! {
        source: r#"
            fn main() {
                let x = 1.0;
                let x = 2.0;
                print x;
            }
        "#,
        expected: "2",
    };
}
