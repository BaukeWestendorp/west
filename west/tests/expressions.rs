mod common;

#[test]
fn fn_return() {
    expect_output_for! {
        source: r#"
            fn foo(): float {
                return 1.0 + 2.0;
            }

            fn main() {
                print foo();
            }
        "#,
        expected: "3\n",
    };
}
