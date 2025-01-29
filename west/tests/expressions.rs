mod common;

#[test]
fn fn_call() {
    expect_output_for! {
        source: r#"
            fn foo() {
                print 1.0 + 2.0;
            }

            fn main() {
                foo();
            }
        "#,
        expected: "3\n",
    };
}

#[test]
fn fn_call_order() {
    expect_output_for! {
        source: r#"
            fn main() {
                foo();
            }

            fn foo() {
                print 1.0 + 2.0;
            }
        "#,
        expected: "3\n",
    };
}
