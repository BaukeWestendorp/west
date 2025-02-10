use crate::expect_output_for;

#[test]
fn fn_early_return() {
    expect_output_for! {
        source: r#"
            fn foo() {
                return;
                print 42;
            }

            fn main() {
                foo();
            }
        "#,
        expected: "",
    };
}

#[test]
fn fn_return_value() {
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

#[test]
fn fn_parameters() {
    expect_output_for! {
        source: r#"
            fn foo(x: float, y: float): float {
                return x + y;
            }

            fn main {
                print foo(1.0, 2.0);
            }
        "#,
        expected: "3\n",
    };
}
