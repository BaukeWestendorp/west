use crate::expect_output_for;

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

#[test]
fn comparison() {
    expect_output_for! {
        source: r#"
            fn main() {
                print 1.0 < 2.0;
                print 1.0 > 2.0;
                print 1.0 <= 2.0;
                print 1.0 <= 1.0;
                print 1.0 >= 1.0;
                print 1.0 >= 2.0;
                print 1.0 == 1.0;
                print 1.0 == 2.0;
                print 1.0 != 2.0;
                print 1.0 != 1.0;
            }
        "#,
        expected: "true\nfalse\ntrue\ntrue\ntrue\nfalse\ntrue\nfalse\ntrue\nfalse\n",
    };
}
