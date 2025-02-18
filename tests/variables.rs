mod common;

#[test]
fn print_stmt() {
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

#[test]
fn scope() {
    expect_output_for! {
        source: r#"
            fn main {
                let x = 1;
                if true {
                    print x;
                }
            }
        "#,
        expected: "1\n",
    };
}
