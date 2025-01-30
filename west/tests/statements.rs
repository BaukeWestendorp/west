mod common;

#[test]
fn if_cond_true() {
    expect_output_for! {
        source: r#"
            fn main {
                let cond = true;
                if cond {
                    print 1;
                }
            }
        "#,
        expected: "1\n",
    };
}

#[test]
fn if_cond_false() {
    expect_output_for! {
        source: r#"
            fn main {
                let cond = false;
                if cond {
                    print 1;
                }
            }
        "#,
        expected: "",
    };
}

#[test]
fn if_else_cond_true() {
    expect_output_for! {
        source: r#"
            fn main {
                let cond = true;
                if cond {
                    print 1;
                } else {
                    print 2;
                }
            }
        "#,
        expected: "1\n",
    };
}

#[test]
fn if_else_cond_false() {
    expect_output_for! {
        source: r#"
            fn main {
                let cond = false;
                if cond {
                    print 1;
                } else {
                    print 2;
                }
            }
        "#,
        expected: "2\n",
    };
}
