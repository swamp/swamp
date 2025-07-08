/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::{should_fail, should_hint, should_work};

mod util;

#[test_log::test]
fn fail_different_types_assignment() {
    should_fail(
        r"
        struct A {
        }

        struct B {
        }

        fn give_out_vec() -> Vec<B> {
            []
        }

        a: Vec<A; 10> = give_out_vec()

    ",
        "incompatible types",
    );
}


#[test_log::test]
fn not_allowed_with_ending_optional_chaining() {
    should_fail(
        r"
        a: Int? = 3

        b = a?

    ",
        "invalid operator after optional chaining (?)",
    );
}

#[test_log::test]
fn fail_not_storage_in_struct() {
    should_hint(
        r#"
        struct Atest {
            name: String, // this is not storage, can not save it here
        }

        a = Atest { name: "hello" }

    "#,
        "storage needed",
    );
}

#[test_log::test]
fn bytes() {
    should_work(
        r#"
        struct Atest {
            name: String<42>,
        }

        a = Atest { name: "hello" }

        print('a:{a}')

    "#,
    );
}
