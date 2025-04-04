/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::{check_fail, check_value};
use swamp_core_extra::prelude::Value;

#[test_log::test]
fn basic_eval_2() {
    check_value(
        r"
        mut a = 3
        a = a + 1
        a
    ",
        Value::Int(4),
    );
}

#[test_log::test]
fn basic_eval_fail() {
    check_fail(
        r"
        a = 3
        a = a + 1 // This should fail since `a` is not mutable
        a
    ",
        r"
Error(Error { node: <23:1>, kind: VariableIsNotMutable })
",
    );
}
