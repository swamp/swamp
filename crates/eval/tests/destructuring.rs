/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::eval;
use swamp_core_extra::prelude::Value;

#[test_log::test]
fn tuple_destructuring() {
    let result = eval(
        "
    x, y = (2, 3)
    y
    ",
    );

    assert_eq!(result, Value::Int(3));
}

#[test_log::test]
fn tuple_destructuring_1() {
    let result = eval(
        "
    x, y = (2, 3)
    x
    ",
    );

    assert_eq!(result, Value::Int(2));
}
