/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

mod util;

#[test_log::test]
fn test_enum_pattern_simple_payload() {
    util::should_work(
        r"
        enum Option {
            Some Int,
            None,
        }

        fn test_simple_enum_pattern() -> Int {
            opt = Option::Some(42)
            match opt {
                Some value -> value,
                None -> 0,
            }
        }

        result = test_simple_enum_pattern()
        print('result:{result}')
        ",
    );
}

#[test_log::test]
fn test_enum_pattern_struct_destructuring() {
    util::should_work(
        r"
        enum Point {
            TwoD { x: Int, y: Int },
            ThreeD { x: Int, y: Int, z: Int },
        }

        fn test_struct_destructuring() -> Int {
            point = Point::TwoD { x: 10, y: 20 }
            match point {
                TwoD { x, y } -> x + y,
                ThreeD { x, y, z } -> x + y + z,
            }
        }

        result = test_struct_destructuring()
        print('result:{result}')
        ",
    );
}

#[test_log::test]
fn test_enum_pattern_tuple_destructuring() {
    util::should_work(
        r"
        enum Result {
            Ok(Int),
            Err(String),
        }

        fn test_tuple_destructuring() -> Int {
            result = Result::Ok(42)
            match result {
                Ok value -> value,
                Err err -> 0,
            }
        }

        result = test_tuple_destructuring()
        print('result:{result}')
        ",
    );
}

#[test_log::test]
fn test_enum_pattern_no_payload() {
    util::should_work(
        r"
        enum Status {
            Active,
            Inactive,
        }

        fn test_no_payload() -> Int {
            status = Status::Active
            match status {
                Active -> 1,
                Inactive -> 0,
            }
        }

        result = test_no_payload()
        print('result:{result}')
        ",
    );
}
