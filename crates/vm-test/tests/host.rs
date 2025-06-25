/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_host_function;

#[test_log::test]
fn host_call() {
    exec_with_host_function(
        "

external fn some_test(i: Int, b: Int)

some_test(-42, 23)

        ",
        "
> 0000: enter 50
> 0001: ld32 $0150 0000002A
> 0002: sneg32 $0050 $0150
> 0003: ld32 $0054 00000017
> 0004: host 0001 8
> 0005: hlt
",
        "
00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "some_test",
        |args| {
            let i = args.register_i32(1);
            let b = args.register_i32(2);
            eprintln!("you called me i:{i} b:{b}");
        },
    );
}
