/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_host_function;
#[test_log::test]
fn with() {
    exec_with_host_function(
        "

external fn print(i: Int)
x = 99
with x = 3 {
    print(x)
}

        ",
        "
> 0000: enter 58
> 0001: ld32 $0000 00000063
> 0002: ld32 $0004 00000003
> 0003: mov $0058 $0004 4
> 0004: host 0001 4
> 0005: hlt
",
        "
00000000  63 00 00 00 03 00 00 00  00 00 00 00 00 00 00 00  c...............

    ",
        "print",
        |args| {
            let i = args.register_i32(1);
            eprintln!("print({i})");
        },
    );
}
