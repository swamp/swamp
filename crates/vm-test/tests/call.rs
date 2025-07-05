/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_host_function;
#[test_log::test]
fn fn_call() {
    exec_with_host_function(
        "

external fn print(i: Int)


fn some_func(i: Int) -> Int {
    i * 2
}

result = some_func(10)

        ",
        "
> 0000: enter 54
> 0001: ld32 $0058 0000000A
> 0002: call @5
> 0003: mov $0000 $0054 4
> 0004: hlt
- some_func -
> 0005: enter 58
> 0006: ld32 $0158 00000002
> 0007: smul32 $0000 $0004 $0158
> 0008: ret
",
        "
00000000  14 00 00 00 0A 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "print",
        |args| {
            let i = args.register_i32(1);
            let b = args.register_i32(2);
            eprintln!("you called me i:{i} b:{b}");
        },
    );
}
