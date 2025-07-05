/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::{exec_with_assembly, exec_with_host_function};

#[test_log::test]
fn vec_iter() {
    exec_with_assembly(
        "

a = [8, 16, 24, 96, 32]

for i in a {
    b = 99
}
        ",
        "
> 0000: enter 5C
> 0001: ld32 $015C 00000008
> 0002: ld32 $0160 00000010
> 0003: ld32 $0164 00000018
> 0004: ld32 $0168 00000060
> 0005: ld32 $016C 00000020
> 0006: vec_from_slice $0000 $015C 4 0005
> 0007: vec_iter_init $0170 ($0000)
> 0008: vec_iter_next $0170 $0004 @B
> 0009: ld32 $0008 00000063
> 000A: jmp @8
> 000B: hlt
",
        "
00000000  18 00 00 00 20 00 00 00  63 00 00 00 00 00 00 00  .... ...c....... ; E8 00 00 00 is the allocated header pointer

    ",
    );
}

#[test_log::test]
fn vec_iter_print() {
    exec_with_host_function(
        "

external fn print(i: Int)

a = [8, 16, 24, 96, 32]

for i in a {
    print(i)
}

        ",
        "
> 0000: enter 58
> 0001: ld32 $0158 00000008
> 0002: ld32 $015C 00000010
> 0003: ld32 $0160 00000018
> 0004: ld32 $0164 00000060
> 0005: ld32 $0168 00000020
> 0006: vec_from_slice $0000 $0158 4 0005
> 0007: vec_iter_init $016C ($0000)
> 0008: vec_iter_next $016C $0004 @C
> 0009: mov $0058 $0004 4
> 000A: host 0001 4
> 000B: jmp @8
> 000C: hlt
",
        "
00000000  18 00 00 00 20 00 00 00  00 00 00 00 00 00 00 00  .... ...........

    ",
        "print",
        |args| {
            let output = args.register_i32(1);
            eprintln!("print({output})");
        },
    );
}
