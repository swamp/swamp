/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_assembly;

#[test_log::test]
fn while_loop() {
    exec_with_assembly(
        "
        mut a = 1
        while a < 32767 {
            a += 1
        }
        ",
        "
> 0000: enter 54
> 0001: ld32 $0000 00000001
> 0002: ld32 $0158 00007FFF
> 0003: slt32 $0000 $0158
> 0004: bnz @8
> 0005: ld32 $015C 00000001
> 0006: sadd32 $0000 $0000 $015C
> 0007: jmp @2
> 0008: hlt
        ",
        "
00000000  FF 7F 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}
