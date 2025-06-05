/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_assembly;

#[test_log::test]
fn guard() {
    exec_with_assembly(
        "

a = 23
b = 49

result =
  | a > 30 && b < 22 -> 1
  | b > 48 -> 2
  | _ -> 99

        ",
        "
> 0000: enter 5C

> 0001: ld32 $0000 00000017     ; a = 23
> 0002: ld32 $0004 00000031     ; b = 49

> 0003: ld32 $0160 0000001E     ; 30
> 0004: sgt32 $0000 $0160       ; a > 30
> 0005: bnz @8                  ; &&
> 0006: ld32 $0168 00000016     ; 22
> 0007: slt32 $0004 $0168       ; b < 22
> 0008: bnz @B                  ; skip if not equal
    > 0009: ld32 $0008 00000001     ; match_expr = 1
    > 000A: jmp @11                 ; jump to end


> 000B: ld32 $0170 00000030     ; 48
> 000C: sgt32 $0004 $0170       ; b > 48
> 000D: bnz @10                 ; skip if not equal
    > 000E: ld32 $0008 00000002     ; match_expr = 2
    > 000F: jmp @11                 ; jump to end

> 0010: ld32 $0008 00000063     ; match_expr = 99
> 0011: hlt                     ; back to host
",
        "
00000000  17 00 00 00 31 00 00 00  02 00 00 00 00 00 00 00  ....1...........

    ",
    );
}
