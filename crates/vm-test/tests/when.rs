/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_assembly;

#[test_log::test]
fn when() {
    exec_with_assembly(
        "

a: Int? = 3

result = when a {
    a + 7
} else {
    -1
}

        ",
        "
> 0000: enter 60

> 0001: ld8 $0000 01                ; set the Option tag to `Some` 'true'
> 0002: ld32 $0004 00000003         ; set the Some part to int 3

> 0003: tst8 $0000                  ; check if it is `Some`
> 0004: bnz @9                      ; if it wasn't jump to else
> 0005: movlp $0008 $0004 4         ; copy overlapping the Some part into the new variable `a`

; true block `a + 7`
    > 0006: ld32 $0160 00000007         ;    7
    > 0007: sadd32 $000C $0008 $0160    ; result = a + 7
> 0008: jmp @B                      ; jump over to end

; false block `-1`
    > 0009: ld32 $0164 00000001         ; temp = 1
    > 000A: sneg32 $000C $0164          ; result = -temp

> 000B: hlt
",
        "
00000000  01 00 00 00 03 00 00 00  03 00 00 00 0A 00 00 00  ................ ; 0A 00 00 00 is the a + 7 ( 3 + 7 )

    ",
    );
}

#[test_log::test]
fn when_else() {
    exec_with_assembly(
        "

a: Int? = none

result = when a {
    a + 7
} else {
    -1
}

        ",
        "
> 0000: enter 60
> 0001: ld8 $0000 00
> 0002: tst8 $0000
> 0003: bnz @8
> 0004: movlp $0008 $0004 4
> 0005: ld32 $0160 00000007
> 0006: sadd32 $000C $0008 $0160
> 0007: jmp @A
> 0008: ld32 $0164 00000001
> 0009: sneg32 $000C $0164
> 000A: hlt
",
        "
00000000  00 00 00 00 00 00 00 00  00 00 00 00 FF FF FF FF  ................ ; FF FF FF FF is the -1 since the value was none

    ",
    );
}
