/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_assembly;

#[test_log::test]
fn match_simple() {
    exec_with_assembly(
        "

enum Test {
    Simple,
    Another,
    Third,
}

a = Test::Another

result = match a {
    Another => 2,
    Third => 3,
    _ => 0,
}
        ",
        "
> 0000: enter 58
> 0001: ld8 $0000 01 ;  a = Test::Another
> 0002: eq8 $0000 01 ; match Another (index 1)
> 0003: bnz @6
> 0004: ld32 $0004 00000002 ; result = 2
> 0005: jmp @B ; exit
> 0006: eq8 $0000 02 ; match Third (index 2)
> 0007: bnz @A ; exit
> 0008: ld32 $0004 00000003
> 0009: jmp @B
> 000A: ld32 $0004 00000000
> 000B: hlt

",
        "
00000000  01 00 00 00 03 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}

#[test_log::test]
fn match_simple_with_guard() {
    exec_with_assembly(
        "

enum Test {
    Simple,
    Another,
    Third,
}

sun_is_shining = false
a = Test::Third

result = match a {
    Another => 2,
    Third | sun_is_shining => 3,
    _ => 255,
}
        ",
        "
> 0000: enter 58 ; variables:
; $0000:1 sun_is_shining
; $0001:1 a
; $0004:4 result
> 0001: ld8 $0000 00 ; bool literal (u8, int:0)
> 0002: ld8 $0001 02 ; enum variant Third tag (u8, int:2)
> 0003: eq8 $0001 01 ; check for enum variant (i32, int:1)
> 0004: bnz @7 ; placeholder for enum match
> 0005: ld32 $0004 00000002 ; int literal (i32, int:2)
> 0006: jmp @E ; jump to exit
> 0007: eq8 $0001 02 ; check for enum variant (i32, int:2)
> 0008: bnz @D ; placeholder for enum match
> 0009: tst8 $0000 ; set P flag for sun_is_shining
> 000A: bnz @D ; placeholder for skip guard
> 000B: ld32 $0004 00000003 ; int literal (i32, int:3)
> 000C: jmp @E ; jump to exit
> 000D: ld32 $0004 000000FF ; int literal (i32, int:0)
> 000E: hlt

",
        "
00000000  00 02 00 00 FF 00 00 00  00 00 00 00 00 00 00 00  ................ ; 00:sun_is_shining, 02: a (Third), 00 00: padding, FF 00 00 00: result (255)

    ",
    );
}
