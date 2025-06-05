/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_assembly;

#[test_log::test]
fn optional_none() {
    exec_with_assembly(
        "

result: Int? = none

        ",
        "
> 0000: enter 58
> 0001: ld8 $0000 00 ; 00 is `none`, no need to fill out the int portion
> 0002: hlt
",
        "
00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}

#[test_log::test]
fn optional_int() {
    exec_with_assembly(
        "

result: Int? = 128

        ",
        "
> 0000: enter 58
> 0001: ld8 $0000 01 ; `01` is the `Some` tag, that a value exists
> 0002: ld32 $0004 00000080 ; hex `80` is 128. it should be at $0004 since it must be aligned to 4 bytes.
> 0003: hlt

",
        "
00000000  01 00 00 00 80 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}

#[test_log::test]
fn optional_struct() {
    exec_with_assembly(
        "

struct Something {
    x: Int,
    y: Bool,
    z: Float,
}

result: Something? = Something {
    x: 10,
    y: true,
    z: 32767.0,
}

        ",
        "
> 0000: enter 60
> 0001: ld8 $0000 01 ; some tag
> 0002: ld32 $0004 0000000A ; int 10
> 0003: ld8 $0008 01 ; `true`
> 0004: ld32 $000C 7FFF0000 ; float 32767.0
> 0005: hlt

",
        "
00000000  01 00 00 00 0A 00 00 00  01 00 00 00 00 00 FF 7F  ................

    ",
    );
}
