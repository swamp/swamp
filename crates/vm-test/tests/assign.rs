/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_with_host_function;
#[test_log::test]
fn assignment() {
    exec_with_host_function(
        "

external fn print(i: Int)

struct Inner {
    a: Int,
    f: Float,
}

struct Something {
    i: Int,
    inner: Inner,
}

mut s = Something { i: 10, inner: Inner { a: 32, f: 4.20 } }
print(s.i)
s.inner.f = 42.0
print(s.i)

        ",
        "
> 0000: enter 5C
> 0001: ld32 $0000 0000000A
> 0002: ld32 $0004 00000020
> 0003: ld32 $0008 00043333
> 0004: host 0001 4
> 0005: ld32 $015C 002A0000
> 0006: mov $0008 $015C 4
> 0007: host 0001 4
> 0008: hlt
",
        "
00000000  0A 00 00 00 20 00 00 00  00 00 2A 00 00 00 00 00  .... .....*..... ; 0A 00 00 00 = i:10, 20 00 00 00 = a:32,  00 00 2A 00 = f: 42.0 (2752512 in hex, 2752512/65536 = 42)

    ",
        "print",
        |args| {
            let i = args.register_i32(1);
            eprintln!("print({i})");
        },
    );
}
