/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::{exec_with_assembly, exec_with_host_function_show_heap};
#[test_log::test]
fn emit_string() {
    exec_with_assembly(
        r#"
result = "hello, world"

        "#,
        "
> 0000: enter 5C
> 0001: str_from_const $0000 @#00000000 C
> 0002: hlt
",
        "
00000000  10 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}

#[test_log::test]
fn string_with_print() {
    exec_with_host_function_show_heap(
        r#"

external fn print(output: String)

print("hello, world!")

        "#,
        "
> 0000: enter 50
> 0001: str_from_const $0050 @#00000000 D
> 0002: host 0001 C
> 0003: hlt
",
        0x0,
        32,
        "
00000000  68 65 6C 6C 6F 2C 20 77  6F 72 6C 64 21 00 00 00  hello, world!...
00000010  00 00 00 00 0D 00 0D 00  00 00 00 00 00 00 00 00  ................

    ",
        "print",
        |args| {
            let output = args.string(1);
            eprintln!("print({output})");
        },
    );
}

#[test_log::test]
fn string_with_emoji_print() {
    exec_with_host_function_show_heap(
        r#"
external fn print(extra_arg: Int, output: String)

print(23, "\u(1F431)if this works it is \u(1F525)\u(1F60E)!")

        "#,
        "
> 0000: enter 50
> 0001: ld32 $0050 00000017
> 0002: str_from_const $0054 @#00000000 21
> 0003: host 0001 10
> 0004: hlt
",
        0,
        32,
        "
00000000  F0 9F 90 B1 69 66 20 74  68 69 73 20 77 6F 72 6B  ....if this work
00000010  73 20 69 74 20 69 73 20  F0 9F 94 A5 F0 9F 98 8E  s it is ........

    ",
        "print",
        |args| {
            let extra_arg = args.register_i32(1);
            assert_eq!(extra_arg, 23);
            let output = args.string(1);
            assert_eq!("\u{1F431}if this works it is \u{1F525}\u{1F60E}!", output);
            eprintln!("print({output})");
        },
    );
}

#[test_log::test]
fn string_append() {
    exec_with_host_function_show_heap(
        r#"
external fn print(extra_arg: Int, output: String)

print(23, "\u(1F431)if this works it is \u(1F525)\u(1F60E)!" + "extra")

        "#,
        "
> 0000: enter 50
> 0001: ld32 $0050 00000017
> 0002: str_from_const $0150 @#00000000 21
> 0003: str_from_const $015C @#00000021 5
> 0004: str_append $0054 $0150 $015C
> 0005: host 0001 10
> 0006: hlt
",
        0x40,
        32,
        "
00000000  F0 9F 90 B1 69 66 20 74  68 69 73 20 77 6F 72 6B  ....if this work
00000010  73 20 69 74 20 69 73 20  F0 9F 94 A5 F0 9F 98 8E  s it is ........

    ",
        "print",
        |args| {
            let extra_arg = args.register_i32(1);
            assert_eq!(extra_arg, 23);
            let output = args.string(1);
            assert_eq!(
                "\u{1F431}if this works it is \u{1F525}\u{1F60E}!extra",
                output
            );
            eprintln!("print({output})");
        },
    );
}

#[test_log::test]
fn string_len() {
    exec_with_assembly(
        r#"

a = "\u(1F431)if this works it is \u(1F525)\u(1F60E)!"
r = a.len()

        "#,
        "
> 0000: enter 60
> 0001: str_from_const $0000 @#00000000 21
> 0002: str_len $000C $0000
> 0003: hlt
",
        "
00000000  28 00 00 00 00 00 00 00  00 00 00 00 21 00 00 00  (...........!...

    ",
    );
}
