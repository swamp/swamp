/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_test::util::exec_show_constants;
use swamp_vm_test::util::exec_with_assembly;

#[test]
fn variable_definition() {
    exec_with_assembly(
        "a = 3",
        "
> 0000: enter 54
> 0001: ld32 $0000 00000003
> 0002: hlt ",
        "
00000000  03 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test]
fn var_def_add() {
    exec_with_assembly(
        "a = 3 + 7",
        "
> 0000: enter 54
> 0001: ld32 $0154 00000003
> 0002: ld32 $0158 00000007
> 0003: sadd32 $0000 $0154 $0158
> 0004: hlt ",
        "
00000000  0A 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test]
fn var_def_if() {
    exec_with_assembly(
        "a = if 3 < 7 { 128 } else { 23 }",
        "
> 0000: enter 54
> 0001: ld32 $0158 00000003
> 0002: ld32 $015C 00000007
> 0003: slt32 $0158 $015C
> 0004: bnz @7
> 0005: ld32 $0000 00000080
> 0006: jmp @8
> 0007: ld32 $0000 00000017
> 0008: hlt ",
        "
00000000  80 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn call() {
    exec_with_assembly(
        "
        fn another_fn(a: Int, b: Int) -> Int {
            a+b
        }
        a = 20
        result = another_fn(a, 10)
        ",
        "
> 0000: enter 58 ; variables:
    ;  $0000:4 a
    ;  $0004:4 result

> 0001: ld32 $0000 00000014 ; int literal
> 0002: mov $005C $0000 4 ; variable access 'a' ()
> 0003: ld32 $0060 0000000A ; int literal
> 0004: call @7 ; calling another_fn (frame size: frame size: 0058)
> 0005: mov $0004 $0058 4 ; copy the ret value to destination
> 0006: hlt


- another_fn -
> 0007: enter 5C ; variables:
    ; $0004:4 a
    ; $0008:4 b
> 0008: sadd32 $0000 $0004 $0008 ; i32 add
> 0009: ret
    ",
        "
00000000  14 00 00 00 1E 00 00 00  14 00 00 00 00 00 00 00  ................ ; a = 14 00 00 00 (20),  1E 00 00 00 (20+10=30)
        ",
    );
}

#[test_log::test]
fn call_exec_with_assembly() {
    exec_with_assembly(
        "
        fn another_fn(a: Int, b: Int) -> Int {
            a+b
        }
        a = 20
        result = another_fn(a, 10)
        ",
        "
> 0000: enter 58
> 0001: ld32 $0000 00000014
> 0002: mov $005C $0000 4
> 0003: ld32 $0060 0000000A
> 0004: call @7
> 0005: mov $0004 $0058 4
> 0006: hlt
- another_fn -
> 0007: enter 5C
> 0008: sadd32 $0000 $0004 $0008
> 0009: ret ",
        "
00000000  14 00 00 00 1E 00 00 00  14 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn call_mut_argument() {
    exec_with_assembly(
        "
        fn add_and_overwrite(a: Int, mut b: Int) {
            b = a+b
        }

        a = 20
        mut b = 30
        add_and_overwrite(a, &b)
        ",
        "
> 0000: enter 58
> 0001: ld32 $0000 00000014
> 0002: ld32 $0004 0000001E
> 0003: mov $0058 $0000 4
> 0004: mov $005C $0004 4
> 0005: call @8
> 0006: mov $0004 $005C 4
> 0007: hlt
- add_and_overwrite -
> 0008: enter 58
> 0009: sadd32 $0004 $0000 $0004
> 000A: ret ",
        "
00000000  14 00 00 00 32 00 00 00  00 00 00 00 00 00 00 00  ....2...........
    ",
    );
}

#[test_log::test]
fn call_struct_function() {
    exec_with_assembly(
        "
        struct Something {
            a: Int,
            b: Int,
        }

        fn add(mut s: Something, v: Int) {
            s.b += v
        }

        mut s = Something { a: 10, b: 20 }
        add(&s, 40)
        ",
        "
> 0000: enter 58
> 0001: ld32 $0000 0000000A
> 0002: ld32 $0004 00000014
> 0003: mov $0058 $0000 8   ; copy whole Something (it is mutable)
> 0004: ld32 $0060 00000028
> 0005: call @8
> 0006: mov $0000 $0058 8   ; copy back whole Something (it is probably been mutated in add())
> 0007: hlt
- add -
> 0008: enter 5C
> 0009: sadd32 $0004 $0004 $0008 ; s.b += 40
> 000A: ret ",
        "
00000000  0A 00 00 00 3C 00 00 00  28 00 00 00 00 00 00 00  ....<...(.......
    ",
    );
}

#[test_log::test]
fn call_associated_function() {
    exec_with_assembly(
        "
struct Something {
    a: Int,
    b: Bool,
}

impl Something {
    fn own_add(mut self, b: Int) {
        self.a += b
    }
}

mut s = Something { a: 10, b: false }
s.own_add(40)
        ",
        "
> 0000: enter 58
> 0001: ld32 $0000 0000000A
> 0002: ld8 $0004 00
> 0003: mov $0058 $0000 8
> 0004: ld32 $0060 00000028
> 0005: call @8
> 0006: mov $0000 $0058 8
> 0007: hlt
- own_add -
> 0008: enter 5C
> 0009: sadd32 $0000 $0000 $0008
> 000A: ret ",
        "
00000000  32 00 00 00 00 00 00 00  28 00 00 00 00 00 00 00  2.......(.......
    ",
    );
}

#[test_log::test]
fn enum_literal() {
    exec_with_assembly(
        "
enum Something {
    First,
    Second,
}

a = Something::Second
        ",
        "
> 0000: enter 51
> 0001: ld8 $0000 01
> 0002: hlt ",
        "
00000000  01 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn enum_literal_tuple_one() {
    exec_with_assembly(
        "
enum Something {
    First,
    Second(Int),
}

a = Something::First
        ",
        "
> 0000: enter 58
> 0001: ld8 $0000 00
> 0002: hlt ",
        "
00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn enum_literal_tuple_second() {
    exec_with_assembly(
        "
enum Something {
    First,
    Second(Int),
}

a = Something::Second(42)
        ",
        "
> 0000: enter 58
> 0001: ld8 $0000 01
> 0002: ld32 $0004 0000002A
> 0003: hlt ",
        "
00000000  01 00 00 00 2A 00 00 00  00 00 00 00 00 00 00 00  ....*...........
    ",
    );
}

#[test_log::test]
fn enum_literal_struct() {
    exec_with_assembly(
        "
enum Something {
    First,
    Second { x: Int, y: Float },
}

a = Something::Second {
    x: 10, y: 2.4,
     }
        ",
        "
> 0000: enter 5C
> 0001: ld8 $0000 01
> 0002: ld32 $0004 0000000A
> 0003: ld32 $0008 00026666
> 0004: hlt ",
        "
00000000  01 00 00 00 0A 00 00 00  66 66 02 00 00 00 00 00  ........ff......
    ",
    );
}

#[test_log::test]
fn enum_literal_both() {
    exec_with_assembly(
        "
enum Something {
    First,
    Second { x: Int, y: Float },
    Third(Int, Int, Int, Int, Float)
}

a = Something::Second {
    x: 10, y: 2.4,
     }
        ",
        "
> 0000: enter 68
> 0001: ld8 $0000 01
> 0002: ld32 $0004 0000000A
> 0003: ld32 $0008 00026666
> 0004: hlt
        ",
        "
00000000  01 00 00 00 0A 00 00 00  66 66 02 00 00 00 00 00  ........ff......
    ",
    );
}

#[test_log::test]
fn enum_literal_both_third() {
    exec_with_assembly(
        "
enum Something {
    First,
    Second { x: Int, y: Float },
    Third(Int, Int, Int, Int, Float)
}

a = Something::Third(1,2,3,4,7.6)
        ",
        "
> 0000: enter 68
> 0001: ld8 $0000 02 ; enum tag Third
> 0002: ld32 $0004 00000001
> 0003: ld32 $0008 00000002
> 0004: ld32 $000C 00000003
> 0005: ld32 $0010 00000004
> 0006: ld32 $0014 00079999 ; 7.6
> 0007: hlt
        ",
        "
00000000  02 00 00 00 01 00 00 00  02 00 00 00 03 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn enum_literal_both_third_sub_struct() {
    exec_with_assembly(
        "
struct SubStruct {
  x: Int,
   y: Int,
}

enum Something {
    First,
    Second(SubStruct),
    Third(Int, Int, Int, Float)
}

a = Something::Second(SubStruct{x:99, y:128})
        ",
        "
> 0000: enter 64
> 0001: ld8 $0000 01 ; enum tag
> 0002: ld32 $0004 00000063 ; x: 99
> 0003: ld32 $0008 00000080 ; y: 128
> 0004: hlt
        ",
        "
00000000  01 00 00 00 63 00 00 00  80 00 00 00 00 00 00 00  ....c...........
    ",
    );
}

#[test_log::test]
fn string_literal() {
    exec_show_constants(
        r#"

a = "hello"
        "#,
        "
00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "
00000000  68 65 6C 6C 6F 00 00 00  00 00 00 00 00 00 00 00  hello...........
    ",
    );
}

#[test_log::test]
fn string_literal_return() {
    exec_show_constants(
        r#"
fn get_string() -> String {
    "hello"
}
a = get_string()
        "#,
        "
00000000  08 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "
00000000  68 65 6C 6C 6F 00 00 00  00 00 00 00 00 00 00 00  hello...........
    ",
    );
}

#[test_log::test]
fn vec() {
    exec_with_assembly(
        "
result = [10, 20, 3]
b = 0x0BAD_CAFE
        ",
        "
> 0000: enter 58
; $0000:4 `result`
; $0004:4 `b`

> 0001: ld32 $0158 0000000A
> 0002: ld32 $015C 00000014
> 0003: ld32 $0160 00000003
> 0004: vec_from_slice $0000 $0158 4 0003
> 0005: ld32 $0004 0BADCAFE
> 0006: hlt ",
        "
00000000  10 00 00 00 FE CA AD 0B  00 00 00 00 00 00 00 00  ................

    ",
    );
}

//#[test_log::test]
fn map() {
    exec_with_assembly(
        "
struct Something {
    is_cool: Bool,
    f: Float,
}

result = [
    10 : Something { is_cool: true, f: 42.0 },
    20: Something { is_cool: false, f: 99.0 },
    3: Something { is_cool: true, f: 0.33 }
]

b = 0x0BAD_CAFE
        ",
        "
> 0000: enter 58
> 0001: ld32 $0158 0000000A ; setup slice pair from $0158
> 0002: ld8 $015C 01
> 0003: ld32 $0160 002A0000
> 0004: ld32 $0164 00000014
> 0005: ld8 $0168 00
> 0006: ld32 $016C 00630000
> 0007: ld32 $0170 00000003
> 0008: ld8 $0174 01
> 0009: ld32 $0178 0000547A
> 000A: map_new_from_pairs $0000 $0158 4 8 0003 ; $0158 is slice pair, 4:key size, 8:value size (Something), 0003 is the count
> 000B: ld32 $0004 0BADCAFE
> 000C: hlt
",
        "
00000000  03 00 08 00 D0 00 04 00  08 00 00 00 FE CA AD 0B  ................

    ",
        // 03 00 - len() of map
        // 08 00 - number of buckets
        // 00 0D - bucket pointer
        // 04 00 - key size
        // 08 00 - value size
        // | 0              | length       | u16  | 2            | Number of entries in the map     |
        // | 2              | capacity     | u16  | 2            | Total bucket count (power of 2)  |
        // | 4              | buckets_ptr  | u16  | 2            | Pointer to bucket array          |
        // | 6              | key_size     | u16  | 2            | Key size in bytes                |
        // | 8              | value_size   | u16  | 2            | Value size in bytes              |
    );
}

//#[test_log::test]
fn map_remove() {
    exec_with_assembly(
        "
struct Something {
    is_cool: Bool,
    f: Float,
}

mut result = [
    10 : Something { is_cool: true, f: 42.0 },
    20: Something { is_cool: false, f: 99.0 },
    3: Something { is_cool: true, f: 0.33 }
]

result.remove(10)

b = 0x0BAD_CAFE
        ",
        "
> 0000: enter 58
> 0001: ld32 $0158 0000000A
> 0002: ld8 $015C 01
> 0003: ld32 $0160 002A0000
> 0004: ld32 $0164 00000014
> 0005: ld8 $0168 00
> 0006: ld32 $016C 00630000
> 0007: ld32 $0170 00000003
> 0008: ld8 $0174 01
> 0009: ld32 $0178 0000547A
> 000A: map_new_from_pairs $0000 $0158 4 8 0003
> 000B: ld32 $017C 0000000A
> 000C: map_remove $0000 $017C ; remove from map from the key at addr $017C (10)
> 000D: ld32 $0004 0BADCAFE
> 000E: hlt
        ",
        "
00000000  02 00 08 00 D0 00 04 00  08 00 00 00 FE CA AD 0B  ................

    ",
        // 03 00 - len() of map
        // 08 00 - number of buckets
        // 00 0D - bucket pointer
        // 04 00 - key size
        // 08 00 - value size
        // | 0              | length       | u16  | 2            | Number of entries in the map     |
        // | 2              | capacity     | u16  | 2            | Total bucket count (power of 2)  |
        // | 4              | buckets_ptr  | u16  | 2            | Pointer to bucket array          |
        // | 6              | key_size     | u16  | 2            | Key size in bytes                |
        // | 8              | value_size   | u16  | 2            | Value size in bytes              |
    );
}

#[test_log::test]
fn tuple() {
    exec_with_assembly(
        "

result = (10, true, 0.2)

        ",
        "
> 0000: enter 5C
> 0001: ld32 $0000 0000000A     ; int 10
> 0002: ld8 $0004 01            ; the Bool is aligned to 4 bytes
> 0003: ld32 $0008 00003333     ; the 0.2 float value 13107 (13107 / 65536)
> 0004: hlt                     ; return to host
",
        "
00000000  0A 00 00 00 01 00 00 00  33 33 00 00 00 00 00 00  ........33...... ; 0A 00 00 00 is the int, 01 00 00 00 is the bool, and 33 33 00 00 is the float

    ",
    );
}
