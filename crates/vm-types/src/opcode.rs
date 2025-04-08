/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::fmt::{Display, Formatter};

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Hlt, // Return to the host

    // Operators
    // i32
    AddI32,
    MulI32,
    NegI32,
    SubI32,
    ModI32,

    // Fixed
    AddF32,
    MulF32,
    NegF32,

    // Comparisons
    LtI32,
    LeI32,
    GtI32,
    GeI32,

    // Conditional branching
    Bnz,
    Bz,
    // Unconditional branching
    Jmp,

    // Call, frame and return
    Call, // Introduce CallLong if needed
    Enter,
    Ret,

    // Frame copy
    Mov,
    MovLp,

    // Load immediate into frame
    Ld8,
    Ld16,
    Ld32,

    LdConst, // Load from constant memory

    // Indirect operations (using pointer)
    //St32x,
    //Stx,
    //Ldx,

    // Allocate heap
    //Alloc,
    //     LtU16,
    VecFromSlice,
    VecPush,
    VecIterInit,
    VecIterNext,
    VecIterNextPair,
    VecLen,
    VecSubscript,
    VecSubscriptMut,

    Nop,

    // Intrinsic more advanced opcodes

    // Collection intrinsics
    MapNewFromPairs,
    MapRemove,
    MapIterInit,
    MapIterNext,
    MapIterNextPair,

    // String
    StringFromConstantSlice,
    StringAppend,

    // others
    Eq8Imm,
    Eq32,

    Tst8,

    HostCall, // calls back into host
    StringLen,
    IntToRnd,

    RangeIterInit,
    RangeIterNext,
    RangeIterNextPair,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hlt => write!(f, "hlt"), // Halt execution

            // Load
            Self::Ld8 => write!(f, "ld8"),
            Self::Ld16 => write!(f, "ld16"),
            Self::Ld32 => write!(f, "ld32"),

            Self::LdConst => write!(f, "ldconst"),

            // Move data
            Self::Mov => write!(f, "mov"),     // Move data
            Self::MovLp => write!(f, "movlp"), // Move data

            Self::AddI32 => write!(f, "sadd32"), // Signed Add
            Self::SubI32 => write!(f, "ssub32"), // Signed Add
            Self::MulI32 => write!(f, "smul32"), // Signed Add
            Self::NegI32 => write!(f, "sneg32"), // Signed negate
            Self::ModI32 => write!(f, "smod32"), // Signed Add

            Self::AddF32 => write!(f, "fadd"), // Signed Add
            Self::MulF32 => write!(f, "fmul"), // Signed Add
            Self::NegF32 => write!(f, "fneg"), // Signed negate

            // Functions
            Self::Call => write!(f, "call"),     // Call function
            Self::Enter => write!(f, "enter"),   // Function prologue
            Self::Ret => write!(f, "ret"),       // Return from function
            Self::HostCall => write!(f, "host"), // Call host function

            // Branches
            Self::Jmp => write!(f, "jmp"), // Unconditional jump
            Self::Bz => write!(f, "bz"),   // Branch if Zero (False)
            Self::Bnz => write!(f, "bnz"), // Branch if Not Zero (True)

            // Comparisons
            Self::LtI32 => write!(f, "slt32"), // signed Less Than
            Self::LeI32 => write!(f, "sle32"), // signed Less Than
            Self::GtI32 => write!(f, "sgt32"), // Set Less Than
            Self::GeI32 => write!(f, "sge32"),
            Self::Eq8Imm => write!(f, "eq8"),
            Self::Eq32 => write!(f, "eq32"),
            Self::Tst8 => write!(f, "tst8"),

            // Vec
            Self::VecPush => write!(f, "vec_push"),
            Self::VecLen => write!(f, "vec_len"),
            Self::VecSubscript => write!(f, "vec_subscript"),
            Self::VecSubscriptMut => write!(f, "vec_subscript_mut"),
            Self::VecFromSlice => write!(f, "vec_from_slice"),
            Self::VecIterInit => write!(f, "vec_iter_init"),
            Self::VecIterNext => write!(f, "vec_iter_next"),
            Self::VecIterNextPair => write!(f, "vec_iter_next_pair"),

            // Map
            Self::MapNewFromPairs => write!(f, "map_new_from_pairs"),
            Self::MapRemove => write!(f, "map_remove"),
            Self::MapIterInit => write!(f, "map_iter_init"),
            Self::MapIterNext => write!(f, "map_iter_next"),
            Self::MapIterNextPair => write!(f, "map_iter_next_pair"),

            // Range
            Self::RangeIterInit => write!(f, "range_iter_init"),
            Self::RangeIterNext => write!(f, "range_iter_next"),
            Self::RangeIterNextPair => write!(f, "map_iter_next_pair"),

            // String
            Self::StringFromConstantSlice => write!(f, "str_from_const"),
            Self::StringAppend => write!(f, "str_append"),
            Self::StringLen => write!(f, "str_len"),

            // Int
            Self::IntToRnd => write!(f, "int_rnd"),

            Self::Nop => write!(f, "nop"),
        }
    }
}

// Add this to your OpCode implementation
impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        // Safety: This assumes the u8 value corresponds to a valid OpCode variant
        // For production code, consider using TryFrom instead to validate the value
        unsafe { std::mem::transmute(value) }
    }
}
