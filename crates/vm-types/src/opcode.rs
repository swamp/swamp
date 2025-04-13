/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::fmt::{Display, Formatter};

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Hlt, // Return to the host
    Panic,

    // Operators
    // i32
    AddI32,
    MulI32,
    NegI32,
    SubI32,
    ModI32,
    DivI32,

    // Fixed
    AddF32,
    MulF32,
    NegF32,
    DivF32,

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
    IntToFloat,

    RangeIterInit,
    RangeIterNext,
    RangeIterNextPair,
    IntToString,
    MapHas,
    MapSubscript,
    MapSubscriptMutCreate,
    VecRemoveIndex,
    FloatToString,
    ModF32,
    SubF32,
    LtF32,
    LeF32,
    GtF32,
    GeF32,
    Ne32,
    FloatRound,
    FloatFloor,
    FloatSqrt,
    FloatSign,
    FloatAbs,
    FloatPseudoRandom,
    FloatSin,
    FloatCos,
    FloatAcos,
    FloatAsin,
    FloatAtan2,
    FloatMin,
    FloatMax,
    FloatClamp,
    IntAbs,
    IntMin,
    IntMax,
    IntClamp,
    Not8,
    VecPop,
    Cmp,

    Cmp8,
    Cmp32,
    Stz,
    Stnz,
    BoolToString,
    MapSubscriptMut,

    VecIsEmpty,
    MapLen,
    VecRemoveIndexGetValue,
    VecClear,
    VecCreate,
    VecGet,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hlt => write!(f, "hlt"), // Halt execution
            Self::Panic => write!(f, "panic"),

            // Load
            Self::Ld8 => write!(f, "ld8"),
            Self::Ld16 => write!(f, "ld16"),
            Self::Ld32 => write!(f, "ld32"),

            Self::LdConst => write!(f, "ldconst"),

            // Move data
            Self::Mov => write!(f, "mov"),     // Move data
            Self::MovLp => write!(f, "movlp"), // Move data

            // Int operations
            Self::AddI32 => write!(f, "sadd32"), // Signed Add
            Self::SubI32 => write!(f, "ssub32"), //
            Self::MulI32 => write!(f, "smul32"), //
            Self::NegI32 => write!(f, "sneg32"), // Signed negate
            Self::ModI32 => write!(f, "smod32"), //
            Self::DivI32 => write!(f, "sdiv32"), //

            // Float operations
            Self::AddF32 => write!(f, "fadd"), // Signed Add
            Self::MulF32 => write!(f, "fmul"), // Signed Add
            Self::DivF32 => write!(f, "fdiv"), //
            Self::NegF32 => write!(f, "fneg"), // Signed negate
            Self::ModF32 => write!(f, "fmod"), //
            Self::SubF32 => write!(f, "fsub"), //

            Self::FloatToString => write!(f, "f_to_string"),

            Self::FloatRound => write!(f, "round"),
            Self::FloatFloor => write!(f, "floor"),
            Self::FloatSqrt => write!(f, "sqrt"),
            Self::FloatSign => write!(f, "fsign"),
            Self::FloatCos => write!(f, "cos"),
            Self::FloatAbs => write!(f, "fabs"),
            Self::FloatSin => write!(f, "sin"),
            Self::FloatAcos => write!(f, "acos"),
            Self::FloatAsin => write!(f, "asin"),
            Self::FloatAtan2 => write!(f, "atan2"),
            Self::FloatMin => write!(f, "fmin"),
            Self::FloatMax => write!(f, "fmax"),
            Self::FloatClamp => write!(f, "fclamp"),
            Self::FloatPseudoRandom => write!(f, "fprnd"),

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

            // Fixed comparisons
            Self::LtF32 => write!(f, "flt32"), // signed Less Than
            Self::LeF32 => write!(f, "fle32"), // signed Less Than
            Self::GtF32 => write!(f, "fgt32"), // Set Less Than
            Self::GeF32 => write!(f, "fge32"),

            // Byte comparisons
            Self::Eq8Imm => write!(f, "eq8"),
            Self::Not8 => write!(f, "not8"),
            Self::Eq32 => write!(f, "eq32"),
            Self::Ne32 => write!(f, "neq32"),
            Self::Tst8 => write!(f, "tst8"),
            Self::Stz => write!(f, "stz8"),
            Self::Stnz => write!(f, "stnz8"),
            Self::Cmp8 => write!(f, "cmp8"),
            Self::Cmp32 => write!(f, "cmp32"),
            Self::Cmp => write!(f, "cmp"),

            // Vec
            Self::VecPush => write!(f, "vec_push"),
            Self::VecLen => write!(f, "vec_len"),
            Self::VecIsEmpty => write!(f, "vec_is_empty"),
            Self::VecClear => write!(f, "vec_clear"),
            Self::VecSubscript => write!(f, "vec_subscript"),
            Self::VecSubscriptMut => write!(f, "vec_subscript_mut"),
            Self::VecFromSlice => write!(f, "vec_from_slice"),
            Self::VecIterInit => write!(f, "vec_iter_init"),
            Self::VecIterNext => write!(f, "vec_iter_next"),
            Self::VecIterNextPair => write!(f, "vec_iter_next_pair"),
            Self::VecRemoveIndex => write!(f, "vec_remove_index"),
            Self::VecRemoveIndexGetValue => write!(f, "vec_remove_index_get_value"),
            Self::VecGet => write!(f, "vec_get"),
            Self::VecPop => write!(f, "vec_pop"),
            Self::VecCreate => write!(f, "vec_create"),

            // Map
            Self::MapNewFromPairs => write!(f, "map_new_from_pairs"),
            Self::MapRemove => write!(f, "map_remove"),
            Self::MapIterInit => write!(f, "map_iter_init"),
            Self::MapIterNext => write!(f, "map_iter_next"),
            Self::MapIterNextPair => write!(f, "map_iter_next_pair"),
            Self::MapHas => write!(f, "map_has"),
            Self::MapSubscript => write!(f, "map_subscript"),
            Self::MapSubscriptMutCreate => write!(f, "map_subscript_mut_create"),
            Self::MapSubscriptMut => write!(f, "map_subscript_mut"),
            Self::MapLen => write!(f, "map_len"),

            // Range
            Self::RangeIterInit => write!(f, "range_iter_init"),
            Self::RangeIterNext => write!(f, "range_iter_next"),
            Self::RangeIterNextPair => write!(f, "range_iter_next_pair"),

            // String
            Self::StringFromConstantSlice => write!(f, "str_from_const"),
            Self::StringAppend => write!(f, "str_append"),
            Self::StringLen => write!(f, "str_len"),

            // Int
            Self::IntToRnd => write!(f, "int_rnd"),
            Self::IntToFloat => write!(f, "int_to_float"),
            Self::IntToString => write!(f, "int_to_string"),
            Self::IntAbs => write!(f, "int_abs"),
            Self::IntMin => write!(f, "int_min"),
            Self::IntMax => write!(f, "int_max"),
            Self::IntClamp => write!(f, "int_clamp"),

            Self::BoolToString => write!(f, "bool_to_string"),

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
