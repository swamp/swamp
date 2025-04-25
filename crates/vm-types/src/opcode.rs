/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::fmt::{Display, Formatter};

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Nop,
    Hlt,   // Return to the host
    Panic, // Stop executing and return to host
    Brk,   // Breakpoint. pause execution, keep all relevant state

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
    SubF32,
    ModF32,
    DivF32,

    // Comparisons
    // Reads or sets the z flag
    // Integer
    LtI32,
    LeI32,
    GtI32,
    GeI32,

    // Fixed point
    LtF32,
    LeF32,
    GtF32,
    GeF32,

    // Comparison, set z flag
    Eq8Imm, // Compares a byte in an address with an immediate 8 bit value. Updates the z flag
    Cmp,    // Compare a memory area of bytes. Updates the z flag
    Cmp8,   // Compare a single byte. Updates the z flag
    Cmp32,  // Compare a 32 bit. Used for both Integer and Fixed point. Updates the z flag
    Tst8, // Load the byte into the z flag (zero = sets the z flag, any other value = clears the z flag)

    NotZ,

    // Store z flag
    Stz,
    Stnz,

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
    MovLp, // Copy overlapping
    Mov32,

    // Heap copy
    MovMem,

    Alloc, // For slices
    Stx,   // Copy from frame region to allocated region with offset
    Ldx,   // Copy from heap region with offset to frame region

    // Load immediate into frame memory
    Ld8,
    Ld16,
    Ld32,

    // Type specific -----

    // Float
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
    FloatToString,

    // Int
    IntToRnd,
    IntToFloat,
    IntAbs,
    IntMin,
    IntMax,
    IntClamp,
    IntToString,

    // Bool
    BoolToString,

    // Collection intrinsics ----

    // Range
    RangeIterInit,
    RangeIterNext,

    // Vec
    VecIterInit,
    VecIterNext,
    VecIterNextPair,
    VecFromSlice,
    VecPush,
    VecSubscript,
    VecSubscriptMut,
    VecRemoveIndex,
    VecPop,
    VecRemoveIndexGetValue,
    VecClear,
    VecCreate,
    VecGet,
    VecGetRange,

    // Map
    MapIterInit,
    MapIterNext,
    MapIterNextPair,
    MapNewFromPairs,
    MapRemove,
    MapSubscriptMut,
    MapHas,
    MapSubscript,
    MapSubscriptMutCreate,

    // String
    StringFromSlice,
    StringAppend,

    // Other
    HostCall, // calls back into host
    VecSwap,
}

impl OpCode {
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub const fn as_string(&self) -> &str {
        match self {
            Self::Nop => "nop",
            Self::Hlt => "hlt",
            Self::Panic => "panic",
            Self::Brk => "brk",

            // Integer arithmetic
            Self::AddI32 => "add",
            Self::MulI32 => "mul",
            Self::NegI32 => "neg",
            Self::SubI32 => "sub",
            Self::ModI32 => "mod",
            Self::DivI32 => "div",

            // Float arithmetic
            Self::AddF32 => "addf",
            Self::MulF32 => "mulf",
            Self::NegF32 => "negf",
            Self::SubF32 => "subf",
            Self::ModF32 => "modf",
            Self::DivF32 => "divf",

            // Integer comparisons
            Self::LtI32 => "lt",
            Self::LeI32 => "le",
            Self::GtI32 => "gt",
            Self::GeI32 => "ge",

            // Float comparisons
            Self::LtF32 => "ltf",
            Self::LeF32 => "lef",
            Self::GtF32 => "gtf",
            Self::GeF32 => "gef",

            // Byte/memory comparisons
            Self::Eq8Imm => "eq8i",
            Self::Cmp => "cmp",
            Self::Cmp8 => "cmp8",
            Self::Cmp32 => "cmp32",
            Self::Tst8 => "tst8",
            Self::NotZ => "notz",

            // Store Z flag
            Self::Stz => "stz",
            Self::Stnz => "stnz",

            // Branches
            Self::Bnz => "bnz",
            Self::Bz => "bz",
            Self::Jmp => "jmp",

            // Call/Frame/Return
            Self::Call => "call",
            Self::Enter => "enter",
            Self::Ret => "ret",

            // Move
            Self::Mov => "mov",
            Self::Mov32 => "mov32",
            Self::MovLp => "movlp",
            Self::MovMem => "movmem",
            Self::Stx => "stx",
            Self::Ldx => "ldx",

            // Load
            Self::Ld8 => "ld8",
            Self::Ld16 => "ld16",
            Self::Ld32 => "ld32",

            Self::Alloc => "alloc",

            // Float functions
            Self::FloatRound => "fround",
            Self::FloatFloor => "ffloor",
            Self::FloatSqrt => "fsqrt",
            Self::FloatSign => "fsign",
            Self::FloatAbs => "fabs",
            Self::FloatPseudoRandom => "fprnd",
            Self::FloatSin => "fsin",
            Self::FloatCos => "fcos",
            Self::FloatAcos => "facos",
            Self::FloatAsin => "fasin",
            Self::FloatAtan2 => "fatan2",
            Self::FloatMin => "fmin",
            Self::FloatMax => "fmax",
            Self::FloatClamp => "fclamp",
            Self::FloatToString => "ftos",

            // Int functions
            Self::IntToRnd => "irnd",
            Self::IntToFloat => "itof",
            Self::IntAbs => "iabs",
            Self::IntMin => "imin",
            Self::IntMax => "imax",
            Self::IntClamp => "iclamp",
            Self::IntToString => "itos",

            // Bool
            Self::BoolToString => "btos",

            // Range
            Self::RangeIterInit => "riter",
            Self::RangeIterNext => "ritern",

            // Vec
            Self::VecFromSlice => "vfrom",
            Self::VecPush => "vpush",
            Self::VecIterInit => "viter",
            Self::VecIterNext => "vitern",
            Self::VecIterNextPair => "viternp",
            Self::VecSubscript => "vsub",
            Self::VecSubscriptMut => "vsubm",
            Self::VecRemoveIndex => "vrem",
            Self::VecPop => "vpop",
            Self::VecRemoveIndexGetValue => "vremv",
            Self::VecClear => "vclear",
            Self::VecCreate => "vcreate",
            Self::VecGet => "vget",
            Self::VecGetRange => "vgetrn",
            Self::VecSwap => "vswap",

            // Map
            Self::MapNewFromPairs => "mnew",
            Self::MapRemove => "mrem",
            Self::MapIterInit => "miter",
            Self::MapIterNext => "mitern",
            Self::MapIterNextPair => "miternp",
            Self::MapSubscriptMut => "mset",
            Self::MapHas => "mhas",
            Self::MapSubscript => "mget",
            Self::MapSubscriptMutCreate => "mgetc",

            // String
            Self::StringFromSlice => "sfromc",
            Self::StringAppend => "sapp",

            // Other
            Self::HostCall => "host",
        }
    }
}
impl Display for OpCode {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
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
