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
    // u32 and i32
    AddU32,
    MulU32,
    SubU32,

    // i32 specific
    NegI32,
    ModI32,
    DivI32,

    // Fixed
    MulF32,
    ModF32,
    DivF32,

    // Comparisons
    // Reads or sets the z flag
    // Integer and Float
    LtI32,
    LeI32,
    GtI32,
    GeI32,

    // Comparison, set z flag
    Eq8Imm,
    CmpReg,
    CmpBlock,

    NotZ, // Invert z flag

    // Conditional branching
    BNe,
    BEq,

    // Unconditional branching
    B,

    // Call, frame and return
    Call, // Introduce CallLong if needed
    Enter,
    Ret,

    BlockCopy, // Copy from heap region with offset to frame region
    FrameMemClr,

    Alloc, // Mainly for slices

    // Loaders --------------

    // Load immediate into reg
    LdPtrFromEffectiveAddress, // Load effective address
    Ld8FromPointerWithOffset,
    Ld32FromPointerWithOffset,

    // Storers ---
    St32UsingPtrWithOffset,
    St8UsingPtrWithOffset,
    StRegToFrame,

    // Movers
    MovReg,
    MovToZFromReg, // Load the byte into the z flag (zero = sets the z flag, any other value = clears the z flag)
    MovFromZToReg,
    MovFromNotZToReg,
    // Mov immediate
    Mov8FromImmediateValue,
    Mov32FromImmediateValue,

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

    // Slice
    SliceFromHeap,
    SlicePairFromHeap,

    // Range
    RangeIterInit,
    RangeIterNext,

    // Vec
    VecIterInit,
    VecIterNext,
    VecIterNextPair,
    VecFromSlice,
    VecPush,
    VecFetch,
    VecSet,
    VecRemoveIndex,
    VecPop,
    VecRemoveIndexGetValue,
    VecClear,
    VecCreate,
    VecGet,
    VecGetRange,
    VecSwap,

    // Map
    MapIterInit,
    MapIterNext,
    MapIterNextPair,
    MapNewFromPairs,
    MapRemove,
    MapHas,
    MapFetch,
    MapSet,

    // String
    StringAppend,

    // Other
    HostCall, // calls back into host
    LdRegFromFrame,
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
            Self::AddU32 => "add",
            Self::MulU32 => "mul",
            Self::SubU32 => "sub",

            Self::NegI32 => "sneg",
            Self::ModI32 => "smod",
            Self::DivI32 => "sdiv",

            // Float arithmetic
            Self::MulF32 => "fmul",
            Self::ModF32 => "fmod",
            Self::DivF32 => "fdiv",

            // Integer comparisons
            Self::LtI32 => "lt",
            Self::LeI32 => "le",
            Self::GtI32 => "gt",
            Self::GeI32 => "ge",

            // Byte/memory comparisons
            Self::Eq8Imm => "eq8",
            Self::CmpReg => "cmp",
            Self::CmpBlock => "cmpblk",
            Self::MovToZFromReg => "tst",
            Self::FrameMemClr => "memclrf",

            // Store Z flag
            Self::NotZ => "notz",
            Self::MovFromZToReg => "stz",
            Self::MovFromNotZToReg => "stnz",

            // Branches
            Self::BNe => "b.ne",
            Self::BEq => "b.eq",
            Self::B => "b",

            // Call/Frame/Return
            Self::Call => "call",
            Self::Enter => "enter",
            Self::Ret => "ret",

            // Move
            Self::MovReg => "mov",
            Self::BlockCopy => "movmem",
            Self::LdPtrFromEffectiveAddress => "lea",

            // Load
            Self::Mov8FromImmediateValue => "movb",
            Self::Mov32FromImmediateValue => "movw",

            Self::Ld8FromPointerWithOffset => "ldb",
            Self::Ld32FromPointerWithOffset => "ldw",
            Self::LdRegFromFrame => "ldfp",

            Self::St32UsingPtrWithOffset => "stw",
            Self::St8UsingPtrWithOffset => "stb",

            Self::StRegToFrame => "stfp",

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

            // Slice
            Self::SliceFromHeap => "slicef",
            Self::SlicePairFromHeap => "slicepf",

            // Range
            Self::RangeIterInit => "riter",
            Self::RangeIterNext => "ritern",

            // Vec
            Self::VecFromSlice => "vfrom",
            Self::VecPush => "vpush",
            Self::VecIterInit => "viter",
            Self::VecIterNext => "vitern",
            Self::VecIterNextPair => "viternp",
            Self::VecFetch => "vget",
            Self::VecSet => "vset",
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
            Self::MapHas => "mhas",
            Self::MapFetch => "mget",
            Self::MapSet => "mset",

            // String
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
