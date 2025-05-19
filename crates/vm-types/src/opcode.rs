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
    Panic, // Stop executing and return to host. String provides reason.
    Trap,  // Provides a reason code.
    Brk,   // Breakpoint. pause execution, keep all relevant state

    // Operators
    // u32 and i32
    AddU32,
    // TODO: Add u32 with immediate, Rd, Rm, #immediate
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

    // Unsigned int
    GeU32,
    LtU32,

    // Comparison, set z flag
    Eq8Imm,
    CmpReg,
    CmpBlock,

    NotT, // Invert z flag

    // Conditional branching
    BFalse,
    BTrue,

    // Unconditional branching
    B,

    // Call, frame and return
    Call,
    Enter,
    Ret,

    BlockCopyWithOffsets, // Copy from heap region with offset to frame region
    BlockCopy,            // Copy without offsets
    FrameMemClr,

    Alloc, // Mainly for slices

    // Loaders --------------

    // Load immediate into reg
    LdPtrFromEffectiveAddress, // Load effective address
    Ld8FromPointerWithOffset,
    Ld16FromPointerWithOffset,
    Ld32FromPointerWithOffset,
    LdRegFromFrame,

    // TODO: LEA Rddest, [Rbase + ImmediateOffset]
    // TODO: LEA.SI Rdest, Rbase, Rindex, ScaleImmediate
    Ld8FromAbsoluteAddress,
    Ld32FromAbsoluteAddress,

    // Storers ---
    St8UsingPtrWithOffset,
    St16UsingPtrWithOffset,
    St32UsingPtrWithOffset,
    StRegToFrame,

    // Movers
    MovReg,
    MovToTFlagFromReg, // Load the byte into the z flag (zero = clears the t flag, any other value = sets the t flag)
    MovFromTFlagToReg,
    MovFromNotTFlagToReg,
    // Mov immediate
    Mov8FromImmediateValue,
    Mov16FromImmediateValue,
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
    VecPushAddr,
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
    StringCmp,

    // Other
    HostCall, // calls back into host
    AddU32Imm,
    LoadEffectiveAddressIndexMultiplier,
}

impl OpCode {
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub const fn as_string(&self) -> &str {
        match self {
            Self::Nop => "nop",
            Self::Hlt => "hlt",
            Self::Panic => "panic",
            Self::Trap => "trap",
            Self::Brk => "brk",

            // Integer arithmetic
            Self::AddU32 => "add",
            Self::AddU32Imm => "add",
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
            Self::LtI32 => "slt",
            Self::LeI32 => "sle",
            Self::GtI32 => "sgt",
            Self::GeI32 => "sge",

            Self::GeU32 => "ge",
            Self::LtU32 => "lt",

            // Byte/memory comparisons
            Self::Eq8Imm => "cmp",
            Self::CmpReg => "cmp",
            Self::CmpBlock => "cmpblk",
            Self::MovToTFlagFromReg => "tst",
            Self::FrameMemClr => "memclrf",

            // Store T flag (maybe rename to Predicate flag?)
            Self::NotT => "notpf",
            Self::MovFromTFlagToReg => "movpf",
            Self::MovFromNotTFlagToReg => "movnpf",

            // Branches
            Self::BFalse => "b.false",
            Self::BTrue => "b.true",
            Self::B => "b",

            // Call/Frame/Return
            Self::Call => "call",
            Self::Enter => "enter",
            Self::Ret => "ret",

            // Move
            Self::MovReg => "mov",
            Self::BlockCopyWithOffsets | Self::BlockCopy => "blkcpy",
            Self::LdPtrFromEffectiveAddress | Self::LoadEffectiveAddressIndexMultiplier => "lea",


            // Load
            Self::Mov8FromImmediateValue => "movb",
            Self::Mov16FromImmediateValue => "movh",
            Self::Mov32FromImmediateValue => "mov",

            Self::Ld8FromPointerWithOffset | Self::Ld8FromAbsoluteAddress => "ldrb",
            Self::Ld16FromPointerWithOffset /*| Self::Ld32FromAbsoluteAddress*/ => "ldrh",
            Self::Ld32FromPointerWithOffset | Self::Ld32FromAbsoluteAddress => "ldr",
            Self::LdRegFromFrame => "ldmf",

            Self::St32UsingPtrWithOffset => "str",
            Self::St16UsingPtrWithOffset => "strh",
            Self::St8UsingPtrWithOffset => "strb",

            Self::StRegToFrame => "stmf",

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

            // Other
            Self::HostCall => "host",

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
            Self::VecPushAddr => "vpush",
            Self::VecIterInit => "viter",
            Self::VecIterNext => "vitern",
            Self::VecIterNextPair => "viternp",
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
            Self::StringCmp => "strcmp",
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
