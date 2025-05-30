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
    AddU32Imm,
    MulU32,
    SubU32,

    // i32 specific
    NegI32,
    ModI32,
    DivI32,

    // Fixed
    MulF32,
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

    // Loaders --------------

    // Load immediate into reg
    LdPtrFromEffectiveAddress, // Load effective address
    LoadEffectiveAddressIndexMultiplier,
    Ld8FromPointerWithOffset,
    Ld16FromPointerWithOffset,
    Ld32FromPointerWithOffset,

    LdRegFromFrame,
    LdRegFromFrameUsingMask,

    // TODO: LEA Rddest, [Rbase + ImmediateOffset]
    // TODO: LEA.SI Rdest, Rbase, Rindex, ScaleImmediate
    Ld8FromAbsoluteAddress,
    Ld32FromAbsoluteAddress,

    // Storers ---
    St8UsingPtrWithOffset,
    St16UsingPtrWithOffset,
    St32UsingPtrWithOffset,
    StRegToFrame,
    StRegToFrameUsingMask,

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

    // Range
    RangeInit,
    RangeIterInit,
    RangeIterNext,

    // Fixed Capacity Size Array
    ArrayInitWithLenAndCapacityAddr,

    // Vec
    VecIterInit,
    VecIterNext,
    VecIterNextPair,
    VecInitWithLenAndCapacityAddr,
    VecPushAddr,
    VecRemoveIndex,
    VecPop,
    VecRemoveIndexGetValue,
    VecClear,
    VecCreate,
    VecGet,
    VecGetRange,
    VecSwap,

    // Map
    MapInitWithCapacityAndKeySizeAddr,
    MapGetEntryLocation,
    MapGetOrReserveEntryLocation,

    MapIterInit,
    MapIterNext,
    MapIterNextPair,
    MapNewFromPairs,
    MapRemove,
    MapHas,

    // String
    StringAppend,
    StringCmp,

    // Other
    HostCall, // calls back into host
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
            Self::AddU32 | Self::AddU32Imm => "add",
            Self::MulU32 => "mul",
            Self::SubU32 => "sub",

            Self::NegI32 => "sneg",
            Self::ModI32 => "smod",
            Self::DivI32 => "sdiv",

            // Float arithmetic
            Self::MulF32 => "fmul",
            Self::DivF32 => "fdiv",

            // Integer comparisons
            Self::LtI32 => "slt",
            Self::LeI32 => "sle",
            Self::GtI32 => "sgt",
            Self::GeI32 => "sge",

            Self::GeU32 => "ge",
            Self::LtU32 => "lt",

            // Byte/memory comparisons
            Self::Eq8Imm | Self::CmpReg => "cmp",
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
            Self::BlockCopyWithOffsets | Self::BlockCopy => "blkcpy",
            Self::LdPtrFromEffectiveAddress | Self::LoadEffectiveAddressIndexMultiplier => "lea",

            // Load
            Self::Mov8FromImmediateValue => "movb",
            Self::Mov16FromImmediateValue => "movh",
            Self::MovReg | Self::Mov32FromImmediateValue => "mov",

            Self::Ld8FromPointerWithOffset | Self::Ld8FromAbsoluteAddress => "ldb",
            Self::Ld16FromPointerWithOffset => "ldh",
            Self::Ld32FromPointerWithOffset | Self::Ld32FromAbsoluteAddress => "ld",
            Self::LdRegFromFrameUsingMask | Self::LdRegFromFrame => "ldmf",

            Self::St32UsingPtrWithOffset => "st",
            Self::St16UsingPtrWithOffset => "sth",
            Self::St8UsingPtrWithOffset => "stb",
            Self::StRegToFrameUsingMask | Self::StRegToFrame => "stmf",

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

            // Range
            Self::RangeInit => "range.init",
            Self::RangeIterInit => "range.iter",
            Self::RangeIterNext => "range.itern",

            // Fixed capacity size array
            Self::ArrayInitWithLenAndCapacityAddr => "array.init",

            // Vec
            Self::VecInitWithLenAndCapacityAddr => "vec.init",
            Self::VecPushAddr => "vec.push",
            Self::VecIterInit => "vec.iter",
            Self::VecIterNext => "vec.itern",
            Self::VecIterNextPair => "vect.iternp",
            Self::VecRemoveIndex => "vec.rem",
            Self::VecPop => "vec.pop",
            Self::VecRemoveIndexGetValue => "vec.remv",
            Self::VecClear => "vec.clear",
            Self::VecCreate => "vec.create",
            Self::VecGet => "vec.get",
            Self::VecGetRange => "vec.getrn",
            Self::VecSwap => "vec.swap",

            // Map
            Self::MapInitWithCapacityAndKeySizeAddr => "map.init",
            Self::MapGetEntryLocation => "map.entry",
            Self::MapGetOrReserveEntryLocation => "map.entry.must",

            Self::MapNewFromPairs => "map.new",
            Self::MapRemove => "map.rem",
            Self::MapIterInit => "map.iter",
            Self::MapIterNext => "map.itern",
            Self::MapIterNextPair => "map.iternp",
            Self::MapHas => "map.has",

            // String
            Self::StringAppend => "str.app",
            Self::StringCmp => "str.cmp",
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
