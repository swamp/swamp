/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::fmt::{Display, Formatter};

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Nop,
    Hlt, // Return to the host
    UserHalt,
    Step,  // Stop executing and return to host. User has a step-point here.
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
    // Clears or sets the P flag
    // Integer and Float
    LtI32,
    LeI32,
    GtI32,
    GeI32,

    // Unsigned int
    GeU32,
    LtU32,

    // Comparison, set P flag
    Eq8Imm,
    CmpReg,
    CmpBlock,

    // Other Comparisons
    TrapOnLessThan,

    // Conditional branching
    BFalse,
    BTrue,

    // Unconditional branching
    B,

    // Call, frame and return
    Call,
    Enter,
    Ret,

    BlockCopy, // Copy without offsets
    FrameMemClr,

    // Loaders --------------

    // Load immediate into reg
    LdPtrFromEffectiveFrameAddress, // Load effective address
    Ld8FromPointerWithOffset,
    Ld16FromPointerWithOffset,
    Ld32FromPointerWithOffset,

    LdRegFromFrameRange,
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
    // Mov immediate
    Mov8FromImmediateValue,
    Mov16FromImmediateValue,
    Mov32FromImmediateValue,

    MovEqualToZero,
    // TODO: Maybe bring back MovNotEqualToZero back for symmetry / completeness?

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

    // Codepoint
    CodepointToString,

    // Bool
    BoolToString,

    ByteToString,

    // Collection intrinsics ----

    // Range
    RangeInit,
    RangeIterInit,
    RangeIterNext,

    // Fixed Capacity Size Array
    ArrayInitWithLenAndCapacity,

    // Vec
    VecInit, // For vec likes
    VecCmp,
    VecCopy,
    VecPushAddr,
    VecRemoveIndex,
    VecPop,
    VecRemoveIndexGetValue,
    VecGet,
    VecCopyRange,
    VecSwap,
    VecIterInit,
    VecIterNext,
    VecIterNextPair,

    // Map
    MapInitWithCapacityAndKeyAndTupleSizeAddr, // Initialize the Map
    MapGetEntryLocation,                       // RHS, lookup, do not add
    MapGetOrReserveEntryLocation,              // For the LHS, create entry if needed
    MapRemove,
    MapHas,
    MapOverwrite,

    MapIterInit,
    MapIterNext,
    MapIterNextPair,

    // String
    StringAppend,
    StringRepeat,
    StringCmp,
    StringToString,
    StringStartsWith,
    StringIterInit,
    StringIterNext,
    StringIterNextPair,

    // Other
    HostCall, // calls back into host

    // Sparse
    SparseInit,
    SparseAddGiveEntryAddress,
    SparseRemove,
    SparseGetEntryAddr,
    SparseIterNext,
    SparseIterNextPair,
    SparseIterInit,
    SparseIsAlive,

    // Grid
    GridInit,
    GridGetEntryAddr,
    LeU32,
    GtU32,
    StringToInt,
    StringToFloat,
}

impl OpCode {
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub const fn as_string(&self) -> &str {
        match self {
            Self::Nop => "nop",
            Self::Hlt => "hlt",
            Self::UserHalt => "user_halt",
            Self::Step => "step",
            Self::Panic => "panic",
            Self::Trap => "trap",
            Self::Brk => "brk",

            // Integer arithmetic
            Self::AddU32 | Self::AddU32Imm => "add",
            Self::MulU32 => "mul",
            Self::SubU32 => "sub",

            Self::NegI32 => "s.neg",
            Self::ModI32 => "s.mod",
            Self::DivI32 => "s.div",

            // Float arithmetic
            Self::MulF32 => "f.mul",
            Self::DivF32 => "f.div",

            // Integer comparisons
            Self::LtI32 => "lt",
            Self::LeI32 => "le",
            Self::GtI32 => "gt",
            Self::GeI32 => "ge",

            Self::GeU32 => "uge",
            Self::GtU32 => "ugt",
            Self::LtU32 => "ult",
            Self::LeU32 => "ule",

            // Byte/memory comparisons
            Self::Eq8Imm | Self::CmpReg => "cmp",
            Self::CmpBlock => "cmp.blk",
            Self::FrameMemClr => "clr.blk.f",

            // Other comparisons
            Self::TrapOnLessThan => "trap.lt",

            // Branches
            Self::BFalse => "b.false",
            Self::BTrue => "b.true",
            Self::B => "b",

            // Call/Frame/Return
            Self::Call => "call",
            Self::Enter => "enter",
            Self::Ret => "ret",

            // Mem
            Self::BlockCopy => "blk.cpy",
            Self::LdPtrFromEffectiveFrameAddress => "lea",

            // Move
            Self::Mov8FromImmediateValue => "mov.b",
            Self::Mov16FromImmediateValue => "mov.h",
            Self::MovReg | Self::Mov32FromImmediateValue => "mov", // alias for `mov.w`
            Self::MovEqualToZero => "meqz",

            // Load. From memory to register
            Self::Ld8FromPointerWithOffset | Self::Ld8FromAbsoluteAddress => "ld.b",
            Self::Ld16FromPointerWithOffset => "ld.h",
            Self::Ld32FromPointerWithOffset | Self::Ld32FromAbsoluteAddress => "ld", // alias for `ld.w`
            Self::LdRegFromFrameUsingMask | Self::LdRegFromFrameRange => "ldmf",

            // Store. From register to memory
            Self::St32UsingPtrWithOffset => "st", // alias for `st.w`
            Self::St16UsingPtrWithOffset => "st.h",
            Self::St8UsingPtrWithOffset => "st.b",
            Self::StRegToFrameUsingMask | Self::StRegToFrame => "stmf",

            // Float functions
            Self::FloatRound => "f.round",
            Self::FloatFloor => "f.floor",
            Self::FloatSqrt => "f.sqrt",
            Self::FloatSign => "f.sign",
            Self::FloatAbs => "f.abs",
            Self::FloatPseudoRandom => "f.prnd",
            Self::FloatSin => "f.sin",
            Self::FloatCos => "f.cos",
            Self::FloatAcos => "f.acos",
            Self::FloatAsin => "f.asin",
            Self::FloatAtan2 => "f.atan2",
            Self::FloatMin => "f.min",
            Self::FloatMax => "f.max",
            Self::FloatClamp => "f.clamp",
            Self::FloatToString => "f.to.str",

            // Int functions
            Self::IntToRnd => "i.rnd",
            Self::IntToFloat => "i.tof",
            Self::IntAbs => "i.abs",
            Self::IntMin => "i.min",
            Self::IntMax => "i.max",
            Self::IntClamp => "i.clamp",
            Self::IntToString => "i.tos",

            // Other
            Self::HostCall => "host",

            // Bool
            Self::BoolToString => "bool.to.str",

            // Bool
            Self::CodepointToString => "codepoint.to.str",

            // Byte
            Self::ByteToString => "byte.to.str",

            // Range
            Self::RangeInit => "range.init",
            Self::RangeIterInit => "range.iter",
            Self::RangeIterNext => "range.iter.next",

            // Fixed capacity size array
            Self::ArrayInitWithLenAndCapacity => "array.init",

            // Vec
            Self::VecPushAddr => "vec.push",
            Self::VecRemoveIndex => "vec.rem",
            Self::VecPop => "vec.pop",
            Self::VecRemoveIndexGetValue => "vec.rem.v",
            Self::VecGet => "vec.get",
            Self::VecCopyRange => "vec.copy.range",
            Self::VecSwap => "vec.swap",
            Self::VecInit => "vec.init",
            Self::VecCopy => "vec.copy",
            Self::VecCmp => "vec.cmp",
            Self::VecIterInit => "vec.iter",
            Self::VecIterNext => "vec.iter.next",
            Self::VecIterNextPair => "vec.iter.next.pair",

            // Map
            Self::MapInitWithCapacityAndKeyAndTupleSizeAddr => "map.init",
            Self::MapGetEntryLocation => "map.entry",
            Self::MapGetOrReserveEntryLocation => "map.entry.or_create",
            Self::MapHas => "map.has",
            Self::MapOverwrite => "map.overwrite",
            Self::MapRemove => "map.rem",
            Self::MapIterInit => "map.iter.init",
            Self::MapIterNext => "map.iter.next",
            Self::MapIterNextPair => "map.iter.next.pair",

            // Sparse
            Self::SparseInit => "sparse.init",
            Self::SparseAddGiveEntryAddress => "sparse.add_entry_addr",
            Self::SparseRemove => "sparse.remove",
            Self::SparseGetEntryAddr => "sparse.entry_addr",
            Self::SparseIsAlive => "sparse.is_alive",
            // iter
            Self::SparseIterInit => "sparse.iter.init",
            Self::SparseIterNext => "sparse.iter.next",
            Self::SparseIterNextPair => "sparse.iter.next.pair",

            // Grid
            Self::GridInit => "grid.init",
            Self::GridGetEntryAddr => "grid.entry_addr",

            // String
            Self::StringAppend => "str.app",
            Self::StringRepeat => "str.repeat",
            Self::StringCmp => "str.cmp",
            Self::StringToString => "str.tos",
            Self::StringStartsWith => "str.starts.with",
            Self::StringToInt => "str.to.int",
            Self::StringToFloat => "str.to.float",
            Self::StringIterInit => "str.iter",
            Self::StringIterNext => "str.iter.next",
            Self::StringIterNextPair => "str.iter.next.pair",
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
