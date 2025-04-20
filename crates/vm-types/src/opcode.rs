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

    // Heap copy
    MovMem,

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
    RangeIterNextPair,

    // Vec
    VecFromSlice,
    VecPush,
    VecIterInit,
    VecIterNext,
    VecIterNextPair,
    VecLen,
    VecSubscript,
    VecSubscriptMut,
    VecRemoveIndex,
    VecIsEmpty,
    VecPop,
    VecRemoveIndexGetValue,
    VecClear,
    VecCreate,
    VecGet,

    // Map
    MapNewFromPairs,
    MapRemove,
    MapIterInit,
    MapIterNext,
    MapIterNextPair,
    MapSubscriptMut,
    MapLen,
    MapHas,
    MapSubscript,
    MapSubscriptMutCreate,

    // String
    StringFromSlice,
    StringAppend,
    StringLen,

    // Other
    HostCall, // calls back into host
}

/*
  match self {
           Self::Nop => write!(f, "nop"),

           Self::Hlt => write!(f, "hlt"), // Halt execution, return to host
           Self::Brk => write!(f, "brk"), // Halt execution, it has hit a breakpoint
           Self::Panic => write!(f, "panic"), // Something bad happened, stops the execution and returns to host

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

           // Float specifics
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


           // Comparisons - uses Z flag
           Self::LtI32 => write!(f, "slt32"), // signed Less Than
           Self::LeI32 => write!(f, "sle32"), // signed Less Than
           Self::GtI32 => write!(f, "sgt32"), // signed Greater Than
           Self::GeI32 => write!(f, "sge32"), // signed greater or Equal

           // Fixed comparisons
           Self::LtF32 => write!(f, "flt32"), // Fixed point Less Than
           Self::LeF32 => write!(f, "fle32"), // Fixed point less than or equal
           Self::GtF32 => write!(f, "fgt32"), // Fixed point greater than
           Self::GeF32 => write!(f, "fge32"), // Fixed point greater than or equal

           // Byte comparisons
           // Sets the Z Flag accordingly
           Self::Eq8Imm => write!(f, "eq8"),
           Self::Not8 => write!(f, "not8"),
           Self::Cmp8 => write!(f, "cmp8"),
           Self::Cmp32 => write!(f, "cmp32"),
           Self::Tst8 => write!(f, "tst8"), // Almost like a load Z flag
           Self::Cmp => write!(f, "cmp"),
           Self::Stz => write!(f, "stz8"), // Store z flag
           Self::Stnz => write!(f, "stnz8"), // store the inverse of z flag

           // Branches
           Self::Bz => write!(f, "bz"),   // Branch if Zero (False). Relative ip.
           Self::Bnz => write!(f, "bnz"), // Branch if Not Zero (True). Relative ip.
           Self::Jmp => write!(f, "jmp"), // Unconditional jump
           Self::LongJmp => write!(f, "jmp_far"), // Unconditional jump

           // Functions
           Self::Call => write!(f, "call"),     // Call function
           Self::Enter => write!(f, "enter"),   // Function prologue
           Self::Ret => write!(f, "ret"),       // Return from function
           Self::HostCall => write!(f, "host"), // Call host function

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

           // Bool
           Self::BoolToString => write!(f, "bool_to_string"),
       }
*/

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
            Self::MovLp => "movlp",
            Self::MovMem => "movmem",

            // Load
            Self::Ld8 => "ld8",
            Self::Ld16 => "ld16",
            Self::Ld32 => "ld32",

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
            Self::RangeIterNextPair => "riternp",

            // Vec
            Self::VecFromSlice => "vfrom",
            Self::VecPush => "vpush",
            Self::VecIterInit => "viter",
            Self::VecIterNext => "vitern",
            Self::VecIterNextPair => "viternp",
            Self::VecLen => "vlen",
            Self::VecSubscript => "vsub",
            Self::VecSubscriptMut => "vsubm",
            Self::VecRemoveIndex => "vrem",
            Self::VecIsEmpty => "visemp",
            Self::VecPop => "vpop",
            Self::VecRemoveIndexGetValue => "vremv",
            Self::VecClear => "vclear",
            Self::VecCreate => "vcreate",
            Self::VecGet => "vget",

            // Map
            Self::MapNewFromPairs => "mnew",
            Self::MapRemove => "mrem",
            Self::MapIterInit => "miter",
            Self::MapIterNext => "mitern",
            Self::MapIterNextPair => "miternp",
            Self::MapSubscriptMut => "msubm",
            Self::MapLen => "mlen",
            Self::MapHas => "mhas",
            Self::MapSubscript => "msub",
            Self::MapSubscriptMutCreate => "msubmc",

            // String
            Self::StringFromSlice => "sfromc",
            Self::StringAppend => "sapp",
            Self::StringLen => "slen",

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
