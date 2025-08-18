//! Swamp MIR (mutable, non-SSA)

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct FunctionId(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct BlockId(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct VReg(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct TypeId(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct StackSlotId(pub u32);

// ---------- Minimal vreg kind metadata ----------

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub enum ValueKind {
    #[default]
    I32,
    Fixed32,
    Bool,        // 0 or 1 in a 32-bit vreg
    Ptr(TypeId), // pointer to TypeId
    Raw,         // escape hatch if needed
}

#[derive(Debug, Default)]
pub struct VRegInfo {
    pub kind: ValueKind,
}

// ---------- Memory typing ----------

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Width {
    Byte,
    Half,
    Word,
} // 8 / 16 / 32

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LoadTy {
    U8,
    I8,
    U16,
    I16,
    I32,
    Fixed32,
}

// ---------- Operands ----------

#[derive(Copy, Clone, Debug)]
pub enum Operand {
    Reg(VReg),
    ImmI32(i32),
    ImmU32(u32),
}

// ---------- Instructions (non-terminators) ----------

#[derive(Debug)]
pub enum Inst {
    // Moves & simple pointer math
    Mov {
        dst: VReg,
        src: Operand,
    },
    /// dst = base + offset (address form). If base is FP, backends may encode as LEA.
    AddPtrImm {
        dst: VReg,
        base: VReg,
        offset: i32,
    },

    // Memory
    Load {
        dst: VReg,
        base: VReg,
        offset: i32,
        ty: LoadTy,
    },
    Store {
        base: VReg,
        offset: i32,
        src: Operand,
        width: Width,
    },

    MemCpy {
        dst_ptr: VReg,
        src_ptr: VReg,
        len: u32,
    },

    ClrFp {
        fp_off: i32,
        size: u32,
    },

    // Integer / fixed-point arithmetic
    Add {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    Sub {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    Mul {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    SDiv {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    SMod {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    SNeg {
        dst: VReg,
        a: Operand,
    },

    FMul {
        dst: VReg,
        a: VReg,
        b: VReg,
    },
    FDiv {
        dst: VReg,
        a: VReg,
        b: VReg,
    },
    IToF {
        dst: VReg,
        x: VReg,
    },

    // Bitwise & shifts (kept small; great for ARM/retro backends)
    And {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    Or {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    Xor {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    Not {
        dst: VReg,
        a: VReg,
    },
    Shl {
        dst: VReg,
        a: VReg,
        b_or_imm: Operand,
    }, // b_or_imm must fit shift range
    LShr {
        dst: VReg,
        a: VReg,
        b_or_imm: Operand,
    },
    AShr {
        dst: VReg,
        a: VReg,
        b_or_imm: Operand,
    },

    // Comparisons (produce 0/1 in dst)
    CmpEq {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpNe {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpSLt {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpSLe {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpSGt {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpSGe {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpULt {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpULe {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpUGt {
        dst: VReg,
        a: Operand,
        b: Operand,
    },
    CmpUGe {
        dst: VReg,
        a: Operand,
        b: Operand,
    },

    // Calls & select
    /// ABI-lowering maps args/return to r0/sret/etc. Backends insert moves as needed.
    Call {
        dst: Option<VReg>,
        callee: FunctionId,
        args: Vec<VReg>,
    },
    /// dst = cond ? if_true : if_false (lowers to small if/else or backend cmov/predication).
    Select {
        dst: VReg,
        cond: VReg,
        if_true: VReg,
        if_false: VReg,
    },

    IMin {
        dst: VReg,
        a: VReg,
        b: VReg,
    },
    IMax {
        dst: VReg,
        a: VReg,
        b: VReg,
    },
    IClamp {
        dst: VReg,
        x: VReg,
        lo: VReg,
        hi: VReg,
    },
    IRnd {
        dst: VReg,
        seed: VReg,
    },
}

// ---------- Terminators (must be last in a block) ----------

#[derive(Debug)]
pub enum Terminator {
    Br { target: BlockId },
    BrZ { v: VReg, target: BlockId },
    BrNZ { v: VReg, target: BlockId },

    Ret { value: Option<VReg> },
    Hlt,
    //Trap { code: u16 },
}

// ---------- CFG containers ----------

#[derive(Debug)]
pub struct Block {
    pub id: BlockId,
    pub instructions: Vec<Inst>,
    pub terminator: Terminator,
}

#[derive(Debug)]
pub struct FnSig {
    pub parameters: Vec<TypeId>,
    pub ret: Option<TypeId>, // None = void
}

#[derive(Copy, Clone, Debug)]
pub enum SlotKind {
    Local,
    Spill,
    ParamShadow,
    SRetBuf,
}

#[derive(Debug)]
pub struct StackSlot {
    pub ty: TypeId,
    pub size: u32,
    pub align: u32,
    pub kind: SlotKind,
}

#[derive(Debug)]
pub struct Function {
    pub id: FunctionId,
    pub sig: FnSig,
    pub entry: BlockId,
    pub blocks: Vec<Block>,

    // vreg & stack metadata (filled by frontend/passes)
    pub vregs: Vec<VRegInfo>,  // index by VReg.0 as usize
    pub slots: Vec<StackSlot>, // symbolic; frame layout assigns concrete FP offsets

    // filled by frame layout pass
    pub stack_size: u32, // for prologue `enter #frame_size`
    pub slot_offsets: Vec<i32>,
}

#[derive(Debug, Default)]
pub struct Module {
    pub functions: Vec<Function>,
}
