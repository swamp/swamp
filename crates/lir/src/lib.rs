use std::sync::Arc;

mod pretty;

/// A minimal LIR abstraction
/// It is designed to be as simple and easy to lower to hardware as possible
///
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct FunctionId(pub u32);
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct BlockId(pub u32);
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct VReg(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Base {
    Reg(VReg), // pointer
    SP,        // physical stack pointer (e.g., r13)
}

// ---------- Memory typing ----------
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Width {
    Byte,
    Half,
    Word,
}
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LoadTy {
    U8,
    I8,
    U16,
    I16,
    I32,
}

#[derive(Copy, Clone, Debug)]
pub enum Operand {
    Reg(VReg),
    ImmI32(i32),
    ImmU32(u32),
}

#[cfg(feature = "debug-info")]
#[derive(Clone, Debug, Default)]
pub struct SrcLoc {
    pub file: Arc<str>,
    pub line: u32,
    pub col: u32,
}

#[cfg(feature = "debug-info")]
#[derive(Clone, Debug, Default)]
pub struct InstDbg {
    pub comment: Option<Arc<str>>,
    pub origin: Option<Arc<str>>,
    pub src: Option<SrcLoc>,
}

#[derive(Debug)]
pub struct Inst {
    pub op: Instruction,
    #[cfg(feature = "debug-info")]
    pub dbg: InstDbg,
}

impl Inst {
    pub fn new(op: Instruction) -> Self {
        Self {
            op,
            #[cfg(feature = "debug-info")]
            dbg: InstDbg::default(),
        }
    }

    #[cfg(feature = "debug-info")]
    pub fn with_comment(mut self, text: impl Into<Arc<str>>) -> Self {
        self.dbg.comment = Some(text.into());
        self
    }

    #[cfg(feature = "debug-info")]
    pub fn because(mut self, origin: impl Into<Arc<str>>) -> Self {
        self.dbg.origin = Some(origin.into());
        self
    }

    #[cfg(feature = "debug-info")]
    pub fn at(mut self, file: impl Into<Arc<str>>, line: u32, col: u32) -> Self {
        self.dbg.src = Some(SrcLoc {
            file: file.into(),
            line,
            col,
        });
        self
    }
}

// ---------- Core instructions (no intrinsics, no Select) ----------
#[derive(Debug)]
pub enum Instruction {
    // moves & addr
    Mov {
        dst: VReg,
        src: Operand,
    },
    AddPtrImm {
        dst: VReg,
        base: Base,
        offset: i32,
    },

    // memory
    Load {
        dst: VReg,
        base: Base,
        offset: i32,
        ty: LoadTy,
    },
    Store {
        base: Base,
        offset: i32,
        src: Operand,
        width: Width,
    },
    MemCpy {
        dst_base: Base,
        dst_off: i32,
        src_base: Base,
        src_off: i32,
        len: u32,
    },

    // integer / fixed
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

    // Bit operations
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
    },
    LShr {
        dst: VReg,
        a: VReg,
        b_or_imm: Operand,
    },
    AShr {
        dst: VReg,
        a: VReg,
        b_or_imm: Operand,
    }, // Keeps the sign when rotating

    // Set value conditionally (e.g. boolean)
    SetCond {
        dst: VReg,
        cond: Cond,
        a: VReg,
        b: Operand,
    },

    // calls (no effects metadata at LIR)
    Call {
        dst: Option<VReg>,
        callee: FunctionId,
        args: Vec<VReg>,
    },
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Cond {
    EQ,
    NE,
    SLT,
    SLE,
    SGT,
    SGE,
    ULT,
    ULE,
    UGT,
    UGE,
}

// It is a bit strange to implement fallthrough on these, with a BR next_block_id that
// gets discarded in lowering.
#[derive(Debug)]
pub enum Terminator {
    Br {
        target: BlockId,
    },
    BrCmp {
        cond: Cond,
        a: VReg,
        b: Operand,
        target: BlockId,
    },
    Ret {
        value: Option<VReg>,
    }, // a bit sceptical on the Option<VReg> but lets keep it for now...
    Hlt,
    //Trap { code: u16 },
}

#[cfg(feature = "debug-info")]
#[derive(Clone, Debug, Default)]
pub struct BlockDbg {
    /// Human label: "if.then", "loop.latch", "prologue", etc.
    pub label: Option<Arc<str>>,
    /// Free-form block description or breadcrumb
    pub comment: Option<Arc<str>>,
}

#[derive(Debug)]
pub struct Block {
    pub id: BlockId,
    pub instructions: Vec<Inst>,
    pub term: Terminator,
    #[cfg(feature = "debug-info")]
    pub dbg: BlockDbg,
}

#[derive(Debug)]
pub struct Function {
    pub id: FunctionId,
    pub entry: BlockId,
    pub blocks: Vec<Block>,
    pub stack_size: u32,
}

#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
}
