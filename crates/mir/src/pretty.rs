use ansi_pretty_print::{PrettyPrint, Printer};
use std::fmt::{Display, Formatter};

use crate::{
    Block, FnSig, Function, Inst, LoadTy, Module, Operand, SlotKind, StackSlot, StackSlotId,
    Terminator, VReg, VRegInfo, ValueKind, Width,
};

impl Display for StackSlotId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "slot{}", self.0)
    }
}

impl PrettyPrint for Module {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        p.block("", |p| {
            for func in &self.functions {
                p.line("")?;
                func.pretty(p)?;
                p.line("")?;
            }
            Ok(())
        })
    }
}

impl PrettyPrint for Function {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        p.symbol(&format!("fn {}", self.id))?;
        p.punctuation(" : ")?;
        self.sig.pretty(p)?;
        p.line("")?;

        if !self.vregs.is_empty() {
            // TODO: each vreg on own line
            p.punctuation("  ; vregs: ")?;
            for (i, info) in self.vregs.iter().enumerate() {
                if i > 0 {
                    p.punctuation(", ")?;
                }
                p.literal(&format!("v{}:", i))?;
                info.pretty(p)?;
            }
            p.line("")?;
        }
        if !self.slots.is_empty() {
            p.punctuation("  ; slots: ")?;
            // TODO: Slots on own line, or at least wrapped after N count.
            for (i, s) in self.slots.iter().enumerate() {
                if i > 0 {
                    p.punctuation(", ")?;
                }
                p.literal(&format!("slot{}:", i))?;
                s.pretty(p)?;
            }
            p.line("")?;
        }

        p.with_indent(|p| {
            for b in &self.blocks {
                b.pretty(p)?;
            }
            Ok(())
        })
    }
}

impl PrettyPrint for FnSig {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        p.punctuation("(")?;
        for (i, ty) in self.parameters.iter().enumerate() {
            if i > 0 {
                p.punctuation(", ")?;
            }
            p.symbol(&format!("{}", ty))?;
        }
        p.punctuation(")")?;
        p.punctuation(" -> ")?;
        match self.ret {
            Some(t) => p.symbol(&format!("{}", t))?,
            None => p.punctuation("void")?,
        }
        Ok(())
    }
}

impl PrettyPrint for VRegInfo {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        self.kind.pretty(p)
    }
}

impl PrettyPrint for ValueKind {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            ValueKind::I32 => p.punctuation("i32"),
            ValueKind::Fixed32 => p.punctuation("fixed32"),
            ValueKind::Bool => p.punctuation("bool"),
            ValueKind::Ptr(t) => {
                p.punctuation("&")?;
                p.symbol(&format!("{t}"))
            }
            ValueKind::Raw => p.punctuation("raw"),
        }
    }
}

impl PrettyPrint for StackSlot {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        p.punctuation("{ ")?;
        p.symbol(&format!("{}", self.ty))?;
        p.punctuation(", size=")?;
        p.literal(&format!("{}", self.size))?;
        p.punctuation(", align=")?;
        p.literal(&format!("{}", self.align))?;
        p.punctuation(", ")?;
        match self.kind {
            SlotKind::Local => p.punctuation("local"),
            SlotKind::Spill => p.punctuation("spill"),
            SlotKind::ParamShadow => p.punctuation("param_shadow"),
            SlotKind::SRetBuf => p.punctuation("sret_buf"),
        }?;
        p.punctuation(" }")
    }
}

impl PrettyPrint for Block {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        p.line("")?;
        p.symbol(&format!("{}:", self.id))?;

        p.with_indent(|p| {
            for inst in &self.instructions {
                p.line("")?;
                inst.pretty(p)?;
            }
            p.line("")?;
            p.punctuation(" ")?;
            self.terminator.pretty(p)
        })
    }
}

impl PrettyPrint for Inst {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        use Inst::*;
        match self {
            Mov { dst, src } => {
                p.symbol(&format!("{} = mov ", dst))?;
                src.pretty(p)?;
            }

            AddPtrImm { dst, base, offset } => {
                p.symbol(&format!("{} = addptr ", dst))?;
                p.symbol(&format!("{}", base))?;
                p.punctuation(", ")?;
                p.literal(&format!("{}", offset))?;
            }

            Load {
                dst,
                base,
                offset,
                ty,
            } => {
                p.symbol(&format!("{} = ld.", dst))?;
                ty.pretty(p)?;
                p.punctuation(" [")?;
                p.symbol(&format!("{}", base))?;
                if *offset != 0 {
                    p.punctuation(" + ")?;
                    p.literal(&format!("{}", offset))?;
                }
                p.punctuation("]");
            }
            Store {
                base,
                offset,
                src,
                width,
            } => {
                p.punctuation("st.")?;
                width.pretty(p)?;
                p.punctuation(" [")?;
                p.symbol(&format!("{}", base))?;
                if *offset != 0 {
                    p.punctuation(" + ")?;
                    p.literal(&format!("{}", offset))?;
                }
                p.punctuation("], ")?;
                src.pretty(p)?;
            }

            MemCpy {
                dst_ptr,
                src_ptr,
                len,
            } => {
                p.punctuation("memcpy ")?;
                p.symbol(&format!("{}", dst_ptr))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{}", src_ptr))?;
                p.punctuation(", ")?;
                p.literal(&format!("{}", len))?;
            }

            ClrFp { fp_off, size } => {
                p.punctuation("clrfp ")?;
                p.literal(&format!("{}", fp_off))?;
                p.punctuation(", ")?;
                p.literal(&format!("{}", size))?;
            }

            Add { dst, a, b } => bin3(p, "add", dst, a, b)?,
            Sub { dst, a, b } => bin3(p, "sub", dst, a, b)?,
            Mul { dst, a, b } => bin3(p, "mul", dst, a, b)?,
            SDiv { dst, a, b } => bin3(p, "sdiv", dst, a, b)?,
            SMod { dst, a, b } => bin3(p, "smod", dst, a, b)?,
            SNeg { dst, a } => un3(p, "sneg", dst, a)?,

            FMul { dst, a, b } => {
                p.symbol(&format!("{} = fmul ", dst))?;
                p.symbol(&format!("{}", a))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{}", b))?;
            }
            FDiv { dst, a, b } => {
                p.symbol(&format!("{} = fdiv ", dst))?;
                p.symbol(&format!("{}", a))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{}", b))?;
            }
            IToF { dst, x } => {
                p.symbol(&format!("{} = itof ", dst))?;
                p.symbol(&format!("{}", x))?;
            }

            And { dst, a, b } => bin3(p, "and", dst, a, b)?,
            Or { dst, a, b } => bin3(p, "or", dst, a, b)?,
            Xor { dst, a, b } => bin3(p, "xor", dst, a, b)?,
            Not { dst, a } => {
                p.symbol(&format!("{} = not ", dst))?;
                p.symbol(&format!("{}", a))?;
            }
            Shl { dst, a, b_or_imm } => shift3(p, "shl", dst, a, b_or_imm)?,
            LShr { dst, a, b_or_imm } => shift3(p, "lshr", dst, a, b_or_imm)?,
            AShr { dst, a, b_or_imm } => shift3(p, "ashr", dst, a, b_or_imm)?,

            CmpEq { dst, a, b } => bin3(p, "cmp.eq", dst, a, b)?,
            CmpNe { dst, a, b } => bin3(p, "cmp.ne", dst, a, b)?,
            CmpSLt { dst, a, b } => bin3(p, "cmp.slt", dst, a, b)?,
            CmpSLe { dst, a, b } => bin3(p, "cmp.sle", dst, a, b)?,
            CmpSGt { dst, a, b } => bin3(p, "cmp.sgt", dst, a, b)?,
            CmpSGe { dst, a, b } => bin3(p, "cmp.sge", dst, a, b)?,
            CmpULt { dst, a, b } => bin3(p, "cmp.ult", dst, a, b)?,
            CmpULe { dst, a, b } => bin3(p, "cmp.ule", dst, a, b)?,
            CmpUGt { dst, a, b } => bin3(p, "cmp.ugt", dst, a, b)?,
            CmpUGe { dst, a, b } => bin3(p, "cmp.uge", dst, a, b)?,

            Call { dst, callee, args } => {
                if let Some(d) = dst {
                    p.symbol(&format!("{} = call ", d))?;
                } else {
                    p.punctuation("call ")?;
                }
                p.symbol(&format!("{}", callee))?;
                p.punctuation("(")?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        p.punctuation(", ")?;
                    }
                    p.symbol(&format!("{}", a))?;
                }
                p.punctuation(")");
            }

            Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                p.symbol(&format!("{} = select ", dst))?;
                p.symbol(&format!("{}", cond))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{}", if_true))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{}", if_false))?;
            }

            IMin { dst, a, b } => binr3(p, "imin", dst, a, b)?,
            IMax { dst, a, b } => binr3(p, "imax", dst, a, b)?,
            IClamp { dst, x, lo, hi } => {
                p.symbol(&format!("{dst} = iclamp "))?;
                p.symbol(&format!("{x}"))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{lo}"))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{hi}"))?;
            }
            IRnd { dst, seed } => {
                p.symbol(&format!("{dst} = irnd ", dst))?;
                p.symbol(&format!("{seed}"))?;
            }
        }
        Ok(())
    }
}

impl PrettyPrint for Terminator {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            Terminator::Br { target } => {
                p.punctuation("br ")?;
                p.symbol(&format!("{}", target))?;
            }
            Terminator::BrZ { v, target } => {
                p.punctuation("brz ")?;
                p.symbol(&format!("{}", v))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{}", target))?;
            }
            Terminator::BrNZ { v, target } => {
                p.punctuation("brnz ")?;
                p.symbol(&format!("{}", v))?;
                p.punctuation(", ")?;
                p.symbol(&format!("{}", target))?;
            }
            Terminator::Ret { value } => {
                p.punctuation("ret")?;
                if let Some(v) = value {
                    p.punctuation(" ")?;
                    p.symbol(&format!("{}", v))?;
                }
            }
            Terminator::Hlt => p.punctuation("hlt")?,
        }
        Ok(())
    }
}

impl PrettyPrint for Operand {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            Operand::Reg(v) => p.symbol(&format!("{}", v)),
            Operand::ImmI32(k) => p.literal(&format!("{}", k)),
            Operand::ImmU32(k) => p.literal(&format!("{}", k)),
        }
    }
}

impl PrettyPrint for LoadTy {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            LoadTy::U8 => p.punctuation("u8"),
            LoadTy::I8 => p.punctuation("i8"),
            LoadTy::U16 => p.punctuation("u16"),
            LoadTy::I16 => p.punctuation("i16"),
            LoadTy::I32 => p.punctuation("i32"),
            LoadTy::Fixed32 => p.punctuation("fixed32"),
        }
    }
}

impl PrettyPrint for Width {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            Width::Byte => p.punctuation("b"),
            Width::Half => p.punctuation("h"),
            Width::Word => p.punctuation("w"),
        }
    }
}

fn bin3(p: &mut Printer, name: &str, dst: &VReg, a: &Operand, b: &Operand) -> std::fmt::Result {
    p.symbol(&format!("{} = {} ", dst, name))?;
    a.pretty(p)?;
    p.punctuation(", ")?;
    b.pretty(p)
}
fn binr3(p: &mut Printer, name: &str, dst: &VReg, a: &VReg, b: &VReg) -> std::fmt::Result {
    p.symbol(&format!("{} = {} ", dst, name))?;
    p.symbol(&format!("{}", a))?;
    p.punctuation(", ")?;
    p.symbol(&format!("{}", b))
}
fn un3(p: &mut Printer, name: &str, dst: &VReg, a: &Operand) -> std::fmt::Result {
    p.symbol(&format!("{} = {} ", dst, name))?;
    a.pretty(p)
}
fn shift3(p: &mut Printer, name: &str, dst: &VReg, a: &VReg, b: &Operand) -> std::fmt::Result {
    p.symbol(&format!("{} = {} ", dst, name))?;
    p.symbol(&format!("{}", a))?;
    p.punctuation(", ")?;
    b.pretty(p)
}
