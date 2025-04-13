use seq_map::SeqMap;
use std::fmt::{Display, Formatter};
use swamp_vm_types::{
    ConstantMemoryAddress, FrameMemoryAddress, FrameMemorySize, InstructionPosition, MemoryOffset,
    MemorySize,
};

pub struct FrameMemoryInfo {
    pub infos: Vec<FrameAddressInfo>,
}

impl FrameMemoryInfo {
    pub fn get(&self, memory_addr: &FrameMemoryAddress) -> Option<FrameAddressInfo> {
        for x in &self.infos {
            if x.addr.0 == memory_addr.0 {
                return Some(x.clone());
            }
        }
        None
    }
}

#[derive(Clone, Debug)]
pub struct OffsetMemoryItem {
    pub offset: MemoryOffset,
    pub size: MemorySize,
    pub name: String,
    pub ty: ComplexType,
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub fields: SeqMap<MemoryOffset, OffsetMemoryItem>,
}

#[derive(Clone, Debug)]
pub enum TaggedUnionDataKind {
    Struct(StructType),
    Tuple(Vec<OffsetMemoryItem>),
}

#[derive(Clone, Debug)]
pub struct TaggedUnionData {
    pub kind: TaggedUnionDataKind,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct TaggedUnion {
    pub name: String,
    pub variants: Vec<TaggedUnionData>,
}

#[derive(Debug, Clone)]
pub struct FrameMemoryAttribute {
    pub is_temporary: bool,
}

#[derive(Debug, Clone)]
pub enum DecoratedOperandAccessKind {
    ReadFrameAddress(
        FrameMemoryAddress,
        DecoratedMemoryKind,
        FrameMemoryAttribute,
    ),
    WriteFrameAddress(
        FrameMemoryAddress,
        DecoratedMemoryKind,
        FrameMemoryAttribute,
    ),
    ReadIndirectPointer(FrameMemoryAddress),
    ConstantAddress(ConstantMemoryAddress),
    Ip(InstructionPosition),
    ImmediateU32(u32),
    ImmediateU16(u16),
    MemorySize(MemorySize),
    ImmediateU8(u16),
    //WriteIndirectMemory(MemoryAddress, MemoryOffset, DecoratedMemoryKind),
    //ReadIndirectMemory(MemoryAddress, MemoryOffset, DecoratedMemoryKind),
    CountU16(u16),
    //HeapAddress(HeapMemoryAddress),
}

#[derive(Clone, Debug)]
pub enum DecoratedMemoryKind {
    U8,
    U16,
    U32,
    S32,  // Int
    Fp32, // Float
    B8,   // Bool
    Octets,
    IndirectHeapPointer,
}

pub struct DecoratedOperandOrigin {}

pub struct DecoratedOperand {
    pub kind: DecoratedOperandAccessKind,
    pub origin: DecoratedOperandOrigin,
}

pub struct DecoratedOpcode {
    pub name: String,
    pub operands: Vec<DecoratedOperand>,
}

#[derive(Clone, Debug)]
pub enum BasicType {
    U8,
    S32,
    U32,
    Struct(StructType),
    TaggedUnion(TaggedUnion),
    Tuple(Vec<ComplexType>),
}

impl Display for BasicType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => {
                write!(f, "u8")
            }
            Self::S32 => {
                write!(f, "s32")
            }
            Self::U32 => {
                write!(f, "u32")
            }
            Self::Struct(_) => {
                write!(f, "struct")
            }
            Self::TaggedUnion(_) => {
                write!(f, "tagged_union")
            }
            Self::Tuple(_) => {
                write!(f, "tuple")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum ComplexType {
    Optional(BasicType),
    IndirectPointer(BasicType),
    BasicType(BasicType),
}

impl Display for ComplexType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Optional(basic) => {
                write!(f, "optional<{basic}>")
            }
            Self::IndirectPointer(basic) => {
                write!(f, "ptr to <{basic}>")
            }
            Self::BasicType(basic) => {
                write!(f, "{basic}")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariableInfo {
    pub is_mutable: bool,
    pub name: String,
    pub ty: ComplexType,
}

impl Display for VariableInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut_prefix = if self.is_mutable { "mut " } else { "" };

        let name = &self.name;
        let ty = &self.ty;
        write!(f, "{mut_prefix}{name}: {ty}")
    }
}

#[derive(Clone, Debug)]
pub enum FrameAddressInfoKind {
    Variable(VariableInfo),
    Parameter(VariableInfo),
    Return(ComplexType),
}

impl Display for FrameAddressInfoKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(var_info) => {
                write!(f, "var: {var_info}")
            }
            Self::Parameter(var_info) => {
                write!(f, "param: {var_info}")
            }
            Self::Return(ty) => {
                write!(f, "return: {ty}")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct FrameAddressInfo {
    pub addr: FrameMemoryAddress,
    pub size: FrameMemorySize,
    pub kind: FrameAddressInfoKind,
}
