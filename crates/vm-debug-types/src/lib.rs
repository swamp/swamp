use std::fmt::{Display, Formatter, Write};
use swamp_vm_types::{
    FrameMemoryAddress, FrameMemorySize, HeapMemoryAddress, HeapMemoryOffset, InstructionPosition,
    InstructionPositionOffset, InstructionRange, MemoryOffset, MemorySize,
};
use swamp_vm_types::{FrameMemoryRegion, MemoryAlignment, align_to};
use yansi::Paint;

impl FrameMemoryInfo {
    pub fn get(&self, memory_addr: &FrameMemoryAddress) -> Option<FrameAddressInfo> {
        for x in &self.infos {
            if x.region.addr.0 == memory_addr.0 {
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
    pub ty: BasicType,
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<OffsetMemoryItem>,
    pub total_size: MemorySize,
    pub total_alignment: MemoryAlignment,
}

#[derive(Clone, Debug)]
pub struct TupleType {
    pub fields: Vec<OffsetMemoryItem>,
    pub total_size: MemorySize,
    pub total_alignment: MemoryAlignment,
}

#[derive(Clone, Debug)]
pub enum TaggedUnionDataKind {
    Struct(StructType),
    Tuple(TupleType),
    Empty,
}

#[derive(Clone, Debug)]
pub struct TaggedUnionData {
    pub kind: TaggedUnionDataKind,
    pub name: String,
}

impl TaggedUnionData {
    #[must_use]
    pub const fn payload_size(&self) -> MemorySize {
        match &self.kind {
            TaggedUnionDataKind::Struct(st) => st.total_size,
            TaggedUnionDataKind::Tuple(tt) => tt.total_size,
            TaggedUnionDataKind::Empty => MemorySize(0),
        }
    }
    #[must_use]
    pub const fn payload_alignment(&self) -> MemoryAlignment {
        match &self.kind {
            TaggedUnionDataKind::Struct(st) => st.total_alignment,
            TaggedUnionDataKind::Tuple(tt) => tt.total_alignment,
            TaggedUnionDataKind::Empty => MemoryAlignment::U8,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TaggedUnion {
    pub name: String,
    pub tag_offset: MemoryOffset, // should always be 0
    pub tag_size: MemorySize,
    pub variants: Vec<TaggedUnionData>,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

impl TaggedUnion {
    pub fn payload_offset(&self) -> MemoryOffset {
        align_to(MemoryOffset(self.tag_size.0), self.max_alignment)
    }
}

impl TaggedUnion {
    #[must_use]
    pub fn get_variant_by_index(&self, index: usize) -> &TaggedUnionData {
        &self.variants[index]
    }
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
    Ip(InstructionPosition),
    ImmediateU32(u32),
    ImmediateU16(u16),
    MemorySize(MemorySize),
    ImmediateU8(u16),
    CountU16(u16),
    HeapAddress(HeapMemoryAddress),
    WriteIndirectHeapWithOffset(FrameMemoryAddress, HeapMemoryOffset, DecoratedMemoryKind),
    ReadIndirectHeapWithOffset(FrameMemoryAddress, HeapMemoryOffset, DecoratedMemoryKind),
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
pub struct MemoryElement {
    pub offset: MemoryOffset,
    pub size: MemorySize,
    pub alignment: MemoryAlignment,
}

#[derive(Clone, Debug)]
pub enum BasicTypeKind {
    Empty,
    U8,
    B8,
    S32,
    Fixed32,
    U32,
    InternalStringHeader,
    InternalRangeHeader,
    InternalVecHeader,
    InternalMapHeader,
    InternalGridHeader,
    InternalVecIterator,
    InternalMapIterator,
    //InternalGridIterator,
    Struct(StructType),
    TaggedUnion(TaggedUnion),
    Tuple(TupleType),
    Optional(Box<BasicType>),
    Slice(Box<BasicType>),
    SlicePair(Box<OffsetMemoryItem>, Box<OffsetMemoryItem>),
}
#[derive(Clone, Debug)]
pub struct BasicType {
    pub kind: BasicTypeKind,
    pub total_size: MemorySize,
    pub total_alignment: MemoryAlignment,
}

impl Display for BasicType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

impl Display for BasicTypeKind {
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
            Self::Empty => {
                write!(f, "()")
            }
            Self::B8 => {
                write!(f, "bool8")
            }
            Self::Fixed32 => {
                write!(f, "fixed32")
            }
            Self::InternalStringHeader => {
                write!(f, "str")
            }
            Self::InternalRangeHeader => {
                write!(f, "range<>")
            }
            Self::InternalVecHeader => {
                write!(f, "vec<>")
            }
            Self::InternalMapHeader => {
                write!(f, "map<>")
            }
            Self::InternalGridHeader => {
                write!(f, "grid<>")
            }
            Self::InternalVecIterator => {
                write!(f, "vec_iter")
            }
            Self::InternalMapIterator => {
                write!(f, "map_iter")
            }
            Self::Optional(basic) => {
                write!(f, "optional<{basic}>")
            }
            Self::Slice(basic) => {
                write!(f, "slice {basic}")
            }
            Self::SlicePair(a, b) => {
                write!(f, "slice {a:?} {b:?}")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariableInfo {
    pub is_mutable: bool,
    pub name: String,
}

impl Display for VariableInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut_prefix = if self.is_mutable { "mut " } else { "" };

        let name = &self.name;
        write!(f, "{mut_prefix}{name}")
    }
}

#[derive(Clone, Debug)]
pub enum FrameAddressInfoKind {
    Variable(VariableInfo),
    Parameter(VariableInfo),
    Return,
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
            Self::Return => {
                write!(f, "return: ")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct FrameAddressInfo {
    pub region: FrameMemoryRegion,
    pub kind: FrameAddressInfoKind,
    pub ty: BasicType,
}

#[derive(Clone, Debug)]
pub struct FrameMemoryInfo {
    pub infos: Vec<FrameAddressInfo>,
    pub size: FrameMemorySize,
}

#[derive(Clone)]
pub struct FrameRelativeInfo {
    pub frame_memory_region: FrameMemoryRegion,
    pub kind: FrameAddressInfo,
}

impl FrameMemoryInfo {
    #[must_use]
    pub fn size(&self) -> FrameMemorySize {
        self.size
    }
}

#[derive(Clone, Debug)]
pub enum FunctionInfoKind {
    Constant(usize),
    Normal(usize),
}

#[derive(Clone, Debug)]
pub struct FunctionInfo {
    pub kind: FunctionInfoKind,
    pub frame_memory: FrameMemoryInfo,
    pub name: String,
    pub ip_range: InstructionRange,
}

#[derive(Clone, Debug)]
pub struct CompleteFunctionInfo {
    pub ip: InstructionPosition,
    pub size: InstructionPositionOffset,
    pub info: FunctionInfo,
}

pub fn new_line_and_tab(f: &mut dyn Write, tabs: usize) -> std::fmt::Result {
    let tab_str = "  ".repeat(tabs);
    writeln!(f)?;
    write!(f, "{tab_str}")
}

pub fn show_memory_offset(
    offset: MemoryOffset,
    origin: FrameMemoryAddress,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    let result = origin + offset;
    write!(f, "{:04X}+{:04X}", result.0.cyan(), offset.0.yellow())
}
fn show_memory_size(size: MemorySize, f: &mut dyn Write, tabs: usize) -> std::fmt::Result {
    write!(f, "{:X}", size.0.green())
}

pub fn show_offset_item(
    offset_item: &OffsetMemoryItem,
    origin: FrameMemoryAddress,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    show_memory_offset(offset_item.offset, origin, f, tabs)?;
    write!(f, ":")?;
    show_memory_size(offset_item.size, f, tabs)?;
    write!(f, " ")?;
    write_identifier_and_colon(&offset_item.name, f)?;
    write_basic_type(&offset_item.ty, origin, f, tabs + 1)
}

pub fn show_struct_type(
    s: &StructType,
    origin: FrameMemoryAddress,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    write!(f, "{{")?;
    for offset_item in &s.fields {
        new_line_and_tab(f, tabs + 1)?;
        show_offset_item(offset_item, origin, f, tabs + 1)?;
    }
    write!(f, " }}")
}

pub fn show_tuple_type(
    s: &TupleType,
    origin: FrameMemoryAddress,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    write!(f, "(")?;
    for offset_item in &s.fields {
        new_line_and_tab(f, tabs + 1)?;
        let local_origin = origin + offset_item.offset;
        show_offset_item(offset_item, local_origin, f, tabs + 1)?;
    }
    write!(f, " )")
}

pub fn write_identifier_and_colon(identifier: &str, f: &mut dyn Write) -> std::fmt::Result {
    write!(f, "{}:", identifier.blue())
}

pub fn write_basic_type(
    ty: &BasicType,
    origin: FrameMemoryAddress,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    match &ty.kind {
        BasicTypeKind::Empty => write!(f, "()"),
        BasicTypeKind::U8 => write!(f, "{}", "u8".white()),
        BasicTypeKind::B8 => write!(f, "{}", "b8".white()),
        BasicTypeKind::S32 => write!(f, "{}", "s32".white()),
        BasicTypeKind::Fixed32 => write!(f, "{}", "f32".white()),
        BasicTypeKind::U32 => write!(f, "{}", "u32".white()),
        BasicTypeKind::Struct(s) => show_struct_type(s, origin, f, tabs),
        BasicTypeKind::TaggedUnion(tagged_union) => {
            write!(f, "union tag_size:")?;
            show_memory_size(tagged_union.tag_size, f, tabs)?;
            for (index, union_data) in tagged_union.variants.iter().enumerate() {
                new_line_and_tab(f, tabs + 1)?;
                write!(f, "{}> ", index.bright_magenta())?;
                write_identifier_and_colon(&union_data.name, f)?;
                match &union_data.kind {
                    TaggedUnionDataKind::Struct(struct_type) => {
                        show_struct_type(struct_type, origin, f, tabs + 2)?;
                    }
                    TaggedUnionDataKind::Tuple(tuple_type) => {
                        show_tuple_type(tuple_type, origin, f, tabs + 2)?;
                    }
                    TaggedUnionDataKind::Empty => {}
                }
            }
            Ok(())
        }
        BasicTypeKind::Tuple(tuple_type) => show_tuple_type(tuple_type, origin, f, tabs),
        BasicTypeKind::Optional(inner_type) => {
            write!(f, "Option")?;
            write_basic_type(inner_type, origin, f, tabs)
        }
        BasicTypeKind::Slice(slice_inner_type) => {
            write!(f, "[|")?;
            write_basic_type(slice_inner_type, origin, f, tabs + 1)?;
            write!(f, "|]")
        }
        BasicTypeKind::SlicePair(key_type, value_type) => {
            write!(f, "[|")?;
            show_offset_item(key_type, origin, f, tabs + 1)?;
            write!(f, ", ")?;
            show_offset_item(value_type, origin, f, tabs + 1)?;
            write!(f, "|]")
        }
        BasicTypeKind::InternalStringHeader => {
            write!(f, "str")
        }
        BasicTypeKind::InternalRangeHeader => {
            write!(f, "range")
        }
        BasicTypeKind::InternalVecHeader => {
            write!(f, "vec<>")
        }
        BasicTypeKind::InternalMapHeader => {
            write!(f, "map<>")
        }
        BasicTypeKind::InternalGridHeader => {
            write!(f, "grid<>")
        }
        BasicTypeKind::InternalVecIterator => {
            write!(f, "vec_iter")
        }
        BasicTypeKind::InternalMapIterator => {
            write!(f, "map_iter")
        }
    }
}

pub fn show_frame_addr(
    addr: FrameMemoryAddress,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    write!(f, "{:04X}", addr.0.blue())
}

pub fn show_frame_region(
    region: FrameMemoryRegion,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    show_frame_addr(region.addr, f, tabs)?;
    write!(f, ":")?;
    show_memory_size(region.size, f, tabs)
}

pub fn show_frame_memory(
    frame_relative_infos: &FrameMemoryInfo,
    f: &mut dyn Write,
) -> std::fmt::Result {
    for mem in &frame_relative_infos.infos {
        let addr = mem.region.addr;
        show_frame_region(mem.region, f, 0)?;
        write!(f, " ")?;
        match &mem.kind {
            FrameAddressInfoKind::Variable(v) => {
                write!(f, "var ")?;
                write_identifier_and_colon(&v.name, f)?;
            }
            FrameAddressInfoKind::Parameter(v) => {
                write!(f, "param ")?;
                write_identifier_and_colon(&v.name, f)?;
            }
            FrameAddressInfoKind::Return => {
                write!(f, "return:")?;
            }
        }
        write!(f, " ")?;
        write_basic_type(&mem.ty, addr, f, 0)?;
        writeln!(f)?;
    }

    Ok(())
}
