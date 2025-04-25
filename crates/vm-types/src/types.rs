use crate::MemoryAlignment::U32;
use crate::{
    FrameMemoryAddress, FrameMemoryRegion, FrameMemorySize, HEAP_PTR_ON_FRAME_ALIGNMENT,
    HEAP_PTR_ON_FRAME_SIZE, HeapMemoryAddress, HeapMemoryOffset, HeapMemoryRegion,
    InstructionPosition, InstructionPositionOffset, InstructionRange, MemoryAlignment,
    MemoryOffset, MemorySize, STRING_HEADER_ALIGNMENT, STRING_HEADER_SIZE, align_to,
};
use std::fmt::{Display, Formatter, Write};
use tracing::{error, info};
use yansi::Paint;

impl FrameMemoryInfo {
    pub fn get(&self, memory_addr: &FrameMemoryAddress) -> Option<FrameAddressInfo> {
        for x in &self.infos {
            if x.frame_placed_type.addr.0 == memory_addr.0 {
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
    pub max_alignment: MemoryAlignment,
}

impl Display for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        for field in &self.fields {
            write!(
                f,
                "{:04X}:{:X} {}:{}",
                field.offset.0, field.size.0, field.name, field.ty
            )?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct TupleType {
    pub fields: Vec<OffsetMemoryItem>,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

impl Display for TupleType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for field in &self.fields {
            write!(f, "{:04X}:{:X} {}", field.offset.0, field.size.0, field.ty)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct TaggedUnionVariant {
    pub name: String,  // e.g., "None", "Some"
    pub ty: BasicType, // the payload type (could be unit/empty)
}

impl Display for TaggedUnionVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

#[derive(Clone, Debug)]
pub struct TaggedUnion {
    pub name: String,
    pub tag_offset: MemoryOffset, // should always be 0
    pub tag_size: MemorySize,
    pub payload_max_size: MemorySize,
    pub payload_offset: MemoryOffset,
    pub variants: Vec<TaggedUnionVariant>,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

impl Display for TaggedUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "union {}:", self.name,)?;
        for (offset, variant) in self.variants.iter().enumerate() {
            writeln!(f, "  {offset}: {variant}")?;
        }
        Ok(())
    }
}

impl TaggedUnion {
    pub fn payload_offset(&self) -> MemoryOffset {
        align_to(MemoryOffset(self.tag_size.0), self.max_alignment)
    }
}

impl TaggedUnion {
    #[must_use]
    pub fn get_variant_by_index(&self, index: usize) -> &TaggedUnionVariant {
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
    VecIterator,
    VecHeader,
    IndirectHeapPointer,

    StringHeader,
}

impl DecoratedMemoryKind {
    pub fn to_str(&self) -> &str {
        match self {
            Self::B8 => "b8",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::S32 => "i32",
            Self::Fp32 => "fp32",
            Self::Octets => "*b8",
            Self::VecIterator => "vec_iter",
            Self::VecHeader => "vec_header",
            Self::IndirectHeapPointer => "<heap ptr}",
            Self::StringHeader => "string header",
        }
    }
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
    U16,
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
    InternalRangeIterator,
    //InternalGridIterator,
    Struct(StructType),
    TaggedUnion(TaggedUnion),
    Tuple(TupleType),
    Optional(TaggedUnion),
    Slice(Box<BasicType>),
    SlicePair(Box<OffsetMemoryItem>, Box<OffsetMemoryItem>),
    IndirectHeapPointerOnFrame,
}

#[must_use]
pub const fn int_type() -> BasicType {
    BasicType {
        kind: BasicTypeKind::S32,
        total_size: MemorySize(4),
        max_alignment: MemoryAlignment::U32,
    }
}

#[must_use]
pub const fn u16_type() -> BasicType {
    BasicType {
        kind: BasicTypeKind::U16,
        total_size: MemorySize(2),
        max_alignment: MemoryAlignment::U16,
    }
}

#[must_use]
pub const fn heap_ptr_size() -> BasicType {
    BasicType {
        kind: BasicTypeKind::IndirectHeapPointerOnFrame,
        total_size: HEAP_PTR_ON_FRAME_SIZE,
        max_alignment: HEAP_PTR_ON_FRAME_ALIGNMENT,
    }
}

/// Represents a type that has been allocated to a heap relative address
#[derive(Clone, Debug)]
pub struct HeapPlacedArray {
    addr: HeapMemoryAddress,
    byte_count: u32,
}

impl HeapPlacedArray {
    #[must_use]
    pub const fn new(addr: HeapMemoryAddress, byte_count: u32) -> Self {
        Self { addr, byte_count }
    }

    #[must_use]
    pub const fn addr(&self) -> HeapMemoryAddress {
        self.addr
    }

    #[must_use]
    pub const fn byte_count(&self) -> u32 {
        self.byte_count
    }
}

/// Represents a type that has been allocated to a frame relative address
#[derive(Clone, Debug)]
pub struct HeapPlacedType {
    addr: HeapMemoryAddress,
    ty: BasicType,
}

impl HeapPlacedType {
    #[must_use]
    pub const fn new(addr: HeapMemoryAddress, ty: BasicType) -> Self {
        Self { addr, ty }
    }

    #[must_use]
    pub const fn region(&self) -> HeapMemoryRegion {
        HeapMemoryRegion {
            addr: self.addr,
            size: self.ty.total_size,
        }
    }

    #[must_use]
    pub const fn addr(&self) -> HeapMemoryAddress {
        self.addr
    }

    #[must_use]
    pub const fn size(&self) -> MemorySize {
        self.ty.total_size
    }

    #[must_use]
    pub const fn ty(&self) -> &BasicType {
        &self.ty
    }
}

/// Represents a type that has been allocated to a frame relative address
#[derive(Clone, Debug)]
pub struct FramePlacedType {
    addr: FrameMemoryAddress,
    ty: BasicType,
}

impl FramePlacedType {}

impl FramePlacedType {
    #[must_use]
    pub fn union_payload(&self, index: usize) -> Self {
        let (BasicTypeKind::TaggedUnion(tagged_union) | BasicTypeKind::Optional(tagged_union)) =
            &self.ty.kind
        else {
            panic!("should not work")
        };

        let variant = &tagged_union.variants[index];

        Self {
            addr: self.addr + tagged_union.payload_offset,
            ty: variant.ty.clone(),
        }
    }
}

impl FramePlacedType {
    #[must_use]
    pub const fn new(addr: FrameMemoryAddress, ty: BasicType) -> Self {
        Self { addr, ty }
    }
    #[must_use]
    pub const fn region(&self) -> FrameMemoryRegion {
        FrameMemoryRegion {
            addr: self.addr,
            size: self.ty.total_size,
        }
    }

    #[must_use]
    pub const fn addr(&self) -> FrameMemoryAddress {
        self.addr
    }

    #[must_use]
    pub const fn size(&self) -> MemorySize {
        self.ty.total_size
    }

    #[must_use]
    pub const fn ty(&self) -> &BasicType {
        &self.ty
    }

    #[must_use]
    pub fn move_with_offset(&self, offset: MemoryOffset, ty: BasicType) -> Self {
        Self {
            addr: self.addr + offset,
            ty,
        }
    }

    #[must_use]
    pub fn move_to_field(&self, index: usize) -> Self {
        let offset_info = self.ty.get_field_offset(index).unwrap();
        Self {
            addr: self.addr + offset_info.offset,
            ty: offset_info.ty.clone(),
        }
    }

    pub fn move_to_optional_some_payload(&self) -> Self {
        self.move_to_optional_payload_variant(1)
    }

    pub fn move_to_optional_none_payload(&self) -> Self {
        self.move_to_optional_payload_variant(0)
    }

    #[must_use]
    pub fn move_to_optional_payload_variant(&self, index: usize) -> Self {
        let optional_info = self.ty.optional_info().unwrap();
        Self::new(
            self.addr + optional_info.payload_offset,
            optional_info.variants[index].ty.clone(),
        )
    }

    pub fn move_to_optional_tag(&self) -> Self {
        let optional_info = self.ty().optional_info().unwrap();

        let addr = self.addr() + optional_info.tag_offset;
        assert_eq!(optional_info.tag_size.0, 1);

        let tag_type = BasicType {
            kind: BasicTypeKind::B8,
            total_size: MemorySize(1),
            max_alignment: MemoryAlignment::U8,
        };

        FramePlacedType::new(addr, tag_type)
    }
}

#[derive(Clone, Debug)]
pub struct BasicType {
    pub kind: BasicTypeKind,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

impl BasicType {
    #[must_use]
    pub const fn unwrap_info(
        &self,
    ) -> Option<(MemoryOffset, MemorySize, MemoryOffset, MemorySize)> {
        match &self.kind {
            BasicTypeKind::TaggedUnion(tagged) | BasicTypeKind::Optional(tagged) => Some((
                tagged.tag_offset,
                tagged.tag_size,
                tagged.payload_offset,
                tagged.payload_max_size,
            )),
            _ => None,
        }
    }

    #[must_use]
    pub const fn optional_info(&self) -> Option<&TaggedUnion> {
        match &self.kind {
            BasicTypeKind::Optional(tagged_union) => Some(tagged_union),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_field_offset(&self, index: usize) -> Option<&OffsetMemoryItem> {
        match &self.kind {
            BasicTypeKind::Struct(struct_type) => struct_type.fields.get(index),
            BasicTypeKind::Tuple(tuple_type) => tuple_type.fields.get(index),
            _ => {
                error!(?self, "not a type with fields");
                None
            }
        }
    }

    #[must_use]
    pub fn element(&self) -> Option<&Self> {
        match &self.kind {
            BasicTypeKind::Slice(inner) => Some(inner),
            _ => None,
        }
    }

    #[must_use]
    pub fn element_pair(&self) -> Option<(&OffsetMemoryItem, &OffsetMemoryItem)> {
        match &self.kind {
            BasicTypeKind::SlicePair(a, b) => Some((a, b)),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_int(&self) -> bool {
        matches!(self.kind, BasicTypeKind::S32)
            && self.total_size.0 == 4
            && self.max_alignment == MemoryAlignment::U32
    }
    #[must_use]
    pub fn is_float(&self) -> bool {
        matches!(self.kind, BasicTypeKind::Fixed32)
            && self.total_size.0 == 4
            && self.max_alignment == MemoryAlignment::U32
    }

    #[must_use]
    pub fn is_str(&self) -> bool {
        matches!(self.kind, BasicTypeKind::InternalStringHeader)
            && self.total_size == STRING_HEADER_SIZE
            && self.max_alignment == STRING_HEADER_ALIGNMENT
    }

    #[must_use]
    pub fn is_bool(&self) -> bool {
        matches!(self.kind, BasicTypeKind::B8)
            && self.total_size.0 == 1
            && self.max_alignment == MemoryAlignment::U8
    }
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
            Self::U16 => {
                write!(f, "u16")
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
            Self::TaggedUnion(basic) => {
                write!(f, "tagged_union<{basic}>")
            }
            Self::Optional(basic) => {
                write!(f, "optional<{basic}>")
            }
            Self::Tuple(tuple) => {
                write!(f, "tuple({tuple})")
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
            Self::InternalRangeIterator => {
                write!(f, "range_iter")
            }
            Self::IndirectHeapPointerOnFrame => {
                write!(f, "heap_ptr")
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
    pub kind: FrameAddressInfoKind,
    pub frame_placed_type: FramePlacedType,
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
    _tabs: usize,
) -> std::fmt::Result {
    let result = origin + offset;
    write!(f, "{:04X}+{:04X}", result.0.cyan(), offset.0.yellow())
}
fn show_memory_size(size: MemorySize, f: &mut dyn Write, _tabs: usize) -> std::fmt::Result {
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
    let adjusted_origin = origin + offset_item.offset;
    write_basic_type(&offset_item.ty, adjusted_origin, f, tabs + 1)
}

pub fn show_tagged_union(
    tagged_union: &TaggedUnion,
    origin: FrameMemoryAddress,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    write!(f, "union tag_size:")?;
    show_memory_size(tagged_union.tag_size, f, tabs)?;
    write!(f, " max payload:")?;
    show_memory_size(tagged_union.payload_max_size, f, tabs)?;

    let adjusted_payload_origin = origin + tagged_union.payload_offset;

    for (index, union_data) in tagged_union.variants.iter().enumerate() {
        new_line_and_tab(f, tabs + 1)?;
        write!(f, "{}> ", index.bright_magenta())?;
        write_identifier_and_colon(&union_data.name, f)?;
        write_basic_type(&union_data.ty, adjusted_payload_origin, f, tabs + 1)?;
    }
    Ok(())
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
        show_offset_item(offset_item, origin, f, tabs + 1)?;
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
        BasicTypeKind::U16 => write!(f, "{}", "u16".white()),
        BasicTypeKind::S32 => write!(f, "{}", "s32".white()),
        BasicTypeKind::Fixed32 => write!(f, "{}", "f32".white()),
        BasicTypeKind::U32 => write!(f, "{}", "u32".white()),
        BasicTypeKind::Struct(s) => show_struct_type(s, origin, f, tabs),
        BasicTypeKind::TaggedUnion(tagged_union) => {
            show_tagged_union(tagged_union, origin, f, tabs)
        }
        BasicTypeKind::Optional(tagged_union) => {
            write!(f, "Option")?;
            show_tagged_union(tagged_union, origin, f, tabs)
        }
        BasicTypeKind::Tuple(tuple_type) => show_tuple_type(tuple_type, origin, f, tabs),

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
        BasicTypeKind::IndirectHeapPointerOnFrame => {
            write!(f, "heap_ptr")
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
        BasicTypeKind::InternalRangeIterator => {
            write!(f, "range_iter")
        }
        BasicTypeKind::InternalMapIterator => {
            write!(f, "map_iter")
        }
    }
}

pub fn show_frame_addr(
    addr: FrameMemoryAddress,
    f: &mut dyn Write,
    _tabs: usize,
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
        let addr = mem.frame_placed_type.addr;
        show_frame_region(mem.frame_placed_type.region(), f, 0)?;
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
        write_basic_type(&mem.frame_placed_type.ty, addr, f, 0)?;
        writeln!(f)?;
    }

    Ok(())
}
