/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{
    align_to, AggregateMemoryLocation, CountU16, FrameMemoryAddress, FrameMemoryRegion,
    FrameMemorySize, InstructionPositionOffset, InstructionRange, MemoryLocation,
};
use fxhash::FxHasher;
use seq_fmt::comma;
use std::cmp::{max, Ordering};
use std::fmt::{Debug, Display, Formatter, Write};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use swamp_vm_isa::{
    HeapMemoryAddress, HeapMemoryRegion, InstructionPosition, MemoryAlignment,
    MemoryOffset, MemorySize, ProgramCounterDelta, RegIndex,
    HEAP_PTR_ON_FRAME_ALIGNMENT, HEAP_PTR_ON_FRAME_SIZE, MAP_HEADER_ALIGNMENT, MAP_HEADER_SIZE, MAP_ITERATOR_ALIGNMENT,
    MAP_ITERATOR_SIZE, RANGE_HEADER_ALIGNMENT, RANGE_HEADER_SIZE, RANGE_ITERATOR_ALIGNMENT,
    RANGE_ITERATOR_SIZE, STRING_PTR_ALIGNMENT, STRING_PTR_SIZE, VEC_HEADER_SIZE, VEC_ITERATOR_ALIGNMENT,
    VEC_ITERATOR_SIZE, VEC_PTR_ALIGNMENT, VEC_PTR_SIZE,
};
use tracing::error;
use yansi::Paint;

pub type BasicTypeRef = Rc<BasicType>;

#[derive(Clone, Debug)]
pub struct OffsetMemoryItem {
    pub offset: MemoryOffset,
    pub size: MemorySize,
    pub name: String,
    pub ty: BasicTypeRef,
}

impl Display for OffsetMemoryItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.name, self.ty)
    }
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

impl TupleType {
    #[must_use]
    pub fn aligned_size_of_field(&self, field_index: usize) -> MemorySize {
        let max_alignment: usize = self.max_alignment.into();
        MemorySize(max(self.fields[field_index].size.0 as usize, max_alignment) as u32)
    }
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
    pub name: String,     // e.g., "None", "Some"
    pub ty: BasicTypeRef, // the payload type (could be unit/empty)
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
    pub tag_alignment: MemoryAlignment,
    pub tag_size: MemorySize,
    pub payload_max_size: MemorySize,
    pub max_payload_alignment: MemoryAlignment,
    pub payload_offset: MemoryOffset,
    pub variants: Vec<TaggedUnionVariant>,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

impl Display for TaggedUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "union {}:", self.name, )?;
        for (offset, variant) in self.variants.iter().enumerate() {
            writeln!(f, "  {offset}: {variant}")?;
        }
        Ok(())
    }
}

impl TaggedUnion {
    #[must_use]
    pub fn payload_offset(&self) -> MemoryOffset {
        align_to(MemoryOffset(self.tag_size.0), self.max_alignment)
    }
}

impl TaggedUnion {
    #[must_use]
    pub fn get_variant_by_index(&self, index: usize) -> &TaggedUnionVariant {
        debug_assert!(
            index < self.variants.len(),
            "variant out of bounds {index} out of {}",
            self.variants.len()
        );
        &self.variants[index]
    }

    #[must_use]
    pub fn get_variant_as_offset_item(&self, index: usize) -> OffsetMemoryItem {
        let variant = self.get_variant_by_index(index);
        OffsetMemoryItem {
            offset: self.payload_offset,
            size: variant.ty.total_size,
            name: variant.name.clone(),
            ty: variant.ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FrameMemoryAttribute {
    pub is_temporary: bool,
}

#[derive(Debug, Clone)]
pub enum DecoratedOperandAccessKind {
    WriteRegister(TypedRegister, Option<PathInfo>, FrameMemoryAttribute), // register is modified
    ReadRegister(TypedRegister, Option<PathInfo>, FrameMemoryAttribute), // register is only read from
    ReadIndirectPointer(FrameMemoryAddress), // TODO: should probably be removed
    DeltaPc(ProgramCounterDelta),            // for branch
    AbsolutePc(InstructionPosition),         // e.g. call
    MemorySize(MemorySize),
    AbsoluteMemoryPosition(HeapMemoryAddress), // only for loading constants?
    WriteFrameMemoryAddress(FrameMemoryAddress), // e.g. store to frame
    ReadFrameMemoryAddress(FrameMemoryAddress), // e.g., load reg from frame
    WriteBaseRegWithOffset(RegIndex, MemoryOffset),
    ReadBaseRegWithOffset(RegIndex, MemoryOffset),
    ImmediateU8(u8),
    ImmediateU16(u16),
    ImmediateU32(u32),
    CountU16(u16),
    CountU8(u8),
    WriteMask(u8),
    ReadMask(u8),
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
    Pointer,
    U8,
    B8,
    U16,
    S32,
    Fixed32,
    U32,
    StringView {
        byte: BasicTypeRef,
        char: BasicTypeRef,
    },
    Struct(StructType),
    TaggedUnion(TaggedUnion),
    Tuple(TupleType),
    Optional(TaggedUnion),
    InternalRangeHeader,

    // Can not be stored:
    InternalVecIterator,
    InternalStringIterator,
    InternalMapIterator,
    InternalSparseIterator,
    InternalRangeIterator,
    //InternalGridIterator,
    SliceView(BasicTypeRef),

    // Collections
    FixedCapacityArray(BasicTypeRef, usize),
    DynamicLengthVecView(BasicTypeRef),
    VecStorage(BasicTypeRef, usize),
    StringStorage {
        element_type: BasicTypeRef,
        char: BasicTypeRef,
        capacity: usize,
    },
    StackStorage(BasicTypeRef, usize),
    QueueStorage(BasicTypeRef, usize),
    MapStorage {
        key_type: BasicTypeRef,
        value_type: BasicTypeRef,
        logical_limit: usize,
        capacity: CountU16,
    },
    SparseView(BasicTypeRef),
    SparseStorage(BasicTypeRef, usize),

    DynamicLengthMapView(Box<OffsetMemoryItem>, Box<OffsetMemoryItem>),
    // DynamicLengthMapView(),
    GridView(BasicTypeRef),
    GridStorage(BasicTypeRef, usize, usize),
    Any,
}

impl BasicTypeKind {
    pub(crate) const fn manifestation(&self) -> Manifestation {
        if self.is_scalar() {
            Manifestation::Scalar
        } else {
            Manifestation::Aggregate
        }
    }

    #[must_use]
    pub fn element(&self) -> Option<BasicTypeRef> {
        match &self {
            Self::StackStorage(inner, _)
            | Self::GridView(inner)
            | Self::GridStorage(inner, ..)
            | Self::SparseStorage(inner, _)
            | Self::SparseView(inner)
            | Self::QueueStorage(inner, _)
            | Self::DynamicLengthVecView(inner)
            | Self::FixedCapacityArray(inner, _)
            | Self::VecStorage(inner, _)
            | Self::SliceView(inner) => Some(inner.clone()),

            Self::StringStorage { char, .. } | Self::StringView { char, .. } => Some(char.clone()),

            _ => None,
        }
    }
}

pub enum Manifestation {
    Scalar,
    Aggregate,
}

impl Manifestation {
    #[must_use]
    pub const fn string(&self) -> &str {
        match self {
            Self::Scalar => "scalar",
            Self::Aggregate => "aggregate",
        }
    }
}

impl Display for Manifestation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string())
    }
}

impl BasicTypeKind {
    pub(crate) const fn is_scalar(&self) -> bool {
        matches!(
            self,
            Self::Empty | Self::B8 | Self::U8 | Self::U16 | Self::S32 | Self::U32 | Self::Fixed32
        )
    }

    pub const fn is_reg_copy(&self) -> bool {
        self.is_scalar() || self.is_view()
    }

    pub const fn is_view(&self) -> bool {
        match self {
            BasicTypeKind::GridView(_)
            | BasicTypeKind::DynamicLengthMapView(_, _)
            | BasicTypeKind::SparseView(_)
            | BasicTypeKind::DynamicLengthVecView(_)
            | BasicTypeKind::SliceView(_)
            | BasicTypeKind::StringView { .. } => true,
            _ => false,
        }
    }

    #[must_use]
    pub const fn is_aggregate(&self) -> bool {
        !self.is_scalar()
    }

    #[must_use]
    pub const fn is_mutable_reference(&self) -> bool {
        false // MutablePointer removed - mutability is handled at analyzer level
    }

    pub(crate) const fn is_immutable(&self) -> bool {
        !self.is_mutable_reference()
    }

    #[must_use]
    pub const fn needs_copy_back_when_mutable(&self) -> bool {
        self.is_scalar()
    }

    #[must_use]
    pub fn union_info(&self) -> &TaggedUnion {
        if let Self::TaggedUnion(union) = self {
            union
        } else {
            panic!("wrong type")
        }
    }
}

impl Display for BasicTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "()"),
            Self::Pointer => write!(f, "Ptr"),
            Self::Any => write!(f, "Any"),
            Self::U8 => write!(f, "u8"),
            Self::B8 => write!(f, "b8"),
            Self::U16 => write!(f, "u16"),
            Self::S32 => write!(f, "i32"),
            Self::Fixed32 => write!(f, "Fixed(i32)"),
            Self::U32 => write!(f, "u32"),
            Self::StringView { .. } => write!(f, "String"),
            Self::InternalRangeHeader => write!(f, "Range"),
            Self::FixedCapacityArray(item_type, size) => write!(f, "[{item_type}; {size}]"),
            Self::DynamicLengthVecView(item_type) => write!(f, "Vec<{item_type}>"),
            Self::VecStorage(item_type, size) => write!(f, "Vec<{item_type}, {size}>"),
            Self::StringStorage {
                element_type: byte,
                capacity,
                ..
            } => write!(f, "String<{byte}, {capacity}>"),
            Self::SparseView(item_type) => write!(f, "Sparse<{item_type}>"),
            Self::GridView(item_type) => write!(f, "Grid<{item_type}>"),
            Self::SparseStorage(item_type, size) => write!(f, "SparseStorage<{item_type}, {size}>"),
            Self::StackStorage(item_type, size) => write!(f, "StackStorage<{item_type}, {size}>"),
            Self::GridStorage(item_type, width, height) => {
                write!(f, "GridStorage<{item_type}, ({width},{height})>")
            }
            Self::QueueStorage(item_type, size) => write!(f, "QueueStorage<{item_type}, {size}>"),
            Self::MapStorage {
                logical_limit: logical_size,
                capacity,
                key_type,
                value_type,
                ..
            } => write!(
                f,
                "MapStorage<{key_type}, {value_type}, {logical_size} ({capacity})>",
            ),
            Self::DynamicLengthMapView(key, value) => write!(f, "Map<{key}, {value}>"),
            Self::InternalVecIterator => write!(f, "Vec::Iterator"),
            Self::InternalStringIterator => write!(f, "String::Iterator"),
            Self::InternalMapIterator => write!(f, "Map::Iterator"),
            Self::InternalSparseIterator => write!(f, "Sparse::Iterator"),
            Self::InternalRangeIterator => write!(f, "Range::Iterator"),
            Self::Struct(struct_type) => write!(f, "{}", struct_type.name),
            Self::TaggedUnion(union) => write!(f, "enum {}", union.name),
            Self::Optional(optional) => write!(f, "{}?", optional.get_variant_by_index(1).ty),
            Self::Tuple(tuple_type) => write!(f, "({})", comma(&tuple_type.fields)),
            Self::SliceView(a) => write!(f, "slice<{}>", a.kind),
        }
    }
}

#[must_use]
pub fn pointer_type_again() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::U8, // Generic pointer type
        total_size: MemorySize(4),
        max_alignment: MemoryAlignment::U32,
    })
}

// HACK:
#[must_use]
pub fn string_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::U8,
        total_size: STRING_PTR_SIZE,
        max_alignment: STRING_PTR_ALIGNMENT,
    })
}
// Hack:
#[must_use]
pub fn int_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::S32,
        total_size: MemorySize(4),
        max_alignment: MemoryAlignment::U32,
    })
}

// Hack
#[must_use]
pub fn float_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::Fixed32,
        total_size: MemorySize(4),
        max_alignment: MemoryAlignment::U32,
    })
}

// HACK
#[must_use]
pub fn bytes_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::Empty,
        total_size: MemorySize(0),
        max_alignment: MemoryAlignment::U32,
    })
}

#[must_use]
pub const fn range_type() -> BasicType {
    BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::InternalRangeHeader,
        total_size: RANGE_HEADER_SIZE,
        max_alignment: RANGE_HEADER_ALIGNMENT,
    }
}

#[must_use]
pub const fn range_iter_type() -> BasicType {
    BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::InternalRangeIterator,
        total_size: RANGE_ITERATOR_SIZE,
        max_alignment: RANGE_ITERATOR_ALIGNMENT,
    }
}

#[must_use]
pub fn slice_type() -> BasicType {
    BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::SliceView(BasicTypeRef::from(u8_type())), // todo: fix
        total_size: MAP_HEADER_SIZE,
        max_alignment: MAP_HEADER_ALIGNMENT,
    }
}

// HACK: Used when we don't know the actual type. that should be a rare case.
#[must_use]
pub fn unknown_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::Empty,
        total_size: MemorySize(0),
        max_alignment: MemoryAlignment::U8,
    })
}

#[must_use]
pub fn vec_type() -> BasicType {
    BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::DynamicLengthVecView(BasicTypeRef::from(unknown_type())),
        total_size: VEC_PTR_SIZE,
        max_alignment: VEC_PTR_ALIGNMENT,
    }
}

#[must_use]
pub const fn vec_iter_type() -> BasicType {
    BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::InternalVecIterator,
        total_size: VEC_ITERATOR_SIZE,
        max_alignment: VEC_ITERATOR_ALIGNMENT,
    }
}

#[must_use]
pub fn map_type() -> BasicType {
    BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::DynamicLengthMapView(
            Box::from(OffsetMemoryItem {
                offset: MemoryOffset(0),
                size: MemorySize(0),
                name: String::new(),
                ty: Rc::new(BasicType {
                    id: BasicTypeId::EMPTY,
                    kind: BasicTypeKind::Empty,
                    total_size: MemorySize(0),
                    max_alignment: MemoryAlignment::U8,
                }),
            }),
            Box::from(OffsetMemoryItem {
                offset: MemoryOffset(0),
                size: MemorySize(0),
                name: String::new(),
                ty: Rc::new(BasicType {
                    id: BasicTypeId::EMPTY,
                    kind: BasicTypeKind::Empty,
                    total_size: MemorySize(0),
                    max_alignment: MemoryAlignment::U8,
                }),
            }),
        ),

        total_size: MAP_HEADER_SIZE,
        max_alignment: MAP_HEADER_ALIGNMENT,
    }
}

#[must_use]
pub const fn map_iter_type() -> BasicType {
    BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::InternalMapIterator,
        total_size: MAP_ITERATOR_SIZE,
        max_alignment: MAP_ITERATOR_ALIGNMENT,
    }
}

// HACK: a way to go around the cache. must be fixed.
#[must_use]
pub fn u32_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::U32,
        total_size: MemorySize(4),
        max_alignment: MemoryAlignment::U32,
    })
}

#[must_use]
pub fn u16_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::U16,
        total_size: MemorySize(2),
        max_alignment: MemoryAlignment::U16,
    })
}

// HACK:
#[must_use]
pub fn u8_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::U8,
        total_size: MemorySize(1),
        max_alignment: MemoryAlignment::U8,
    })
}

// HACK: should go through the cache
#[must_use]
pub fn b8_type() -> BasicTypeRef {
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::B8,
        total_size: MemorySize(1),
        max_alignment: MemoryAlignment::U8,
    })
}

// This is a hack, used when we don't know what
// the actual type is
#[must_use]
pub fn pointer_type() -> BasicTypeRef {
    // This is a
    Rc::new(BasicType {
        id: BasicTypeId::EMPTY,
        kind: BasicTypeKind::U8, // Generic pointer type
        total_size: HEAP_PTR_ON_FRAME_SIZE,
        max_alignment: HEAP_PTR_ON_FRAME_ALIGNMENT,
    })
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
    ty: BasicTypeRef,
}

impl HeapPlacedType {
    #[must_use]
    pub const fn new(addr: HeapMemoryAddress, ty: BasicTypeRef) -> Self {
        Self { addr, ty }
    }

    #[must_use]
    pub fn region(&self) -> HeapMemoryRegion {
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
    pub fn size(&self) -> MemorySize {
        self.ty.total_size
    }

    #[must_use]
    pub const fn ty(&self) -> &BasicTypeRef {
        &self.ty
    }
}

#[derive(Clone, Debug)]
pub enum Destination {
    Unit, // no output
    Register(TypedRegister),
    Memory(MemoryLocation),
}

impl Destination {
    #[must_use]
    pub const fn vm_type(&self) -> Option<&VmType> {
        match self {
            Self::Unit => None,
            Self::Register(reg) => Some(&reg.ty),
            Self::Memory(mem) => Some(&mem.ty),
        }
    }
}

impl Display for Destination {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "unit ()"),
            Self::Register(reg) => write!(f, "reg: {reg}"),
            Self::Memory(memory_location) => write!(f, "memory_location {memory_location:?}"),
        }
    }
}

impl Destination {
    #[must_use]
    pub const fn memory_location(&self) -> Option<&MemoryLocation> {
        match self {
            Self::Unit => None,
            Self::Register(_) => None,
            Self::Memory(memory_location) => Some(memory_location),
        }
    }
}

impl Destination {
    #[must_use]
    pub fn add_offset(&self, offset: MemoryOffset, vm_type: VmType) -> Self {
        match self {
            Self::Unit => {
                panic!("add_offset")
            }
            Self::Register(reg) => {
                if reg.ty.is_aggregate() {
                    Self::Memory(MemoryLocation {
                        base_ptr_reg: reg.clone(),
                        offset,
                        ty: vm_type,
                    })
                } else {
                    panic!(
                        "can not add offset to a register since it wasn't an aggregate {}",
                        reg.ty
                    )
                }
            }
            Self::Memory(memory_location) => Self::Memory(MemoryLocation {
                base_ptr_reg: memory_location.base_ptr_reg.clone(),
                offset: memory_location.offset + offset,
                ty: vm_type,
            }),
        }
    }
    #[must_use]
    pub const fn new_unit() -> Self {
        Self::Unit
    }
    #[must_use]
    pub const fn new_reg(register: TypedRegister) -> Self {
        Self::Register(register)
    }
    #[must_use]
    pub const fn new_location(memory_location: MemoryLocation) -> Self {
        Self::Memory(memory_location)
    }

    #[must_use]
    pub const fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }
    #[must_use]
    pub const fn is_memory_location(&self) -> bool {
        matches!(self, Self::Memory(_))
    }

    #[must_use]
    pub const fn is_register(&self) -> bool {
        matches!(self, Self::Register(_))
    }
    #[must_use]
    pub fn ty(&self) -> &BasicTypeRef {
        match self {
            Self::Unit => panic!("no type"),
            Self::Register(reg) => &reg.ty.basic_type,
            Self::Memory(location) => &location.ty.basic_type,
        }
    }

    #[must_use]
    pub const fn register(&self) -> Option<&TypedRegister> {
        match self {
            Self::Register(reg) => Some(reg),
            _ => None,
        }
    }

    #[must_use]
    pub const fn register_involved_in_destination(&self) -> Option<&TypedRegister> {
        match self {
            Self::Register(reg) => Some(reg),
            Self::Memory(memory_location) => Some(memory_location.reg()),
            Self::Unit => None,
        }
    }

    #[must_use]
    pub fn grab_register(&self) -> &TypedRegister {
        match self {
            Self::Register(reg) => reg,
            Self::Memory(_) => {
                panic!("assumed it would be a register")
            }
            Self::Unit => panic!("assumed it would be a register, but was unit"),
        }
    }

    #[must_use]
    pub fn grab_memory_location(&self) -> &MemoryLocation {
        match self {
            Self::Register(_reg) => panic!("assumed it was a memory location"),
            Self::Memory(location) => location,
            Self::Unit => {
                panic!("assumed it would be a memory location, but was unit")
            }
        }
    }

    /// Use if you know for a fact that the Destination is a memory location
    /// Either a `base_reg` + `offset` or a normal reg (usually from a variable)
    #[must_use]
    pub fn memory_location_or_pointer_reg(&self) -> MemoryLocation {
        match self {
            Self::Register(reg) => MemoryLocation {
                base_ptr_reg: reg.clone(),
                offset: MemoryOffset(0),
                ty: reg.ty.clone(),
            },
            Self::Memory(location) => location.clone(),
            Self::Unit => {
                panic!("assumed it would be a memory location, but was unit")
            }
        }
    }

    #[must_use]
    pub fn grab_aggregate_memory_location_or_pointer_reg(&self) -> AggregateMemoryLocation {
        AggregateMemoryLocation {
            location: self.memory_location_or_pointer_reg(),
        }
    }
    #[must_use]
    pub fn grab_aggregate_memory_location(&self) -> AggregateMemoryLocation {
        AggregateMemoryLocation {
            location: self.grab_memory_location().clone(),
        }
    }
}

#[derive(Clone)]
pub struct TypedRegister {
    pub index: u8,
    pub ty: VmType,
    pub comment: String,
}

impl PartialEq<Self> for TypedRegister {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for TypedRegister {}

impl Ord for TypedRegister {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}
impl PartialOrd<Self> for TypedRegister {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl TypedRegister {
    #[must_use]
    pub fn comment(&self) -> &str {
        &self.comment
    }
}

impl TypedRegister {
    #[must_use]
    pub fn new_empty_reserved() -> Self {
        Self {
            index: 0xff,
            ty: VmType::new_unknown_placement(BasicTypeRef::from(unknown_type())),
            comment: String::new(),
        }
    }
}

impl TypedRegister {
    #[must_use]
    pub fn final_type(&self) -> BasicTypeRef {
        self.ty.basic_type.clone()
    }
}

impl Debug for TypedRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "r{} ({} - {})", self.index, self.ty, self.comment)
    }
}

impl Display for TypedRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "r{}", self.index)
    }
}

impl TypedRegister {
    #[must_use]
    pub fn new_frame_placed(index: u8, frame_placed: FramePlacedType) -> Self {
        Self {
            index,
            ty: VmType::new_frame_placed(frame_placed),
            comment: String::new(),
        }
    }

    #[must_use]
    pub const fn new_vm_type(index: u8, ty: VmType) -> Self {
        Self {
            index,
            ty,
            comment: String::new(),
        }
    }

    pub fn with_comment(&mut self, comment: &str) -> &mut Self {
        self.comment = comment.to_string();
        self
    }
    #[must_use]
    pub const fn addressing(&self) -> u8 {
        self.index
    }

    #[must_use]
    pub const fn ty(&self) -> &BasicTypeRef {
        &self.ty.basic_type
    }

    #[must_use]
    pub fn frame_placed(&self) -> FramePlacedType {
        if let Some(fp) = self.ty.frame_placed_type() {
            fp
        } else {
            panic!("")
        }
    }

    #[must_use]
    pub fn size(&self) -> MemorySize {
        self.ty.basic_type.total_size
    }

    #[must_use]
    pub fn addr(&self) -> FrameMemoryAddress {
        self.frame_placed().addr
    }

    #[must_use]
    pub fn region(&self) -> FrameMemoryRegion {
        self.frame_placed().region()
    }

    #[must_use]
    pub fn underlying(&self) -> BasicTypeRef {
        self.ty.basic_type.clone()
    }
}

#[derive(Clone, Debug)]
pub enum VmTypeOrigin {
    Unknown,
    InsideReg,
    Frame(FrameMemoryRegion),
    Heap(HeapMemoryRegion), // Constants only?
}

impl Display for VmTypeOrigin {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown => Ok(()),
            Self::Frame(region) => write!(f, "frame {region}"),
            Self::Heap(region) => write!(f, "heap {region}"),
            Self::InsideReg => write!(f, "reg"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VmType {
    pub basic_type: BasicTypeRef,
    pub origin: VmTypeOrigin,
}

impl VmType {
    #[must_use]
    pub fn is_collection_like(&self) -> bool {
        self.basic_type.is_collection_like()
    }
}

impl VmType {
    #[must_use]
    pub fn is_mutable_primitive(&self) -> bool {
        self.basic_type.is_mutable_reference()
            && self
            .basic_type
            .should_be_copied_back_when_mutable_arg_or_return()
    }

    #[must_use]
    pub fn element_count_always_same_as_capacity(&self) -> bool {
        self.basic_type.element_count_always_same_as_capacity()
    }
}

impl Display for VmType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            tinter::bright_green(&self.basic_type),
            self.origin,
            tinter::bright_magenta(self.manifestation())
        )
    }
}

impl VmType {
    #[must_use]
    pub fn new_frame_placed(frame_placed: FramePlacedType) -> Self {
        Self {
            basic_type: frame_placed.ty.clone(),
            origin: VmTypeOrigin::Frame(frame_placed.region()),
        }
    }
    #[must_use]
    pub fn is_immutable(&self) -> bool {
        self.basic_type.kind.is_immutable()
    }

    #[must_use]
    pub fn manifestation(&self) -> Manifestation {
        self.basic_type.manifestation()
    }

    #[must_use]
    pub const fn new_heap_placement(
        basic_type: BasicTypeRef,
        heap_region: HeapMemoryRegion,
    ) -> Self {
        Self {
            basic_type,
            origin: VmTypeOrigin::Heap(heap_region),
        }
    }

    #[must_use]
    pub const fn new_contained_in_register(basic_type: BasicTypeRef) -> Self {
        Self {
            basic_type,
            origin: VmTypeOrigin::InsideReg,
        }
    }

    #[must_use]
    pub const fn new_unknown_placement(basic_type: BasicTypeRef) -> Self {
        Self {
            basic_type,
            origin: VmTypeOrigin::Unknown,
        }
    }

    #[must_use]
    pub const fn new_basic_with_origin(basic_type: BasicTypeRef, origin: VmTypeOrigin) -> Self {
        Self { basic_type, origin }
    }

    #[must_use]
    pub fn frame_placed_type(&self) -> Option<FramePlacedType> {
        if let VmTypeOrigin::Frame(region) = self.origin {
            Some(FramePlacedType {
                addr: region.addr,
                ty: self.basic_type.clone(),
            })
        } else {
            None
        }
    }

    #[must_use]
    pub fn is_mutable_reference_semantic(&self) -> bool {
        self.basic_type.is_mutable_reference()
    }

    #[must_use]
    pub fn is_aggregate(&self) -> bool {
        self.basic_type.is_aggregate()
    }

    #[must_use]
    pub fn is_scalar(&self) -> bool {
        self.basic_type.is_scalar()
    }

    pub fn is_reg_copy(&self) -> bool {
        self.basic_type.is_reg_copy()
    }

    #[must_use]
    pub fn needs_allocated_space_for_return_in_reg0(&self) -> bool {
        self.basic_type.is_aggregate()
    }

    #[must_use]
    pub fn needs_copy_back_for_mutable(&self) -> bool {
        !self.basic_type.is_aggregate()
    }
    #[must_use]
    pub const fn basic_type(&self) -> &BasicTypeRef {
        &self.basic_type
    }
}

/// Represents a type that has been allocated to a frame relative address
#[derive(Clone, Debug)]
pub struct FramePlacedType {
    addr: FrameMemoryAddress,
    ty: BasicTypeRef,
}

impl FramePlacedType {
    #[must_use]
    pub const fn new(addr: FrameMemoryAddress, ty: BasicTypeRef) -> Self {
        Self { addr, ty }
    }

    #[must_use]
    pub fn region(&self) -> FrameMemoryRegion {
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
    pub fn size(&self) -> MemorySize {
        self.ty.total_size
    }

    #[must_use]
    pub fn ty(&self) -> &BasicType {
        &self.ty
    }

    #[must_use]
    pub fn final_type(&self) -> BasicTypeRef {
        self.ty.clone()
    }

    #[must_use]
    pub fn move_with_offset(&self, offset: MemoryOffset, ty: BasicTypeRef) -> Self {
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

    #[must_use]
    pub fn move_to_optional_some_payload(&self) -> Self {
        self.move_to_optional_payload_variant(1)
    }

    #[must_use]
    pub fn move_to_optional_none_payload(&self) -> Self {
        self.move_to_optional_payload_variant(0)
    }

    #[must_use]
    pub fn move_to_optional_payload_variant(&self, index: usize) -> Self {
        let optional_info = self.ty.optional_info().unwrap();
        Self::new(
            self.addr + optional_info.payload_offset,
            optional_info.get_variant_by_index(index).ty.clone(),
        )
    }

    #[must_use]
    pub fn move_to_optional_tag(&self) -> Self {
        let optional_info = self.ty().optional_info().unwrap();

        let addr = self.addr() + optional_info.tag_offset;
        assert_eq!(optional_info.tag_size.0, 1);

        let tag_type = Rc::new(BasicType {
            id: BasicTypeId::EMPTY,
            kind: BasicTypeKind::B8,
            total_size: MemorySize(1),
            max_alignment: MemoryAlignment::U8,
        });

        Self::new(addr, tag_type)
    }
}

pub enum BoundsCheck {
    KnownSizeAtCompileTime(u16),
    RegisterWithMaxCount(TypedRegister),
}

#[derive(Clone, Debug)]
pub struct BasicTypeId(pub u32);

impl BasicTypeId {
    pub const EMPTY: Self = Self(0xffff_ffff);
}

#[derive(Clone, Debug)]
pub struct BasicType {
    pub id: BasicTypeId,
    pub kind: BasicTypeKind,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

impl BasicType {
    #[must_use]
    pub const fn is_vec_like(&self) -> bool {
        matches!(
            self.kind,
            BasicTypeKind::VecStorage(..)
                | BasicTypeKind::DynamicLengthVecView(..)
                | BasicTypeKind::StringStorage { .. }
                | BasicTypeKind::StringView { .. }
                | BasicTypeKind::StackStorage(..)
                | BasicTypeKind::QueueStorage(..)
                | BasicTypeKind::SliceView(..)
                | BasicTypeKind::FixedCapacityArray(..)
        )
    }

    #[must_use]
    pub fn element(&self) -> Option<BasicTypeRef> {
        self.kind.element()
    }

    #[must_use]
    pub const fn is_collection(&self) -> bool {
        matches!(
            self.kind,
            BasicTypeKind::VecStorage(..)
                | BasicTypeKind::StringStorage { .. }
                | BasicTypeKind::StringView { .. }
                | BasicTypeKind::SliceView(..)
                | BasicTypeKind::FixedCapacityArray(..)
                | BasicTypeKind::DynamicLengthVecView(..)
                | BasicTypeKind::MapStorage { .. }
                | BasicTypeKind::DynamicLengthMapView(..)
                | BasicTypeKind::GridStorage(..)
                | BasicTypeKind::GridView(..)
                | BasicTypeKind::SparseStorage(..)
                | BasicTypeKind::SparseView(..)
                | BasicTypeKind::StackStorage(..)
                | BasicTypeKind::QueueStorage(..)
        )
    }

    #[must_use]
    pub fn might_contain_collections(&self) -> bool {
        match &self.kind {
            BasicTypeKind::Struct(struct_type) => struct_type
                .fields
                .iter()
                .any(|field| field.ty.is_collection() || field.ty.might_contain_collections()),
            BasicTypeKind::Tuple(tuple_type) => tuple_type
                .fields
                .iter()
                .any(|field| field.ty.is_collection() || field.ty.might_contain_collections()),
            BasicTypeKind::TaggedUnion(union_type) => union_type.variants.iter().any(|variant| {
                variant.ty.is_collection() || variant.ty.might_contain_collections()
            }),
            BasicTypeKind::Optional(union_type) => union_type.variants.iter().any(|variant| {
                variant.ty.is_collection() || variant.ty.might_contain_collections()
            }),
            BasicTypeKind::MapStorage { value_type, .. } => {
                value_type.is_collection() || value_type.might_contain_collections()
            }
            _ => false,
        }
    }

    pub(crate) const fn element_count_always_same_as_capacity(&self) -> bool {
        matches!(self.kind, BasicTypeKind::FixedCapacityArray(..))
    }
    #[must_use]
    pub const fn is_collection_like(&self) -> bool {
        matches!(
            self.kind,
            BasicTypeKind::FixedCapacityArray(..)
            // String
            | BasicTypeKind::StringView { .. }
            | BasicTypeKind::StringStorage { .. }

            // Vec
            | BasicTypeKind::VecStorage(..)
            | BasicTypeKind::StackStorage(..)
            | BasicTypeKind::DynamicLengthVecView(..)
            // Map
            | BasicTypeKind::MapStorage {  ..}
            | BasicTypeKind::DynamicLengthMapView(..)
        )
    }
    #[must_use]
    pub const fn is_collection_with_capacity(&self) -> bool {
        matches!(
            self.kind,
            BasicTypeKind::FixedCapacityArray(..)
            // Vec
            | BasicTypeKind::VecStorage(..)
            | BasicTypeKind::StringStorage {..}
            // Map
            | BasicTypeKind::MapStorage { ..}
        )
    }
}

impl BasicType {
    pub(crate) const fn manifestation(&self) -> Manifestation {
        self.kind.manifestation()
    }
}

impl BasicType {
    #[must_use]
    pub const fn should_be_copied_back_when_mutable_arg_or_return(&self) -> bool {
        !self.is_aggregate()
    }
}

impl BasicType {
    #[must_use]
    pub fn referenced_type(&self) -> &Self {
        // MutablePointer removed - mutability handled at analyzer level
        panic!("referenced_type() is no longer supported - MutablePointer was removed")
    }
}

impl BasicType {
    #[must_use]
    pub const fn make_pointer(&self) -> Self {
        // MutablePointer removed - return a generic pointer type
        Self {
            id: BasicTypeId::EMPTY,
            kind: BasicTypeKind::U8,
            total_size: HEAP_PTR_ON_FRAME_SIZE,
            max_alignment: HEAP_PTR_ON_FRAME_ALIGNMENT,
        }
    }
}

impl BasicType {}

impl BasicType {
    #[must_use]
    pub fn get_variant(&self, variant_index: usize) -> &TaggedUnionVariant {
        match &self.kind {
            BasicTypeKind::TaggedUnion(tagged) | BasicTypeKind::Optional(tagged) => {
                tagged.get_variant_by_index(variant_index)
            }
            _ => panic!("type is not a tagged union"),
        }
    }
}

impl BasicType {
    #[must_use]
    pub const fn is_scalar(&self) -> bool {
        self.kind.is_scalar()
    }

    #[must_use]
    pub const fn is_reg_copy(&self) -> bool {
        self.kind.is_reg_copy()
    }

    pub const fn is_scratch_arena_allocated(&self) -> bool {
        matches!(self.kind, BasicTypeKind::StringView {..})
    }

    #[must_use]
    pub const fn needs_hidden_pointer_as_return(&self) -> bool {
        self.kind.is_aggregate() && !self.is_scratch_arena_allocated()
    }

    #[must_use]
    pub const fn is_mutable_reference(&self) -> bool {
        self.kind.is_mutable_reference()
    }
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
    pub fn union_info(&self) -> &TaggedUnion {
        self.kind.union_info()
    }

    #[must_use]
    pub fn create_mutable_pointer(&self) -> Self {
        // MutablePointer removed - return a generic pointer type
        debug_assert!(!self.is_mutable_reference());

        Self {
            id: BasicTypeId::EMPTY,
            kind: BasicTypeKind::U8,
            total_size: HEAP_PTR_ON_FRAME_SIZE,
            max_alignment: HEAP_PTR_ON_FRAME_ALIGNMENT,
        }
    }

    #[must_use]
    pub const fn is_aggregate(&self) -> bool {
        self.kind.is_aggregate()
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
    pub fn bucket_size_for_vec_like(&self) -> Option<MemorySize> {
        match &self.kind {
            BasicTypeKind::FixedCapacityArray(inner, _capacity) => Some(inner.total_size),
            BasicTypeKind::SliceView(inner) => Some(inner.total_size),
            BasicTypeKind::VecStorage(inner, _) => Some(inner.total_size),
            BasicTypeKind::StringStorage {
                element_type: byte, ..
            } => Some(byte.total_size),
            BasicTypeKind::DynamicLengthVecView(inner) => Some(inner.total_size),
            BasicTypeKind::StringView { .. } => Some(MemorySize(1)), // String elements are bytes (u8)
            _ => None,
        }
    }

    #[must_use]
    pub const fn header_size_for_vec_like(&self) -> Option<MemorySize> {
        match &self.kind {
            BasicTypeKind::SliceView(_)
            | BasicTypeKind::DynamicLengthVecView(_)
            | BasicTypeKind::VecStorage(_, _)
            | BasicTypeKind::StringStorage { .. }
            | BasicTypeKind::StringView { .. } => Some(VEC_HEADER_SIZE),
            BasicTypeKind::DynamicLengthMapView(..) => Some(MAP_HEADER_SIZE),
            _ => None,
        }
    }

    #[must_use]
    pub fn element_pair(&self) -> Option<(&OffsetMemoryItem, &OffsetMemoryItem)> {
        match &self.kind {
            BasicTypeKind::DynamicLengthMapView(a, b) => Some((a, b)),
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
    pub fn is_codepoint(&self) -> bool {
        matches!(self.kind, BasicTypeKind::U32)
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
        matches!(self.kind, BasicTypeKind::StringView { .. })
            && self.total_size == STRING_PTR_SIZE
            && self.max_alignment == STRING_PTR_ALIGNMENT
    }

    #[must_use]
    pub fn is_bool(&self) -> bool {
        matches!(self.kind, BasicTypeKind::B8)
            && self.total_size.0 == 1
            && self.max_alignment == MemoryAlignment::U8
    }
    #[must_use]
    pub fn is_byte(&self) -> bool {
        matches!(self.kind, BasicTypeKind::U8)
            && self.total_size.0 == 1
            && self.max_alignment == MemoryAlignment::U8
    }
}

impl Display for BasicType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
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
pub enum VariableInfoKind {
    Variable(VariableInfo),
    Parameter(VariableInfo),
    Return,
}

impl Display for VariableInfoKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(var_info) => {
                write!(f, "{} {var_info}", "var:".white())
            }
            Self::Parameter(var_info) => {
                write!(f, "{} {var_info}", "param:".white())
            }
            Self::Return => {
                write!(f, "{}", "return".white())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct FrameAddressInfo {
    pub kind: VariableInfoKind,
    pub frame_placed_type: FramePlacedType,
}

#[derive(Clone, Debug)]
pub struct VariableRegister {
    pub unique_id_in_function: usize,
    pub variable: VariableInfo,
    pub register: TypedRegister,
}

impl Display for VariableRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.register, self.variable)
    }
}

#[derive(Clone, Debug)]
pub struct VariableRegisterRange {
    pub start: u8,
    pub count: u8,
}

#[derive(Clone, Debug)]
pub struct FrameMemoryInfo {
    pub infos: Vec<FrameAddressInfo>,
    pub total_frame_size: FrameMemorySize,
    pub variable_frame_size: FrameMemorySize,
    pub variable_registers: Vec<VariableRegister>,
    //    pub variable_register_range: VariableRegisterRange,
    pub frame_size_for_variables_except_temp: FrameMemorySize,
}

#[derive(Clone)]
pub struct FrameRelativeInfo {
    pub frame_memory_region: FrameMemoryRegion,
    pub kind: FrameAddressInfo,
}

#[derive(Clone, Debug)]
pub struct PathInfo {
    pub steps: Vec<PathStep>,
}

impl PathInfo {
    #[must_use]
    pub fn convert_to_string(&self) -> String {
        let path = &self.steps;

        let last = path.last().unwrap();

        let names: Vec<_> = path.iter().map(|x| x.item.name.to_string()).collect();

        let types: Vec<_> = path.iter().map(|x| format!("{}", x.item.ty.kind)).collect();

        let names_path = names.join(".");
        let types_path = types.join("->");

        format!(
            "{:04X}:{:X} {} ({})",
            (last.origin + last.item.offset).0.bright_cyan(),
            last.item.ty.total_size.0.yellow(),
            names_path.bright_blue(),
            types_path.yellow(),
        )
    }
}

#[derive(Clone, Debug)]
pub struct PathStep {
    pub item: OffsetMemoryItem,
    pub origin: FrameMemoryAddress,
}

impl PathStep {
    #[must_use]
    pub const fn absolute_address(&self) -> FrameMemoryAddress {
        FrameMemoryAddress(self.origin.0 + self.item.offset.0)
    }
}

impl FrameMemoryInfo {
    #[must_use]
    pub const fn size(&self) -> FrameMemorySize {
        self.total_frame_size
    }

    /// Returns a vector of `OffsetMemoryItem` from root to the one containing the address.
    #[must_use]
    pub fn find_path_to_address_items(&self, target: FrameMemoryAddress) -> Option<PathInfo> {
        for info in &self.infos {
            // Synthesize a root OffsetMemoryItem
            let root_item = OffsetMemoryItem {
                offset: MemoryOffset(0),
                size: info.frame_placed_type.ty.total_size,
                name: info.kind.to_string(),
                ty: info.frame_placed_type.ty.clone(),
            };
            let base_addr = info.frame_placed_type.addr;
            let mut path = Vec::new();
            if find_in_item(&root_item, base_addr, target, &mut path) {
                return Some(PathInfo { steps: path });
            }
        }
        None
    }
}
fn find_in_item(
    item: &OffsetMemoryItem,
    base_addr: FrameMemoryAddress,
    target: FrameMemoryAddress,
    path: &mut Vec<PathStep>,
) -> bool {
    let item_addr = FrameMemoryAddress(base_addr.0 + item.offset.0);
    path.push(PathStep {
        item: item.clone(),
        origin: base_addr,
    });

    if item_addr.0 == target.0 {
        return true;
    }

    match &item.ty.kind {
        BasicTypeKind::Struct(st) => {
            for field in &st.fields {
                if find_in_item(field, item_addr, target, path) {
                    return true;
                }
            }
        }
        BasicTypeKind::Tuple(tt) => {
            for field in &tt.fields {
                if find_in_item(field, item_addr, target, path) {
                    return true;
                }
            }
        }
        BasicTypeKind::TaggedUnion(tu) => {
            for variant in &tu.variants {
                // Synthesize an OffsetMemoryItem for the variant
                let variant_item = OffsetMemoryItem {
                    offset: MemoryOffset(0),
                    size: variant.ty.total_size,
                    name: variant.name.clone(),
                    ty: variant.ty.clone(),
                };
                if find_in_item(&variant_item, item_addr, target, path) {
                    return true;
                }
            }
        }
        BasicTypeKind::Optional(tu) => {
            for variant in &tu.variants {
                let variant_item = OffsetMemoryItem {
                    offset: MemoryOffset(0),
                    size: variant.ty.total_size,
                    name: variant.name.clone(),
                    ty: variant.ty.clone(),
                };
                if find_in_item(&variant_item, item_addr, target, path) {
                    return true;
                }
            }
        }
        BasicTypeKind::SliceView(inner) => {
            let slice_item = OffsetMemoryItem {
                offset: MemoryOffset(0),
                size: inner.total_size,
                name: "slice".to_string(),
                ty: inner.clone(),
            };
            if find_in_item(&slice_item, item_addr, target, path) {
                return true;
            }
        }
        BasicTypeKind::DynamicLengthMapView(a, b) => {
            if find_in_item(a, item_addr, target, path) {
                return true;
            }
            if find_in_item(b, item_addr, target, path) {
                return true;
            }
        }
        _ => {}
    }

    path.pop();
    false
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
    pub params: Vec<BasicTypeRef>,
    pub return_type: VmType,
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
    write!(f, "{}+{:04X}", result, offset.0.yellow())
}
fn show_memory_size(size: MemorySize, f: &mut dyn Write, _tabs: usize) -> std::fmt::Result {
    write!(f, "{} ({})", size.green(), size.0)
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

    for (index, union_data) in tagged_union
        .variants
        .clone()
        .into_iter()
        .take(4)
        .enumerate()
    {
        //iter().enumerate() {
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
    write!(f, "{} {{", s.name)?;
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
    write!(f, "{}: ", identifier.blue())
}

pub fn write_basic_type(
    ty: &BasicType,
    origin: FrameMemoryAddress,
    f: &mut dyn Write,
    tabs: usize,
) -> std::fmt::Result {
    match &ty.kind {
        BasicTypeKind::Any => write!(f, "Any"),
        BasicTypeKind::Pointer => write!(f, "Ptr"),
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
            write!(f, "Option<{}> ", tagged_union.variants[1].ty)?;
            show_tagged_union(tagged_union, origin, f, tabs)
        }
        BasicTypeKind::Tuple(tuple_type) => show_tuple_type(tuple_type, origin, f, tabs),

        BasicTypeKind::SliceView(slice_inner_type) => {
            write!(f, "[|")?;
            write_basic_type(slice_inner_type, origin, f, tabs + 1)?;
            write!(f, "|]")
        }
        BasicTypeKind::DynamicLengthMapView(key_type, value_type) => {
            write!(f, "[|")?;
            show_offset_item(key_type, origin, f, tabs + 1)?;
            write!(f, ", ")?;
            show_offset_item(value_type, origin, f, tabs + 1)?;
            write!(f, "|]")
        }
        BasicTypeKind::StringView { .. } => {
            write!(f, "str")
        }
        BasicTypeKind::InternalRangeHeader => {
            write!(f, "range")
        }

        BasicTypeKind::FixedCapacityArray(element_type, size) => {
            write!(f, "[{element_type}, {size}]")
        }
        BasicTypeKind::DynamicLengthVecView(element_type) => {
            write!(f, "Vec<{element_type}>")
        }
        BasicTypeKind::VecStorage(element_type, size) => {
            write!(f, "VecStorage<{element_type}, {size}>")
        }
        BasicTypeKind::StringStorage {
            element_type: byte,
            capacity,
            ..
        } => {
            write!(f, "StringStorage<{byte}, {capacity}>")
        }
        BasicTypeKind::QueueStorage(element_type, size) => {
            write!(f, "QueueStorage<{element_type}, {size}>")
        }
        BasicTypeKind::GridStorage(element_type, width, height) => {
            write!(f, "GridStorage<{element_type}, ({width},{height})>")
        }
        BasicTypeKind::StackStorage(element_type, size) => {
            write!(f, "StackStorage<{element_type}, {size}>")
        }
        BasicTypeKind::SparseView(element_type) => {
            write!(f, "Sparse<{element_type}>")
        }
        BasicTypeKind::GridView(element_type) => {
            write!(f, "grid<{element_type}>")
        }
        BasicTypeKind::SparseStorage(element_type, size) => {
            write!(f, "SparseStorage<{element_type}, {size}>")
        }
        BasicTypeKind::MapStorage {
            logical_limit: logical_size,
            key_type,
            value_type,
            ..
        } => {
            write!(f, "MapStorage<{key_type}, {value_type}, {logical_size}>", )
        }
        BasicTypeKind::InternalVecIterator => {
            write!(f, "vec_iter")
        }
        BasicTypeKind::InternalStringIterator => {
            write!(f, "str_iter")
        }
        BasicTypeKind::InternalRangeIterator => {
            write!(f, "range_iter")
        }
        BasicTypeKind::InternalMapIterator => {
            write!(f, "map_iter")
        }
        BasicTypeKind::InternalSparseIterator => {
            write!(f, "sparse_iter")
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
    use_color: bool,
) -> std::fmt::Result {
    if use_color {
        yansi::enable();
    } else {
        yansi::disable();
    }
    show_frame_addr(region.addr, f, tabs)?;
    write!(f, ":")?;
    show_memory_size(region.size, f, tabs)
}

pub fn show_frame_memory(
    frame_relative_infos: &FrameMemoryInfo,
    f: &mut dyn Write,
    use_color: bool,
) -> std::fmt::Result {
    for mem in &frame_relative_infos.infos {
        let addr = mem.frame_placed_type.addr;
        show_frame_region(mem.frame_placed_type.region(), f, 0, use_color)?;
        write!(f, " ")?;
        match &mem.kind {
            VariableInfoKind::Variable(v) => {
                write!(f, "var ")?;
                write_identifier_and_colon(&v.name, f)?;
            }
            VariableInfoKind::Parameter(v) => {
                write!(f, "param ")?;
                write_identifier_and_colon(&v.name, f)?;
            }
            VariableInfoKind::Return => {
                write!(f, "return:")?;
            }
        }
        write!(f, " ")?;
        write_basic_type(&mem.frame_placed_type.ty, addr, f, 0)?;
        writeln!(f)?;
    }

    writeln!(
        f,
        "frame size for variables: {:0}",
        frame_relative_infos.frame_size_for_variables_except_temp
    )?;
    Ok(())
}

impl Hash for BasicType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Remember: do not include the application-specific ID in the universal hash
        // Only hash the structural components
        self.kind.hash(state);
        self.total_size.0.hash(state);
        self.max_alignment.hash(state);
    }
}

impl Hash for BasicTypeKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);

        match self {
            Self::Any => {}
            Self::Pointer => {}
            Self::Empty => {}
            Self::U8 => {}
            Self::B8 => {}
            Self::U16 => {}
            Self::S32 => {}
            Self::Fixed32 => {}
            Self::U32 => {}
            Self::InternalRangeHeader => {}
            Self::InternalVecIterator => {}
            Self::InternalStringIterator => {}
            Self::InternalMapIterator => {}
            Self::InternalSparseIterator => {}
            Self::InternalRangeIterator => {}

            Self::StringView { byte, char } => {
                byte.universal_hash(state);
                char.universal_hash(state);
            }

            Self::Struct(struct_type) => {
                struct_type.hash(state);
            }

            Self::TaggedUnion(union) => {
                union.hash(state);
            }

            Self::Tuple(tuple_type) => {
                tuple_type.hash(state);
            }

            Self::Optional(optional) => {
                optional.hash(state);
            }

            Self::SliceView(inner) => {
                inner.universal_hash(state);
            }

            Self::FixedCapacityArray(inner, size) => {
                inner.universal_hash(state);
                size.hash(state);
            }

            Self::DynamicLengthVecView(inner) => {
                inner.universal_hash(state);
            }

            Self::VecStorage(inner, size) => {
                inner.universal_hash(state);
                size.hash(state);
            }

            Self::StringStorage {
                element_type,
                char,
                capacity,
            } => {
                element_type.universal_hash(state);
                char.universal_hash(state);
                capacity.hash(state);
            }

            Self::StackStorage(inner, size) => {
                inner.universal_hash(state);
                size.hash(state);
            }

            Self::QueueStorage(inner, size) => {
                inner.universal_hash(state);
                size.hash(state);
            }

            Self::MapStorage {
                key_type,
                value_type,
                logical_limit,
                capacity,
            } => {
                key_type.universal_hash(state);
                value_type.universal_hash(state);
                logical_limit.hash(state);
                capacity.0.hash(state);
            }

            Self::SparseView(inner) => {
                inner.universal_hash(state);
            }

            Self::SparseStorage(inner, size) => {
                inner.universal_hash(state);
                size.hash(state);
            }

            Self::DynamicLengthMapView(key, value) => {
                key.hash(state);
                value.hash(state);
            }

            Self::GridView(inner) => {
                inner.universal_hash(state);
            }

            Self::GridStorage(inner, width, height) => {
                inner.universal_hash(state);
                width.hash(state);
                height.hash(state);
            }
        }
    }
}

impl Hash for StructType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        // Hash fields in a deterministic order
        for field in &self.fields {
            field.hash(state);
        }
        self.total_size.0.hash(state);
        self.max_alignment.hash(state);
    }
}

impl Hash for TupleType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash fields in order
        for field in &self.fields {
            field.hash(state);
        }
        self.total_size.0.hash(state);
        self.max_alignment.hash(state);
    }
}

impl Hash for TaggedUnion {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.tag_offset.0.hash(state);
        self.tag_alignment.hash(state);
        self.tag_size.0.hash(state);
        self.payload_max_size.0.hash(state);
        self.max_payload_alignment.hash(state);
        self.payload_offset.0.hash(state);

        // Hash variants in order
        for variant in &self.variants {
            variant.hash(state);
        }

        self.total_size.0.hash(state);
        self.max_alignment.hash(state);
    }
}

impl Hash for TaggedUnionVariant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.ty.universal_hash(state);
    }
}

impl Hash for OffsetMemoryItem {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.offset.0.hash(state);
        self.size.0.hash(state);
        self.name.hash(state);
        self.ty.universal_hash(state);
    }
}

// Add universal hash methods to BasicType
impl BasicType {
    /// Computes a universal hash based on the type's structure, independent of application-specific IDs.
    /// This is useful for RPC calls and serialization in general.
    ///
    /// Two types with the same structure will have the same universal hash across different applications,
    /// even if they have different `BasicTypeId` values.
    pub fn universal_hash<H: Hasher>(&self, state: &mut H) {
        self.hash(state);
    }

    /// Computes a universal hash and returns it as an u64.
    #[must_use]
    pub fn universal_hash_u64(&self) -> u64 {
        let mut hasher = FxHasher::default();
        self.universal_hash(&mut hasher);
        hasher.finish()
    }
}
