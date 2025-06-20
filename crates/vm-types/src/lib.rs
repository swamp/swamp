/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::aligner::align;
use crate::types::BasicTypeRef;
use crate::types::{TypedRegister, VmType};
use hashmap_mem::MapHeader;
use source_map_node::Node;
use std::cmp::PartialOrd;
use std::fmt::{Alignment, Debug, Display, Formatter};
use std::ops::{Add, Div, Sub};

pub mod aligner;
pub mod opcode;
pub mod prelude;
pub mod types;

/// An instruction is always 6 bytes.
#[repr(C)]
#[derive(Clone)]
pub struct BinaryInstruction {
    pub opcode: u8,
    pub operands: [u8; 8], // Do not increase the size
}

#[derive(Clone, Debug)]
pub struct RegIndex(pub u8);

impl Display for RegIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct MemoryAddress(pub u32);

#[derive(Copy, Clone)]
pub struct StackMemoryAddress(pub u32);

impl Add<MemorySize> for StackMemoryAddress {
    type Output = Self;

    fn add(self, rhs: MemorySize) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Add<MemoryOffset> for StackMemoryAddress {
    type Output = Self;

    fn add(self, rhs: MemoryOffset) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CountU16(pub u16);

impl Display for CountU16 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Copy, Clone)]
pub struct CountU32(pub u32);

impl StackMemoryAddress {
    #[must_use]
    pub const fn add(&self, memory_size: MemorySize) -> Self {
        Self(self.0 + memory_size.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct HeapMemoryAddress(pub u32);
impl Display for HeapMemoryAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:08X}", self.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct HeapMemoryRegion {
    pub addr: HeapMemoryAddress,
    pub size: MemorySize,
}

impl Display for HeapMemoryRegion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.addr, self.size)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct FrameMemoryAddress(pub u32);

impl Display for FrameMemoryAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:04X}", self.0)
    }
}

impl Add<MemoryOffset> for FrameMemoryAddress {
    type Output = Self;

    fn add(self, rhs: MemoryOffset) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FrameMemoryRegion {
    pub addr: FrameMemoryAddress,
    pub size: MemorySize,
}

impl Display for FrameMemoryRegion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.addr, self.size)
    }
}

impl Default for FrameMemoryRegion {
    fn default() -> Self {
        Self {
            addr: FrameMemoryAddress(0),
            size: MemorySize(0),
        }
    }
}

impl FrameMemoryRegion {
    #[must_use]
    pub const fn new(frame_addr: FrameMemoryAddress, size: MemorySize) -> Self {
        Self {
            addr: frame_addr,
            size,
        }
    }

    #[must_use]
    pub fn last_valid_end_addr(&self) -> FrameMemoryAddress {
        self.addr.add(MemoryOffset(self.size.0))
    }
}

impl FrameMemoryRegion {
    #[must_use]
    pub const fn addr(&self) -> FrameMemoryAddress {
        self.addr
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FrameMemoryAddressIndirectPointer(pub FrameMemoryAddress);

#[derive(Debug, Copy, Clone)]
pub struct TempFrameMemoryAddress(pub FrameMemoryAddress);

impl TempFrameMemoryAddress {
    #[must_use]
    pub const fn to_addr(&self) -> FrameMemoryAddress {
        self.0
    }
}

impl FrameMemoryAddress {
    #[must_use]
    pub const fn advance(&self, memory_offset: MemoryOffset) -> Self {
        Self(self.0 + memory_offset.0)
    }
}

/// relative to the frame pointer
impl FrameMemoryAddress {
    #[must_use]
    pub const fn add(&self, memory_size: MemorySize) -> Self {
        Self(self.0 + memory_size.0)
    }

    #[must_use]
    pub const fn add_offset(&self, memory_offset: MemoryOffset) -> Self {
        Self(self.0 + memory_offset.0)
    }
    #[must_use]
    pub const fn as_size(&self) -> FrameMemorySize {
        FrameMemorySize(self.0)
    }
}

#[must_use]
pub fn align_to(addr: MemoryOffset, alignment: MemoryAlignment) -> MemoryOffset {
    MemoryOffset(align(addr.0 as usize, alignment.into()) as u32)
}

/// # Arguments
/// * `offset` - The offset after the last field (end of layout).
/// * `base_offset` - The starting offset of the struct/tuple/union.
/// * `max_alignment` - The maximum alignment required by any field.
///
/// # Returns
/// The total size, rounded up to `max_alignment`.
/// # Notes
/// The total size of a struct is always rounded up to a multiple of its alignment.
/// It might be strange in that it "wastes" memory for the potential parent struct
/// to place items of lower memory alignment. (reuse tail padding).
/// It simplifies things as well with code generation and similar, that a struct
/// is always the same size and doesn't have to rely on where the struct is contained.
/// It also ensures that arrays of the struct are correctly aligned according to the ABI,
/// and matches the behavior of C, C++, and Rust.
/// Note: The tail padding at the end of a struct is not reused for subsequent fields
/// in a parent struct-this is required for safe and predictable layout
#[must_use]
pub fn adjust_size_to_alignment(
    unaligned_size: MemorySize,
    max_alignment: MemoryAlignment,
) -> MemorySize {
    align_to(MemoryOffset(unaligned_size.0), max_alignment).to_size()
}

impl MemoryAddress {
    #[must_use]
    pub const fn space(&self, memory_size: MemorySize, _alignment: Alignment) -> Self {
        Self(self.0 + memory_size.0)
    }
}

#[derive(Debug, Copy, Eq, PartialEq, Hash, Clone, Ord, PartialOrd)]
pub struct HeapMemoryOffset(pub u32);

impl HeapMemoryOffset {
    #[must_use]
    pub const fn to_size(&self) -> HeapMemorySize {
        HeapMemorySize(self.0)
    }
}

impl Display for HeapMemoryOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "+{:08X}]", self.0)
    }
}

impl Add<HeapMemorySize> for HeapMemoryOffset {
    type Output = Self;

    fn add(self, rhs: HeapMemorySize) -> Self {
        Self(self.0 + rhs.0)
    }
}

impl Sub<Self> for HeapMemoryOffset {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        assert!(rhs.0 <= self.0);
        Self(self.0 - rhs.0)
    }
}

impl HeapMemoryOffset {
    #[must_use]
    pub const fn as_size(&self) -> HeapMemorySize {
        HeapMemorySize(self.0)
    }
}

impl HeapMemoryOffset {
    #[must_use]
    pub fn add(&self, size: HeapMemorySize, alignment: MemoryAlignment) -> Self {
        let new_start = align(self.0 as usize, alignment.into());
        Self(new_start as u32 + size.0)
    }
}

#[derive(Clone)]
pub struct PointerLocation {
    pub ptr_reg: TypedRegister,
}

impl PointerLocation {
    #[must_use]
    pub const fn new(ptr_reg: TypedRegister) -> Self {
        Self { ptr_reg }
    }
    #[must_use]
    pub const fn addressing(&self) -> u8 {
        self.ptr_reg.addressing()
    }
}

impl PointerLocation {
    #[must_use]
    pub fn memory_location(&self) -> MemoryLocation {
        MemoryLocation {
            base_ptr_reg: self.ptr_reg.clone(),
            offset: MemoryOffset(0),
            ty: self.ptr_reg.ty.clone(),
        }
    }
}

#[derive(Clone)]
pub struct MemoryLocation {
    pub base_ptr_reg: TypedRegister,
    pub offset: MemoryOffset,
    pub ty: VmType,
}

impl Display for MemoryLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}+{} ({})]", self.base_ptr_reg, self.offset, self.ty)
    }
}

impl MemoryLocation {
    #[must_use]
    pub const fn vm_type(&self) -> &VmType {
        &self.ty
    }

    #[must_use]
    pub fn unsafe_add_offset(&self, offset: MemoryOffset) -> Self {
        Self {
            base_ptr_reg: self.base_ptr_reg.clone(),
            offset: self.offset.add(offset),
            ty: self.ty.clone(),
        }
    }

    #[must_use]
    pub fn new_copy_over_whole_type_with_zero_offset(base_ptr_reg: TypedRegister) -> Self {
        Self {
            ty: base_ptr_reg.ty.clone(),
            base_ptr_reg,
            offset: MemoryOffset(0),
        }
    }
}

impl Debug for MemoryLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "MemLoc[{}+{:04X}] ({})",
            self.base_ptr_reg, self.offset.0, self.ty
        )
    }
}

impl MemoryLocation {
    #[must_use]
    pub fn pointer_location(&self) -> Option<PointerLocation> {
        if self.offset.0 == 0 {
            Some(PointerLocation {
                ptr_reg: self.base_ptr_reg.clone(),
            })
        } else {
            None
        }
    }

    #[must_use]
    pub const fn reg(&self) -> &TypedRegister {
        &self.base_ptr_reg
    }

    #[must_use]
    pub const fn as_direct_register(&self) -> Option<&TypedRegister> {
        if self.offset.0 == 0 {
            Some(&self.base_ptr_reg)
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct ScalarMemoryLocation {
    pub location: MemoryLocation,
}
#[derive(Clone)]
pub struct AggregateMemoryLocation {
    pub location: MemoryLocation,
}

impl Display for AggregateMemoryLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.location)
    }
}

impl AggregateMemoryLocation {
    #[must_use]
    pub fn offset(&self, memory_offset: MemoryOffset, new_type: BasicTypeRef) -> Self {
        let new_location = MemoryLocation {
            base_ptr_reg: self.location.base_ptr_reg.clone(),
            offset: self.location.offset + memory_offset,
            ty: VmType::new_unknown_placement(new_type),
        };
        Self {
            location: new_location,
        }
    }
}

#[derive(Debug, Copy, Eq, PartialEq, Hash, Clone, Ord, PartialOrd)]
pub struct MemoryOffset(pub u32);

impl MemoryOffset {
    #[must_use]
    pub const fn to_size(&self) -> MemorySize {
        MemorySize(self.0)
    }
}

impl Display for MemoryOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "+{:X}", self.0)
    }
}

impl MemoryOffset {
    pub fn space(&mut self, memory_size: MemorySize, alignment: MemoryAlignment) -> Self {
        let start = align(self.0 as usize, alignment.into()) as u32;
        self.0 = start + memory_size.0;
        Self(start)
    }
}

impl Add<MemorySize> for MemoryOffset {
    type Output = Self;

    fn add(self, rhs: MemorySize) -> Self {
        Self(self.0 + rhs.0)
    }
}

impl Add<Self> for MemoryOffset {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self(self.0 + rhs.0)
    }
}

impl Sub<Self> for MemoryOffset {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        assert!(rhs.0 <= self.0);
        Self(self.0 - rhs.0)
    }
}

impl MemoryOffset {
    #[must_use]
    pub const fn as_size(&self) -> MemorySize {
        MemorySize(self.0)
    }
}

impl MemoryOffset {
    #[must_use]
    pub fn add(&self, size: MemorySize, alignment: MemoryAlignment) -> Self {
        let new_start = align(self.0 as usize, alignment.into()) as u32;
        Self(new_start + size.0)
    }
}

pub enum ZFlagPolarity {
    TrueWhenSet,
    TrueWhenClear,
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, Eq, PartialEq)]
pub struct HeapMemorySize(pub u32);

impl Div<Self> for HeapMemorySize {
    type Output = CountU32;

    fn div(self, rhs: Self) -> Self::Output {
        assert!(rhs.0 > 0, "Division by zero in MemorySize");
        assert!(
            self.0 > 0,
            "Numerator must be positive in MemorySize division"
        );
        assert_eq!(
            self.0 % rhs.0,
            0,
            "MemorySize division must be exact and positive"
        );

        CountU32(self.0 / rhs.0)
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, Eq, PartialEq)]
pub struct MemorySize(pub u32);

impl Display for MemorySize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = f64::from(self.0);

        if bytes < 1024.0 {
            write!(f, "{bytes} B")
        } else if bytes < 1024.0 * 1024.0 {
            write!(f, "{:.2} KiB", bytes / 1024.0)
        } else if bytes < 1024.0 * 1024.0 * 1024.0 {
            write!(f, "{:.2} MiB", bytes / (1024.0 * 1024.0))
        } else {
            write!(f, "{:.2} GiB", bytes / (1024.0 * 1024.0 * 1024.0))
        }
    }
}

impl From<MemorySize> for usize {
    fn from(val: MemorySize) -> Self {
        val.0 as Self
    }
}

impl Div<Self> for MemorySize {
    type Output = CountU32;

    fn div(self, rhs: Self) -> Self::Output {
        assert!(rhs.0 > 0, "Division by zero in MemorySize");
        assert!(
            self.0 > 0,
            "Numerator must be positive in MemorySize division"
        );
        assert_eq!(
            self.0 % rhs.0,
            0,
            "MemorySize division must be exact and positive"
        );

        CountU32(self.0 / rhs.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MemoryAlignment {
    // Do not change the order.
    U8,
    U16,
    U32,
    U64,
}

impl MemoryAlignment {
    #[must_use]
    const fn rank(&self) -> usize {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 3,
            Self::U64 => 4,
        }
    }
    #[must_use]
    pub const fn greater_than(&self, other: Self) -> bool {
        self.rank() > other.rank()
    }
}

impl From<MemoryAlignment> for usize {
    fn from(val: MemoryAlignment) -> Self {
        match val {
            MemoryAlignment::U8 => 1,
            MemoryAlignment::U16 => 2,
            MemoryAlignment::U32 => 4,
            MemoryAlignment::U64 => 8,
        }
    }
}

impl From<MemoryAlignment> for u8 {
    fn from(val: MemoryAlignment) -> Self {
        match val {
            MemoryAlignment::U8 => 1,
            MemoryAlignment::U16 => 2,
            MemoryAlignment::U32 => 4,
            MemoryAlignment::U64 => 8,
        }
    }
}

impl TryInto<MemoryAlignment> for usize {
    type Error = ();

    fn try_into(self) -> Result<MemoryAlignment, Self::Error> {
        let converted = match self {
            1 => MemoryAlignment::U8,
            2 => MemoryAlignment::U16,
            4 => MemoryAlignment::U32,
            8 => MemoryAlignment::U64,

            _ => return Err(()),
        };
        Ok(converted)
    }
}

impl From<MemoryAlignment> for MemoryOffset {
    fn from(val: MemoryAlignment) -> Self {
        let octets: usize = val.into();
        Self(octets as u32)
    }
}

#[must_use]
pub fn align_frame_addr(
    memory_address: FrameMemoryAddress,
    alignment: MemoryAlignment,
) -> FrameMemoryAddress {
    let raw_addr = align(memory_address.0 as usize, alignment.into());

    FrameMemoryAddress(raw_addr as u32)
}

#[must_use]
pub fn align_offset(memory_address: MemoryOffset, alignment: MemoryAlignment) -> MemoryOffset {
    let raw_addr = align(memory_address.0 as usize, alignment.into());

    MemoryOffset(raw_addr as u32)
}

#[derive(Copy, Clone, Debug)]
pub struct FrameMemorySize(pub u32);

impl Display for FrameMemorySize {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "frame size: {:04X}", self.0)
    }
}

impl FrameMemorySize {
    #[must_use]
    pub const fn add(&self, inc: MemorySize) -> Self {
        Self(self.0 + inc.0)
    }
}

#[derive(Clone)]
pub struct Meta {
    pub comment: String,
    pub node: Node,
}

#[derive(Debug)]
pub struct PatchPosition(pub InstructionPosition);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct InstructionPosition(pub u32);

impl Add<ProgramCounterDelta> for InstructionPosition {
    type Output = Self;

    fn add(self, rhs: ProgramCounterDelta) -> Self::Output {
        Self(((self.0 as i32) + i32::from(rhs.0)) as u32)
    }
}

impl Sub<Self> for InstructionPosition {
    type Output = ProgramCounterDelta;

    fn sub(self, rhs: Self) -> Self::Output {
        assert!(self.0 >= rhs.0);

        ProgramCounterDelta(((self.0 as i32) - (rhs.0 as i32)) as i16)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ProgramCounterDelta(pub i16);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionPositionOffset(pub u32);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionRange {
    pub start: InstructionPosition,
    pub count: InstructionPositionOffset,
}

pub const INT_SIZE: u16 = 4;
pub const FLOAT_SIZE: u16 = 4;
pub const BOOL_SIZE: u16 = 1;

pub const PTR_SIZE: MemorySize = MemorySize(4);
pub const PTR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

pub const HEAP_PTR_ON_FRAME_SIZE: MemorySize = MemorySize(4);
pub const HEAP_PTR_ON_FRAME_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

pub const REG_ON_FRAME_SIZE: MemorySize = MemorySize(4);
pub const REG_ON_FRAME_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

pub const COLLECTION_CAPACITY_OFFSET: MemoryOffset = MemoryOffset(0); // Capacity should always be first
pub const COLLECTION_ELEMENT_COUNT_OFFSET: MemoryOffset = MemoryOffset(2); // Element count should always be second

#[repr(C)]
#[derive(Copy, Clone)]
pub struct VecHeader {
    /// Do not change the order of the fields!
    ///
    /// Keep the capacity field at the start of the header for consistency across all
    /// container types. Placing it first simplifies copy operations: we can verify
    /// and preserve capacity before copying the remainder of the header in one contiguous operation.
    pub capacity: u16,

    /// Number of live (active) elements currently stored in the collection.
    ///
    /// Always located at offset 2, enabling:
    /// - **Logical size**: Represents the number of valid elements in use.
    /// - **Bounds checking**: Index and assignment checks (`0 <= idx < element_count`)
    ///   can load this field in a single instruction.
    /// - **Iteration**: Iterators read this field to determine the end of the collection.
    /// - **ABI stability**: External tools, debuggers, and serializers can consistently locate
    ///   `capacity` and `element_count` across all container types.
    pub element_count: u16,
}

pub const VEC_HEADER_SIZE: MemorySize = MemorySize(size_of::<VecHeader>() as u32);
pub const VEC_HEADER_PAYLOAD_OFFSET: MemoryOffset = MemoryOffset(size_of::<VecHeader>() as u32);
pub const VEC_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U16;

pub const VEC_PTR_SIZE: MemorySize = HEAP_PTR_ON_FRAME_SIZE;
pub const VEC_PTR_ALIGNMENT: MemoryAlignment = HEAP_PTR_ON_FRAME_ALIGNMENT;

#[repr(C)]
pub struct VecIterator {
    pub vec_header_heap_ptr: u32,
    pub element_size: u16,
    pub index: u16,
}

pub const VEC_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<VecIterator>() as u32);
pub const VEC_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
pub struct SparseIterator {
    pub sparse_header_heap_ptr: u32,
    pub index: u16,
}

pub const SPARSE_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<SparseIterator>() as u32);
pub const SPARSE_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
#[derive(Debug)]
pub struct RangeIterator {
    pub index: i32,
    pub end: i32,
    pub direction: i32,
}

pub const RANGE_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<RangeIterator>() as u32);
pub const RANGE_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct RangeHeader {
    // Do not change! These must match the structure in Swamp core exactly
    pub min: i32,
    pub max: i32,
    pub inclusive: bool,
}
pub const RANGE_HEADER_SIZE: MemorySize = MemorySize(size_of::<RangeHeader>() as u32);
pub const RANGE_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct GridHeader {
    /// Do not change the order of the fields!
    ///
    /// Keep the capacity field at the start of the header for consistency across all
    /// container types. Placing it first simplifies copy operations: we can verify
    /// and preserve capacity before copying the remainder of the header in one contiguous operation.
    pub capacity: u16,

    /// Number of live (active) elements currently stored in the collection.
    ///
    /// Always located at offset 2, enabling:
    /// - **Logical size**: Represents the number of valid elements in use.
    /// - **Bounds checking**: Index and assignment checks (`0 <= idx < element_count`)
    ///   can load this field in a single instruction.
    /// - **Iteration**: Iterators read this field to determine the end of the collection.
    /// - **ABI stability**: External tools, debuggers, and serializers can consistently locate
    ///   `capacity` and `element_count` across all container types.
    pub element_count: u16, // Always same as capacity
    pub element_size: u32,

    pub width: u16,
    pub height: u16,
}

pub const GRID_HEADER_SIZE: MemorySize = MemorySize(size_of::<GridHeader>() as u32);
pub const GRID_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;
pub const GRID_HEADER_PAYLOAD_OFFSET: MemoryOffset = MemoryOffset(12);

// NOTE: Must align to U32, therefor the padding at the end

pub const MAP_HEADER_SIZE: MemorySize = MemorySize(size_of::<MapHeader>() as u32);
pub const MAP_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;
pub const MAP_HEADER_KEY_SIZE_OFFSET: MemoryOffset = MemoryOffset(4);
pub const MAP_HEADER_TUPLE_SIZE_OFFSET: MemoryOffset = MemoryOffset(6);
pub const MAP_HEADER_LOGICAL_LIMIT_OFFSET: MemoryOffset = MemoryOffset(8);
pub const MAP_BUCKETS_OFFSET: MemoryOffset = MemoryOffset(MAP_HEADER_SIZE.0);

#[repr(C)]
pub struct MapIterator {
    pub map_header_frame_offset: u32,
    pub index: u32,
}

pub const MAP_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<MapIterator>() as u32);
pub const MAP_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
#[repr(packed)]
#[derive(Copy, Clone, Debug)]
pub struct StringHeader {
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory). Pointer should always be first
    pub byte_count: u32,  // Count should be second
}

pub const STRING_HEADER_SIZE: MemorySize = MemorySize(size_of::<StringHeader>() as u32);
pub const STRING_HEADER_COUNT_OFFSET: MemoryOffset = MemoryOffset(4);
pub const STRING_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

pub const STRING_PTR_SIZE: MemorySize = HEAP_PTR_ON_FRAME_SIZE;
pub const STRING_PTR_ALIGNMENT: MemoryAlignment = HEAP_PTR_ON_FRAME_ALIGNMENT;
