/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::aligner::align;
use source_map_node::Node;
use std::cmp::PartialOrd;
use std::fmt::{Alignment, Display, Formatter};
use std::ops::{Add, Div, Sub};
pub mod aligner;
pub mod opcode;
pub mod types;

#[repr(C)] // do not use `packed` for now
#[derive(Clone)]
pub struct BinaryInstruction {
    pub opcode: u8,
    pub padding: u8, // this is just to be explicit about the actual padding.
    pub operands: [u16; 5],
}

#[derive(Copy, Clone, Debug)]
pub struct MemoryAddress(pub u16);

#[derive(Copy, Clone)]
pub struct StackMemoryAddress(pub u16);

#[derive(Copy, Clone)]
pub struct CountU16(pub u16);

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

#[derive(Debug, Copy, Clone)]
pub struct HeapMemoryRegion {
    pub addr: HeapMemoryAddress,
    pub size: MemorySize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct FrameMemoryAddress(pub u16);

impl Display for FrameMemoryAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[frame {:04X} ({})]", self.0, self.0)
    }
}

impl Add<MemoryOffset> for FrameMemoryAddress {
    type Output = FrameMemoryAddress;

    fn add(self, rhs: MemoryOffset) -> Self::Output {
        FrameMemoryAddress(self.0 + rhs.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FrameMemoryRegion {
    pub addr: FrameMemoryAddress,
    pub size: MemorySize,
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
    pub fn new(frame_addr: FrameMemoryAddress, size: MemorySize) -> FrameMemoryRegion {
        Self {
            addr: frame_addr,
            size,
        }
    }

    pub fn last_valid_end_addr(&self) -> FrameMemoryAddress {
        self.addr.add(MemoryOffset(self.size.0))
    }
}

impl FrameMemoryRegion {
    #[must_use]
    pub fn addr(&self) -> FrameMemoryAddress {
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
    pub fn advance(&self, memory_offset: MemoryOffset) -> FrameMemoryAddress {
        FrameMemoryAddress(self.0 + memory_offset.0)
    }
}

/// relative to the frame pointer
impl FrameMemoryAddress {
    #[must_use]
    pub const fn add(&self, memory_size: MemorySize) -> Self {
        Self(self.0 + memory_size.0)
    }

    pub const fn add_offset(&self, memory_offset: MemoryOffset) -> Self {
        Self(self.0 + memory_offset.0)
    }
    #[must_use]
    pub const fn as_size(&self) -> FrameMemorySize {
        FrameMemorySize(self.0)
    }
}

pub fn align_to(addr: MemoryOffset, alignment: MemoryAlignment) -> MemoryOffset {
    MemoryOffset(align(addr.0 as usize, alignment.into()) as u16)
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
/// in a parent struct—this is required for safe and predictable layout
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
    pub fn to_size(&self) -> HeapMemorySize {
        HeapMemorySize(self.0)
    }
}

impl Display for HeapMemoryOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "+{:08X}]", self.0)
    }
}

impl HeapMemoryOffset {
    pub fn space(&mut self, memory_size: HeapMemorySize, alignment: MemoryAlignment) -> Self {
        let start = align(self.0 as usize, alignment.into()) as u32;
        self.0 = start + memory_size.0;
        HeapMemoryOffset(start)
    }
}

impl Add<HeapMemorySize> for HeapMemoryOffset {
    type Output = Self;

    fn add(self, rhs: HeapMemorySize) -> Self {
        Self(self.0 + rhs.0)
    }
}

impl Sub<HeapMemoryOffset> for HeapMemoryOffset {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        assert!(rhs.0 <= self.0);
        Self(self.0 - rhs.0)
    }
}

impl HeapMemoryOffset {
    pub fn as_size(&self) -> HeapMemorySize {
        HeapMemorySize(self.0)
    }
}

impl HeapMemoryOffset {
    pub fn add(&self, size: HeapMemorySize, alignment: MemoryAlignment) -> HeapMemoryOffset {
        let new_start = align(self.0 as usize, alignment.into());
        HeapMemoryOffset(new_start as u32 + size.0)
    }
}

#[derive(Debug, Copy, Eq, PartialEq, Hash, Clone, Ord, PartialOrd)]
pub struct MemoryOffset(pub u16);

impl MemoryOffset {
    pub fn to_size(&self) -> MemorySize {
        MemorySize(self.0)
    }
}

impl MemoryOffset {
    pub fn space(&mut self, memory_size: MemorySize, alignment: MemoryAlignment) -> Self {
        let start = align(self.0 as usize, alignment.into()) as u16;
        self.0 = start + memory_size.0;
        MemoryOffset(start)
    }
}

impl Add<MemorySize> for MemoryOffset {
    type Output = Self;

    fn add(self, rhs: MemorySize) -> Self {
        Self(self.0 + rhs.0)
    }
}

impl Sub<MemoryOffset> for MemoryOffset {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        assert!(rhs.0 <= self.0);
        Self(self.0 - rhs.0)
    }
}

impl MemoryOffset {
    pub fn as_size(&self) -> MemorySize {
        MemorySize(self.0)
    }
}

impl MemoryOffset {
    pub fn add(&self, size: MemorySize, alignment: MemoryAlignment) -> MemoryOffset {
        let new_start = align(self.0 as usize, alignment.into());
        MemoryOffset(new_start as u16 + size.0)
    }
}

pub enum ZFlagPolarity {
    Normal,
    Inverted,
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
pub struct MemorySize(pub u16);

impl Into<usize> for MemorySize {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl Div<Self> for MemorySize {
    type Output = CountU16;

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

        CountU16(self.0 / rhs.0)
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
    pub const fn greater_than(&self, other: MemoryAlignment) -> bool {
        self.rank() > other.rank()
    }
}

impl Into<usize> for MemoryAlignment {
    fn into(self) -> usize {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
        }
    }
}

impl Into<MemoryOffset> for MemoryAlignment {
    fn into(self) -> MemoryOffset {
        let octets: usize = self.into();
        MemoryOffset(octets as u16)
    }
}

#[must_use]
pub fn align_frame_addr(
    memory_address: FrameMemoryAddress,
    alignment: MemoryAlignment,
) -> FrameMemoryAddress {
    let raw_addr = align(memory_address.0 as usize, alignment.into());

    FrameMemoryAddress(raw_addr as u16)
}

#[must_use]
pub fn align_offset(memory_address: MemoryOffset, alignment: MemoryAlignment) -> MemoryOffset {
    let raw_addr = align(memory_address.0 as usize, alignment.into());

    MemoryOffset(raw_addr as u16)
}

#[derive(Copy, Clone, Debug)]
pub struct FrameMemorySize(pub u16);

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

pub struct Meta {
    pub comment: String,
    pub node: Node,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionPosition(pub u16);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionPositionOffset(pub u16);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionRange {
    pub start: InstructionPosition,
    pub count: InstructionPositionOffset,
}

pub const INT_SIZE: u16 = 4;
pub const FLOAT_SIZE: u16 = 4;
pub const BOOL_SIZE: u16 = 1;

pub const HEAP_PTR_ON_FRAME_SIZE: MemorySize = MemorySize(4);
pub const HEAP_PTR_ON_FRAME_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct VecHeader {
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory) // must be first
    pub count: u16,       // must be second. useful for iterator
    pub capacity: u16,    // capacity is always third
    pub element_size: u16, // size (in bytes) of each element; useful for iterator
}
pub const VEC_HEADER_SIZE: MemorySize = MemorySize(size_of::<VecHeader>() as u16);
pub const VEC_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

pub const VEC_HEADER_COUNT_OFFSET: MemoryOffset = MemoryOffset(4);

pub const VEC_PTR_SIZE: MemorySize = HEAP_PTR_ON_FRAME_SIZE;
pub const VEC_PTR_ALIGNMENT: MemoryAlignment = HEAP_PTR_ON_FRAME_ALIGNMENT;

#[repr(C)]
pub struct VecIterator {
    pub vec_header_heap_ptr: u32,
    pub index: u16,
}

pub const VEC_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<VecIterator>() as u16);
pub const VEC_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
pub struct RangeIterator {
    pub index: i32,
    pub end: i32,
    pub direction: i32,
}

pub const RANGE_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<RangeIterator>() as u16);
pub const RANGE_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct RangeHeader {
    pub min: i32,
    pub max: i32,
    pub inclusive: bool,
}
pub const RANGE_HEADER_SIZE: MemorySize = MemorySize(size_of::<RangeHeader>() as u16);
pub const RANGE_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
pub struct GridHeader {
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory). Pointer should always be first
    pub width: u32,
    pub height: u32,
}

pub const GRID_HEADER_SIZE: MemorySize = MemorySize(size_of::<GridHeader>() as u16);
pub const GRID_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;
pub const GRID_PTR_SIZE: MemorySize = HEAP_PTR_ON_FRAME_SIZE;
pub const GRID_PTR_ALIGNMENT: MemoryAlignment = HEAP_PTR_ON_FRAME_ALIGNMENT;

#[repr(C)]
pub struct MapHeader {
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory). Pointer should always be first
    pub element_count: u32, // Count should be second
    pub capacity: u32,
    pub key_size: u32,
    pub value_size: u32,
}

pub const MAP_HEADER_SIZE: MemorySize = MemorySize(size_of::<MapHeader>() as u16);
pub const MAP_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;
pub const MAP_HEADER_COUNT_OFFSET: MemoryOffset = MemoryOffset(4);

pub const MAP_PTR_SIZE: MemorySize = HEAP_PTR_ON_FRAME_SIZE;
pub const MAP_PTR_ALIGNMENT: MemoryAlignment = HEAP_PTR_ON_FRAME_ALIGNMENT;

#[repr(C)]
pub struct MapIterator {
    pub map_header_frame_offset: u32,
    pub index: u32,
}

pub const MAP_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<MapIterator>() as u16);
pub const MAP_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
pub struct StringHeader {
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory). Pointer should always be first
    pub byte_count: u32,  // Count should be second
    pub capacity: u32,
}

pub const STRING_HEADER_SIZE: MemorySize = MemorySize(size_of::<StringHeader>() as u16);
pub const STRING_HEADER_COUNT_OFFSET: MemoryOffset = MemoryOffset(4);
pub const STRING_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

pub const STRING_PTR_SIZE: MemorySize = HEAP_PTR_ON_FRAME_SIZE;
pub const STRING_PTR_ALIGNMENT: MemoryAlignment = HEAP_PTR_ON_FRAME_ALIGNMENT;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SliceHeader {
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory). Pointer should always be first
    pub element_count: u16,
    pub element_size: u16,
}

pub const SLICE_HEADER_SIZE: MemorySize = MemorySize(size_of::<SliceHeader>() as u16);
pub const SLICE_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;
pub const SLICE_PTR_OFFSET: MemoryOffset = MemoryOffset(0);
pub const SLICE_COUNT_OFFSET: MemoryOffset = MemoryOffset(4);

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SlicePairHeader {
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory). Pointer should always be first
    pub element_count: u16,
    pub key_size: u16,
    pub value_size: u16,
}

pub const SLICE_PAIR_HEADER_SIZE: MemorySize = MemorySize(size_of::<SlicePairHeader>() as u16);
pub const SLICE_PAIR_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;
pub const SLICE_PAIR_PTR_OFFSET: MemoryOffset = MemoryOffset(0);
pub const SLICE_PAIR_COUNT_OFFSET: MemoryOffset = MemoryOffset(4);
