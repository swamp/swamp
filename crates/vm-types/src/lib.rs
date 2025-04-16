/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::aligner::align;
use source_map_node::Node;
use std::cmp::PartialOrd;
use std::fmt::{Alignment, Display, Formatter};
use std::ops::{Add, Sub};

pub mod aligner;
pub mod opcode;

#[repr(C, packed)]
#[derive(Clone)]
pub struct BinaryInstruction {
    pub opcode: u8,
    pub operands: [u16; 5],
}

#[derive(Copy, Clone, Debug)]
pub struct MemoryAddress(pub u16);

#[derive(Copy, Clone)]
pub struct StackMemoryAddress(pub u16);

#[derive(Copy, Clone)]
pub struct CountU16(pub u16);

impl StackMemoryAddress {
    #[must_use]
    pub const fn add(&self, memory_size: MemorySize) -> Self {
        Self(self.0 + memory_size.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ConstantMemoryAddress(pub u32);

#[derive(Debug, Copy, Clone)]
pub struct HeapMemoryAddress(pub u32);

#[derive(Debug, Copy, Clone)]
pub struct FrameMemoryAddress(pub u16);

impl Display for FrameMemoryAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[frame {:04X} ({})]", self.0, self.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ConstantMemoryRegion {
    pub addr: ConstantMemoryAddress,
    pub size: MemorySize,
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
        self.addr.add(MemorySize(self.size.0))
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
    offset: MemoryOffset,
    base_offset: MemoryOffset,
    max_alignment: MemoryAlignment,
) -> MemorySize {
    if base_offset > offset {
        panic!("")
    }
    let unaligned_size = offset - base_offset;
    align_to(unaligned_size, max_alignment).to_size()
}

impl MemoryAddress {
    #[must_use]
    pub const fn space(&self, memory_size: MemorySize, _alignment: Alignment) -> Self {
        Self(self.0 + memory_size.0)
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

#[derive(Debug, Copy, Clone, PartialOrd, Ord, Eq, PartialEq)]
pub struct MemorySize(pub u16);

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

pub const INT_SIZE: u16 = 4;
pub const FLOAT_SIZE: u16 = 4;
pub const BOOL_SIZE: u16 = 1;

pub const PTR_SIZE: u16 = 2;
pub const HEAP_PTR_SIZE: u16 = 4;
pub const HEAP_PTR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

pub const VEC_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

pub const STR_SIZE: u16 = VEC_HEADER_SIZE; // TODO: FIX THIS

#[repr(C)]
pub struct VecHeader {
    pub count: u16, // useful for iterator
    pub capacity: u16,
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory)
    pub size: u16,        // size (in bytes) of each element; useful for iterator
}
pub const VEC_HEADER_SIZE: u16 = size_of::<VecHeader>() as u16;
pub const VEC_REFERENCE_SIZE: u16 = HEAP_PTR_SIZE;

pub struct VecIterator {
    pub data_heap_offset: u32,
    pub count: u16,
    pub element_size: u16,
    pub index: u16,
}

pub const VEC_ITERATOR_SIZE: u16 = size_of::<VecIterator>() as u16;

pub const MAP_SIZE: u16 = 2 + 2 + 2 + 2 + 2;
pub const MAP_REFERENCE_SIZE: u16 = HEAP_PTR_SIZE;

pub const RANGE_SIZE: u16 = 2 + 2 + 2;

pub struct StringHeader {
    pub byte_count: u16,
    pub capacity: u16,
    pub heap_offset: u32, // "pointer" to the allocated slice (an offset into memory)
}
pub const STRING_HEADER_SIZE: u16 = size_of::<StringHeader>() as u16;
pub const STRING_REFERENCE_SIZE: u16 = HEAP_PTR_SIZE;
