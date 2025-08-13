pub mod aligner;
pub mod opcode;
pub mod prelude;

use crate::aligner::align;
use hashmap_mem::MapHeader;
use std::fmt::{Alignment, Display, Formatter};
use std::ops::{Add, Div, Sub};

/// An instruction is always 9 bytes.
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

impl From<MemoryAlignment> for MemoryOffset {
    fn from(val: MemoryAlignment) -> Self {
        let octets: usize = val.into();
        Self(octets as u32)
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

#[derive(Copy, Clone, Debug)]
pub struct MemoryAddress(pub u32);

impl MemoryAddress {
    #[must_use]
    pub const fn space(&self, memory_size: MemorySize, _alignment: Alignment) -> Self {
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

#[derive(Copy, Clone)]
pub struct CountU32(pub u32);

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Copy, Clone, Debug)]
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
    pub element_size: u32,
    pub padding: u32,
}

pub const VEC_HEADER_SIZE: MemorySize = MemorySize(size_of::<VecHeader>() as u32);

pub const VEC_HEADER_ELEMENT_COUNT_OFFSET: MemoryOffset = MemoryOffset(2);
pub const VEC_HEADER_PAYLOAD_OFFSET: MemoryOffset = MemoryOffset(size_of::<VecHeader>() as u32);
pub const VEC_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U16;
pub const VEC_HEADER_MAGIC_CODE: u32 = 0xC001C0DE;

pub const VEC_PTR_SIZE: MemorySize = HEAP_PTR_ON_FRAME_SIZE;
pub const VEC_PTR_ALIGNMENT: MemoryAlignment = HEAP_PTR_ON_FRAME_ALIGNMENT;

#[repr(C)]
pub struct VecIterator {
    pub vec_header_heap_ptr: u32,
    pub index: u16,
}

pub const VEC_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<VecIterator>() as u32);
pub const VEC_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct AnyHeader {
    /// VM heap pointer to the actual data
    pub data_ptr: u32,

    /// Size in bytes of the pointed-to data
    pub size: u32,

    /// Universal hash of the type
    pub type_hash: u32,
}

pub const ANY_HEADER_SIZE: MemorySize = MemorySize(size_of::<AnyHeader>() as u32);

pub const ANY_HEADER_PTR_OFFSET: MemoryOffset = MemoryOffset(0);
pub const ANY_HEADER_SIZE_OFFSET: MemoryOffset = MemoryOffset(4);
pub const ANY_HEADER_HASH_OFFSET: MemoryOffset = MemoryOffset(8);
pub const ANY_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

#[repr(C)]
pub struct StringIterator {
    pub string_heap_ptr: u32,
    pub byte_index: u16,
    pub index: u32, // how many times we iterated
}

pub const STRING_ITERATOR_SIZE: MemorySize = MemorySize(size_of::<StringIterator>() as u32);
pub const STRING_ITERATOR_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;

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
#[derive(Copy, Clone, Debug)]
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
    pub padding: u32,
}

pub const GRID_HEADER_SIZE: MemorySize = MemorySize(size_of::<GridHeader>() as u32);
pub const GRID_HEADER_ALIGNMENT: MemoryAlignment = MemoryAlignment::U32;
pub const GRID_HEADER_WIDTH_OFFSET: MemoryOffset = MemoryOffset(8);
pub const GRID_HEADER_HEIGHT_OFFSET: MemoryOffset = MemoryOffset(10);
pub const GRID_HEADER_PAYLOAD_OFFSET: MemoryOffset = MemoryOffset(size_of::<GridHeader>() as u32);

pub const GRID_SECRET_CODE: u32 = 0x00_C0FFEE;

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

pub const MAX_STRING_LEN: u16 = 16 * 1024;

pub const STRING_PTR_SIZE: MemorySize = HEAP_PTR_ON_FRAME_SIZE;
pub const STRING_PTR_ALIGNMENT: MemoryAlignment = HEAP_PTR_ON_FRAME_ALIGNMENT;
