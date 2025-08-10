/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::types::BasicTypeRef;
use crate::types::{TypedRegister, VmType};
use source_map_node::Node;
use std::cmp::PartialOrd;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Div, Sub};
use swamp_vm_isa::prelude::align;
use swamp_vm_isa::{CountU32, FrameMemorySize, InstructionPosition, MemoryAlignment, MemoryOffset, MemorySize};

pub mod prelude;
pub mod types;


pub struct StackMemoryAddress(pub u32);

impl Add<MemorySize> for StackMemoryAddress {
    type Output = Self;

    fn add(self, rhs: MemorySize) -> Self::Output {
        Self(self.0 + rhs.0)
    }
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


impl StackMemoryAddress {
    #[must_use]
    pub const fn add(&self, memory_size: MemorySize) -> Self {
        Self(self.0 + memory_size.0)
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

impl AggregateMemoryLocation {
    #[must_use]
    pub const fn new(location: MemoryLocation) -> Self {
        Self { location }
    }
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

pub enum ZFlagPolarity {
    TrueWhenSet,
    TrueWhenClear,
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

#[derive(Clone)]
pub struct Meta {
    pub comment: String,
    pub node: Node,
}

#[derive(Debug)]
pub struct PatchPosition(pub InstructionPosition);


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionPositionOffset(pub u32);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionRange {
    pub start: InstructionPosition,
    pub count: InstructionPositionOffset,
}
