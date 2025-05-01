/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::layout::layout_type;
use swamp_types::Type;
use swamp_vm_types::HeapMemoryAddress;
use swamp_vm_types::aligner::{SAFE_ALIGNMENT, align};
use swamp_vm_types::types::{HeapPlacedArray, HeapPlacedType};

pub struct ConstantsAllocator {
    current_addr: u32,
}

impl Default for ConstantsAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantsAllocator {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            current_addr: SAFE_ALIGNMENT as u32,
        } // Reserve space so no valid heap address is zero
    }

    pub fn allocate(&mut self, ty: &Type) -> HeapPlacedType {
        let gen_type = layout_type(&ty, "");
        let alignment: usize = gen_type.max_alignment.into();
        let start_addr = align(self.current_addr as usize, alignment) as u32;

        self.current_addr = start_addr + gen_type.total_size.0 as u32;

        HeapPlacedType::new(HeapMemoryAddress(start_addr), gen_type)
    }

    pub fn allocate_byte_array(&mut self, byte_count: u32) -> HeapPlacedArray {
        let start_addr = align(self.current_addr as usize, SAFE_ALIGNMENT) as u32;
        self.current_addr += byte_count;

        HeapPlacedArray::new(HeapMemoryAddress(start_addr), byte_count)
    }

    pub const fn reset(&mut self) {
        self.current_addr = 0;
    }
}

pub struct ConstantsManager {
    pub(crate) allocator: ConstantsAllocator,
    data: Vec<u8>,
}

impl Default for ConstantsManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantsManager {
    #[must_use]
    pub fn new() -> Self {
        Self {
            allocator: ConstantsAllocator::new(),
            data: vec![0u8; 1024 * 1024],
        }
    }

    pub fn allocate_byte_array(&mut self, data: &[u8], count: u32) -> HeapPlacedArray {
        let addr = self.allocator.allocate_byte_array(count);

        let start_idx = addr.addr().0 as usize;
        self.data[start_idx..start_idx + data.len()].copy_from_slice(data);

        addr
    }

    #[must_use]
    pub fn take_data(self) -> Vec<u8> {
        self.data[..self.allocator.current_addr as usize].to_vec()
    }
}
