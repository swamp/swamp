/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_types::TypeRef;
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::HeapMemoryAddress;
use swamp_vm_types::aligner::{SAFE_ALIGNMENT, align};
use swamp_vm_types::types::{HeapPlacedArray, HeapPlacedType};

pub struct ConstantsAllocator {
    current_addr: u32,
    max_size: u32,
}

impl ConstantsAllocator {
    #[must_use]
    pub const fn new(max_size: u32) -> Self {
        Self {
            current_addr: SAFE_ALIGNMENT as u32, // Reserve space (alignment distance) so no valid heap address is zero
            max_size,
        }
    }

    /// # Panics
    ///
    pub fn allocate(&mut self, layout_cache: &mut LayoutCache, ty: &TypeRef) -> HeapPlacedType {
        let gen_type = layout_cache.layout(ty);
        let alignment: usize = gen_type.max_alignment.into();
        let start_addr = align(self.current_addr as usize, alignment) as u32;

        self.current_addr = start_addr + gen_type.total_size.0;
        assert!(self.current_addr < self.max_size);

        HeapPlacedType::new(HeapMemoryAddress(start_addr), gen_type)
    }

    /// # Panics
    ///
    pub fn allocate_byte_array(&mut self, byte_count: u32) -> HeapPlacedArray {
        let start_addr = align(self.current_addr as usize, SAFE_ALIGNMENT) as u32;
        self.current_addr = start_addr + byte_count;
        assert!(self.current_addr < self.max_size);

        HeapPlacedArray::new(HeapMemoryAddress(start_addr), byte_count)
    }

    pub const fn reset(&mut self) {
        self.current_addr = SAFE_ALIGNMENT as u32;
    }
}

pub struct ConstantsManager {
    pub(crate) allocator: ConstantsAllocator,
    data: Vec<u8>,
}

impl ConstantsManager {
    #[must_use]
    pub fn new(memory_size: u32) -> Self {
        let data = vec![0; memory_size as usize];
        Self {
            allocator: ConstantsAllocator::new(memory_size),
            data,
        }
    }

    pub fn allocate_byte_array(&mut self, data: &[u8]) -> HeapPlacedArray {
        let count = data.len();
        if count == 0 {
            return HeapPlacedArray::new(HeapMemoryAddress(0), 0);
        }
        let addr = self.allocator.allocate_byte_array(count as u32);
        let start_idx = addr.addr().0 as usize;
        self.data[start_idx..start_idx + data.len()].copy_from_slice(data);

        addr
    }

    #[must_use]
    pub fn take_data(self) -> Vec<u8> {
        let safe_size = align(self.allocator.current_addr as usize, SAFE_ALIGNMENT);
        self.data[..safe_size].to_vec()
    }
}
