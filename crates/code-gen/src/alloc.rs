/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_types::TypeRef;
use swamp_vm_isa::{MemoryAlignment, MemorySize};
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::types::{BasicTypeRef, FramePlacedType};
use swamp_vm_types::{FrameMemoryAddress, FrameMemoryRegion, align_frame_addr};
use tracing::error;

#[derive(Copy, Clone, Debug)]
pub struct StackFrameAllocator {
    initial_addr: FrameMemoryAddress,
    addr: FrameMemoryAddress,
    target_info: FrameMemoryRegion,
}

impl StackFrameAllocator {
    #[must_use]
    pub const fn new(target_info: FrameMemoryRegion) -> Self {
        Self {
            initial_addr: target_info.addr,
            addr: target_info.addr,
            target_info,
        }
    }

    pub fn allocate(&mut self, size: MemorySize, alignment: MemoryAlignment) -> FrameMemoryAddress {
        let start_addr = align_frame_addr(self.addr, alignment);

        self.addr = start_addr.add(size);

        if self.addr.0 > self.target_info.last_valid_end_addr().0 {
            error!(
                "out of alloc memory {}",
                self.target_info.last_valid_end_addr()
            );
        }

        assert!(self.addr.0 <= self.target_info.last_valid_end_addr().0);

        start_addr
    }

    pub fn allocate_type(&mut self, gen_type: &BasicTypeRef) -> FramePlacedType {
        FramePlacedType::new(
            self.allocate(gen_type.total_size, gen_type.max_alignment),
            gen_type.clone(),
        )
    }

    pub fn reserve(
        &mut self,
        layout_cache: &mut LayoutCache,
        analyzed_type: &TypeRef,
    ) -> FramePlacedType {
        let gen_type = layout_cache.layout(analyzed_type);
        self.allocate_type(&gen_type)
    }

    #[must_use]
    pub const fn create_scope(&self) -> Self {
        Self {
            addr: self.addr,
            initial_addr: self.addr,
            target_info: self.target_info,
        }
    }

    #[must_use]
    pub const fn addr(&self) -> FrameMemoryAddress {
        self.addr
    }

    pub const fn reset(&mut self) {
        self.addr = self.initial_addr;
    }
}
