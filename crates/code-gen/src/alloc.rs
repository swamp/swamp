/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_vm_types::{
    ConstantMemoryAddress, FrameMemoryAddress, FrameMemoryRegion, MemoryAlignment, MemorySize,
    align_frame_addr,
};
use tracing::error;

const ALIGNMENT: u16 = 8;
const ALIGNMENT_REST: u16 = ALIGNMENT - 1;
const ALIGNMENT_MASK: u16 = !ALIGNMENT_REST;

#[derive(Copy, Clone, Debug)]
pub struct ScopeAllocator {
    initial_addr: FrameMemoryAddress,
    addr: FrameMemoryAddress,
    target_info: FrameMemoryRegion,
}

impl ScopeAllocator {
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

    pub fn reserve(&mut self, size: MemorySize, alignment: MemoryAlignment) -> FrameMemoryRegion {
        FrameMemoryRegion {
            addr: self.allocate(size, alignment),
            size,
        }
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

    pub fn reset(&mut self) {
        self.addr = self.initial_addr;
    }
}
