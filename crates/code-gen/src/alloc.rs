/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::Collection;
use crate::layout::layout_type;
use swamp_types::Type;
use swamp_vm_types::types::{BasicType, BasicTypeKind, FramePlacedType, unknown_type};
use swamp_vm_types::{
    FrameMemoryAddress, FrameMemoryRegion, MemoryAlignment, MemorySize, VEC_ITERATOR_ALIGNMENT,
    VEC_ITERATOR_SIZE, align_frame_addr,
};
use tracing::error;

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

    pub fn reserve_iterator(&mut self, collection: Collection) -> FramePlacedType {
        let gen_type = match collection {
            Collection::Vec => BasicType {
                kind: BasicTypeKind::DynamicLengthVecView(Box::new(unknown_type())),
                total_size: VEC_ITERATOR_SIZE,
                max_alignment: VEC_ITERATOR_ALIGNMENT,
            },
            Collection::Map => todo!(),
            Collection::Grid => todo!(),
            Collection::String => todo!(),
            Collection::Range => todo!(),
        };

        self.allocate_type(gen_type)
    }

    pub fn allocate_type(&mut self, gen_type: BasicType) -> FramePlacedType {
        FramePlacedType::new(
            self.allocate(gen_type.total_size, gen_type.max_alignment),
            gen_type,
        )
    }

    pub fn reserve(&mut self, analyzed_type: &Type) -> FramePlacedType {
        let gen_type = layout_type(analyzed_type);
        self.allocate_type(gen_type)
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
