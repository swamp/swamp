/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{TrapCode, Vm, get_reg, i16_from_u8s, set_reg, u16_from_u8s};
use std::ptr;
use swamp_vm_types::{GridHeader, SparseIterator};

impl Vm {
    pub fn execute_grid_init(
        &mut self,
        dest_reg: u8,
        memory_size_lower: u8,
        memory_size_upper: u8,
        width_lower: u8,
        width_upper: u8,
        height_lower: u8,
        height_upper: u8,
    ) {
        let element_size = u16_from_u8s!(memory_size_lower, memory_size_upper);
        let width = u16_from_u8s!(width_lower, width_upper);
        let height = u16_from_u8s!(height_lower, height_upper);

        unsafe {
            let dest_addr = get_reg!(self, dest_reg);
            let grid_header = self.memory.get_heap_ptr(dest_addr as usize) as *mut GridHeader;
            (*grid_header).width = width;
            (*grid_header).height = height;
            (*grid_header).capacity = width * height;
            (*grid_header).element_count = width * height;
        }
    }
}
