/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{TrapCode, Vm, get_reg, i16_from_u8s, set_reg, u16_from_u8s, u32_from_u8s};
use std::ptr;
use swamp_vm_types::{GRID_HEADER_PAYLOAD_OFFSET, GridHeader, SparseIterator};

impl Vm {
    pub fn execute_grid_init(
        &mut self,
        dest_reg: u8,
        element_size_reg: u8,
        width_lower: u8,
        width_upper: u8,
        height_lower: u8,
        height_upper: u8,
    ) {
        let element_size = get_reg!(self, element_size_reg);
        let width = u16_from_u8s!(width_lower, width_upper);
        let height = u16_from_u8s!(height_lower, height_upper);

        unsafe {
            let dest_addr = get_reg!(self, dest_reg);
            let grid_header = self.memory.get_heap_ptr(dest_addr as usize) as *mut GridHeader;
            (*grid_header).width = width;
            (*grid_header).height = height;
            (*grid_header).element_size = element_size;
            (*grid_header).capacity = width * height;
            (*grid_header).element_count = width * height;
        }
    }

    pub fn execute_grid_get_entry_addr(
        &mut self,
        dest_reg: u8,
        self_addr_reg: u8,
        x_reg: u8,
        y_reg: u8,
        element_lower: u8,
        element_upper: u8,
    ) {
        unsafe {
            let grid_header_addr = get_reg!(self, self_addr_reg);
            let grid_header =
                self.memory.get_heap_const_ptr(grid_header_addr as usize) as *mut GridHeader;
            let x_value = get_reg!(self, x_reg);
            let y_value = get_reg!(self, y_reg);
            let index = y_value * (*grid_header).width as u32 + x_value;
            let element_size = u16_from_u8s!(element_lower, element_upper) as u32;
            let element_offset = index * element_size;
            let entry_addr = grid_header_addr + GRID_HEADER_PAYLOAD_OFFSET.0 + element_offset;
            set_reg!(self, dest_reg, entry_addr);
        }
    }
}
