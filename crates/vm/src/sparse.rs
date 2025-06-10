/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{TrapCode, Vm, get_reg, set_reg, u16_from_u8s};

impl Vm {
    pub fn execute_sparse_init(
        &mut self,
        dest_reg: u8,
        memory_size_lower: u8,
        memory_size_upper: u8,
        capacity_lower: u8,
        capacity_upper: u8,
    ) {
        let element_size = u16_from_u8s!(memory_size_lower, memory_size_upper);
        let capacity = u16_from_u8s!(capacity_lower, capacity_upper);

        unsafe {
            let dest_addr = get_reg!(self, dest_reg);
            let sparse_ptr = self.memory.get_heap_ptr(dest_addr as usize);
            sparse_mem::init(sparse_ptr, capacity);
        }
    }

    pub fn execute_sparse_add_get_entry_addr(
        &mut self,
        dest_entry_addr_reg: u8,
        dest_handle_reg: u8,
        sparse_ptr_reg: u8,
        memory_size_lower: u8,
        memory_size_upper: u8,
    ) {
        let element_size = u16_from_u8s!(memory_size_lower, memory_size_upper);

        unsafe {
            let sparse_addr = get_reg!(self, sparse_ptr_reg);
            let sparse_ptr = self.memory.get_heap_ptr(sparse_addr as usize);
            if let Some((index, generation)) = sparse_mem::allocate(sparse_ptr) {
                let relative_sparse_addr_offset = sparse_mem::values_offset(sparse_ptr);
                let addr_for_values_start = sparse_addr as usize + relative_sparse_addr_offset;
                let element_addr = addr_for_values_start + index as usize * element_size as usize;

                let handle = (index as u32) << 16 | (generation as u32);
                set_reg!(self, dest_handle_reg, handle);
                set_reg!(self, dest_entry_addr_reg, element_addr);
            } else {
                self.internal_trap(TrapCode::SparseOutOfSpace)
            }
        }
    }

    pub fn execute_sparse_remove(&mut self, sparse_ptr_reg: u8, int_handle_reg: u8) {
        unsafe {
            let sparse_addr = get_reg!(self, sparse_ptr_reg);
            let sparse_ptr = self.memory.get_heap_ptr(sparse_addr as usize);
            let handle = get_reg!(self, int_handle_reg);
            let index = handle >> 16;
            let generation = handle & 0xffff;
            let could_be_removed = sparse_mem::remove(sparse_ptr, index as u16, generation as u16);
            if !could_be_removed {
                self.internal_trap(TrapCode::SparseRemoveFailed)
            }
        }
    }
}
