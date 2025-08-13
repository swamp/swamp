/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{TrapCode, Vm, get_reg, i16_from_u8s, set_reg, u16_from_u8s, u32_from_u8s};
use std::ptr;
use swamp_vm_isa::SparseIterator;

impl Vm {
    pub fn execute_sparse_init(
        &mut self,
        dest_reg: u8,
        element_size_0: u8,
        element_size_1: u8,
        element_size_2: u8,
        element_size_3: u8,
        capacity_0: u8,
        capacity_1: u8,
    ) {
        let element_size = u32_from_u8s!(
            element_size_0,
            element_size_1,
            element_size_2,
            element_size_3
        );
        let capacity = u16_from_u8s!(capacity_0, capacity_1);

        unsafe {
            let dest_addr = get_reg!(self, dest_reg);
            let sparse_ptr = self.memory.get_heap_ptr(dest_addr as usize);
            sparse_mem::init(sparse_ptr, capacity, element_size);
        }
    }

    pub fn execute_sparse_add_get_entry_addr(
        &mut self,
        dest_entry_addr_reg: u8,
        dest_handle_reg: u8,
        sparse_ptr_reg: u8,
        memory_size_0: u8,
        memory_size_1: u8,
        memory_size_2: u8,
        memory_size_3: u8,
    ) {
        let element_size =
            u32_from_u8s!(memory_size_0, memory_size_1, memory_size_2, memory_size_3);

        unsafe {
            let sparse_addr = get_reg!(self, sparse_ptr_reg);
            let sparse_ptr = self.memory.get_heap_ptr(sparse_addr as usize);
            if let Some((index, generation)) = sparse_mem::allocate(sparse_ptr) {
                let relative_sparse_addr_offset = sparse_mem::values_offset(sparse_ptr);
                let addr_for_values_start = sparse_addr as usize + relative_sparse_addr_offset;
                let element_addr = addr_for_values_start + index as usize * element_size as usize;

                let handle = (index as u32) << 16 | (generation as u32);
                if self.debug_operations_enabled {
                    eprintln!("sparse_add: handle: {handle} 0x{handle:X} ({index}:{generation})");
                }
                set_reg!(self, dest_handle_reg, handle);
                set_reg!(self, dest_entry_addr_reg, element_addr as u32);
            } else {
                self.internal_trap(TrapCode::SparseOutOfSpace);
            }
        }
    }

    pub fn execute_sparse_get_entry_addr(
        &mut self,
        dest_entry_addr_reg: u8,
        sparse_ptr_reg: u8,
        int_handle_reg: u8,
        memory_size_lower: u8,
        memory_size_upper: u8,
    ) {
        let element_size = u16_from_u8s!(memory_size_lower, memory_size_upper);
        let handle = get_reg!(self, int_handle_reg);

        let index = handle >> 16;
        let generation = handle & 0xffff;

        unsafe {
            let sparse_addr = get_reg!(self, sparse_ptr_reg);
            let sparse_ptr = self.memory.get_heap_ptr(sparse_addr as usize);
            if sparse_mem::is_alive(sparse_ptr, index as u16, generation as u16) {
                let relative_sparse_addr_offset = sparse_mem::values_offset(sparse_ptr);
                let addr_for_values_start = sparse_addr as usize + relative_sparse_addr_offset;
                let element_addr = addr_for_values_start + index as usize * element_size as usize;
                set_reg!(self, dest_entry_addr_reg, element_addr);

                if self.debug_operations_enabled {
                    eprintln!("sparse_get: handle: {handle} 0x{handle:X} ({index}:{generation})");
                }
            } else {
                self.internal_trap(TrapCode::SparseGetFailed)
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
                self.internal_trap(TrapCode::SparseRemoveFailed);
            }
            if self.debug_operations_enabled {
                eprintln!("sparse_remove: handle: {handle} 0x{handle:X} ({index}:{generation})");
            }
        }
    }

    pub fn execute_sparse_is_alive(
        &mut self,
        dest_reg_bool: u8,
        sparse_ptr_reg: u8,
        int_handle_reg: u8,
    ) {
        unsafe {
            let sparse_addr = get_reg!(self, sparse_ptr_reg);
            let sparse_ptr = self.memory.get_heap_ptr(sparse_addr as usize);
            let handle = get_reg!(self, int_handle_reg);
            let index = handle >> 16;
            let generation = handle & 0xffff;
            let is_alive = sparse_mem::is_alive(sparse_ptr, index as u16, generation as u16);
            set_reg!(self, dest_reg_bool, is_alive);
            if self.debug_operations_enabled {
                eprintln!("sparse_is_alive: handle: {handle} 0x{handle:X} ({index}:{generation})");
            }
        }
    }

    pub(crate) fn execute_sparse_iter_init(
        &mut self,
        target_map_iterator_header_reg: u8,
        map_header_reg: u8,
    ) {
        let sparse_header_addr = get_reg!(self, map_header_reg);

        #[cfg(feature = "debug_vm")]
        self.debug_operations_enabled;
        let map_iterator = SparseIterator {
            sparse_header_heap_ptr: sparse_header_addr,
            index: 0,
        };

        let map_iterator_mut_ptr =
            self.get_ptr_from_reg(target_map_iterator_header_reg) as *mut SparseIterator;

        unsafe {
            ptr::write(map_iterator_mut_ptr, map_iterator);
        }
    }

    pub fn get_sparse_iterator_header_ptr_from_reg(
        &self,
        map_iterator_reg: u8,
    ) -> *mut SparseIterator {
        self.get_ptr_from_reg(map_iterator_reg) as *mut SparseIterator
    }

    pub fn execute_sparse_iter_next_pair(
        &mut self,
        sparse_iterator_header_reg: u8,
        target_key_reg: u8,
        target_value_reg: u8,
        branch_offset_lower: u8,
        branch_offset_upper: u8,
    ) {
        let sparse_iterator =
            self.get_sparse_iterator_header_ptr_from_reg(sparse_iterator_header_reg);

        unsafe {
            let sparse_header_addr = (*sparse_iterator).sparse_header_heap_ptr;
            let sparse_header_ptr = self.memory.get_heap_const_ptr(sparse_header_addr as usize);

            let element_count = sparse_mem::element_count(sparse_header_ptr);
            let current_index = (*sparse_iterator).index as usize;
            if (current_index as u32) < element_count as u32 {
                let values_index = (*sparse_mem::slot_to_id_ptr_const(sparse_header_ptr)
                    .add(current_index)) as usize;

                #[cfg(feature = "debug_vm")]
                if self.debug_operations_enabled {
                    eprintln!("index {current_index}, slot: {values_index}");
                }

                let offset_to_values = sparse_mem::values_offset(sparse_header_ptr);
                let values_start = sparse_header_addr as usize + offset_to_values;

                let element_size = sparse_mem::element_size(sparse_header_ptr) as usize;
                let entry_addr = values_start + values_index * element_size;

                let generation =
                    *sparse_mem::generation_ptr_const(sparse_header_ptr).add(values_index);
                let handle = (values_index as u32) << 16 | (generation as u32);
                set_reg!(self, target_key_reg, handle);
                set_reg!(self, target_value_reg, entry_addr);
                (*sparse_iterator).index += 1;
            } else {
                // Jump to the provided address if we're done
                let branch_offset = i16_from_u8s!(branch_offset_lower, branch_offset_upper);

                #[cfg(feature = "debug_vm")]
                if self.debug_operations_enabled {
                    eprintln!("iteration done, jumping {branch_offset}");
                }
                self.pc = (self.pc as i32 + branch_offset as i32) as usize;
            }
        }
    }

    pub fn execute_sparse_iter_next(
        &mut self,
        sparse_iterator_header_reg: u8,
        target_entry_address_reg: u8,
        branch_offset_lower: u8,
        branch_offset_upper: u8,
    ) {
        let sparse_iterator =
            self.get_sparse_iterator_header_ptr_from_reg(sparse_iterator_header_reg);

        unsafe {
            let sparse_header_addr = (*sparse_iterator).sparse_header_heap_ptr;
            let sparse_header_ptr = self.memory.get_heap_const_ptr(sparse_header_addr as usize);

            let element_count = sparse_mem::element_count(sparse_header_ptr);
            let current_index = (*sparse_iterator).index as usize;
            if (current_index as u32) < element_count as u32 {
                let values_index = (*sparse_mem::slot_to_id_ptr_const(sparse_header_ptr)
                    .add(current_index)) as usize;

                #[cfg(feature = "debug_vm")]
                if self.debug_operations_enabled {
                    eprintln!("index {current_index}, slot: {values_index}");
                }

                let offset_to_values = sparse_mem::values_offset(sparse_header_ptr);
                let values_start = sparse_header_addr as usize + offset_to_values;

                let element_size = sparse_mem::element_size(sparse_header_ptr) as usize;
                let entry_addr = values_start + values_index * element_size;

                set_reg!(self, target_entry_address_reg, entry_addr);
                (*sparse_iterator).index += 1;
            } else {
                // Jump to the provided address if we're done
                let branch_offset = i16_from_u8s!(branch_offset_lower, branch_offset_upper);

                #[cfg(feature = "debug_vm")]
                if self.debug_operations_enabled {
                    eprintln!("iteration done, jumping {branch_offset}");
                }
                self.pc = (self.pc as i32 + branch_offset as i32) as usize;
            }
        }
    }
}
