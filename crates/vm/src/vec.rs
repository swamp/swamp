/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::Memory;
use crate::{TrapCode, Vm, get_reg, i16_from_u8s, u16_from_u8s};
use crate::{set_reg, u8s_to_u16};
use std::ptr;
use swamp_vm_types::{VEC_HEADER_PAYLOAD_OFFSET, VecHeader, VecIterator};

impl Vm {
    pub fn get_vec_iterator_header_ptr_from_reg(&self, vec_iterator_reg: u8) -> *mut VecIterator {
        self.get_ptr_from_reg(vec_iterator_reg) as *mut VecIterator
    }

    #[inline]
    pub fn execute_array_init(
        &mut self,
        target_vec_ptr_reg: u8,
        element_ptr_reg: u8,
        capacity_lower: u8,
        capacity_upper: u8,
    ) {
        let vec_addr = get_reg!(self, target_vec_ptr_reg);
        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(vec_addr as usize)
            .cast::<VecHeader>();
        let capacity = u16_from_u8s!(capacity_lower, capacity_upper);
        unsafe {
            (*mut_vec_ptr).element_count = capacity;
            (*mut_vec_ptr).capacity = capacity;
        }

        let element_addr = vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 as u32;
        set_reg!(self, element_ptr_reg, element_addr);
    }

    #[inline]
    pub fn execute_vec_iter_init(
        &mut self,
        target_vec_iterator_header_reg: u8,
        vec_header_reg: u8,
        element_size_lower: u8,
        element_size_upper: u8,
    ) {
        let vec_header_addr = get_reg!(self, vec_header_reg);

        let element_size = u16_from_u8s!(element_size_lower, element_size_upper);
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            let iter_addr = get_reg!(self, target_vec_iterator_header_reg);
            eprintln!(
                "vec_iter_init: iter_addr: {iter_addr:04X} vec_header_addr:{vec_header_addr:04X} element_size: {element_size}"
            );
        }
        let vec_iterator = VecIterator {
            vec_header_heap_ptr: vec_header_addr,
            element_size,
            index: 0,
        };

        let vec_iterator_mut_ptr =
            self.get_ptr_from_reg(target_vec_iterator_header_reg) as *mut VecIterator;

        unsafe {
            ptr::write(vec_iterator_mut_ptr, vec_iterator);
        }
    }

    #[inline]
    pub fn execute_vec_iter_next(
        &mut self,
        vec_iterator_header_reg: u8,
        target_variable: u8,
        branch_offset_lower: u8,
        branch_offset_upper: u8,
    ) {
        let vec_iterator = self.get_vec_iterator_header_ptr_from_reg(vec_iterator_header_reg);

        unsafe {
            let vec_header_addr = (*vec_iterator).vec_header_heap_ptr;
            let vec_header_ptr = self
                .memory
                .get_heap_const_ptr(vec_header_addr as usize)
                .cast::<VecHeader>();
            let vec_header = &*vec_header_ptr;
            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                let iter_addr = get_reg!(self, vec_iterator_header_reg);
                let index = (*vec_iterator).index;
                eprintln!(
                    "vec_iter_next: iter_addr: {iter_addr:04X} addr:{vec_header_addr:04X} index:{index} len: {}, capacity: {}",
                    vec_header.element_count, vec_header.capacity
                );
            }

            // Check if we've reached the end
            if (*vec_iterator).index >= vec_header.element_count {
                // Jump to the provided address if we're done
                let branch_offset = i16_from_u8s!(branch_offset_lower, branch_offset_upper);

                #[cfg(feature = "debug_vm")]
                {
                    if self.debug_operations_enabled {
                        eprintln!("vec_iter_next complete. jumping with offset {branch_offset}");
                    }
                }

                self.pc = (self.pc as i32 + branch_offset as i32) as usize;

                return;
            }

            // Calculate the address of the current element
            let element_addr = (*vec_iterator).vec_header_heap_ptr
                + VEC_HEADER_PAYLOAD_OFFSET.0 as u32
                + (*vec_iterator).index as u32 * (*vec_iterator).element_size as u32;

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                eprintln!(
                    "vec_iter_next: element_addr {element_addr:04X} to reg {target_variable}"
                );
            }

            set_reg!(self, target_variable, element_addr);

            (*vec_iterator).index += 1;
        }
    }

    #[inline]
    pub fn execute_vec_iter_next_pair(
        &mut self,
        vec_iterator_header_reg: u8,
        target_key_reg: u8,
        target_value_reg: u8,
        branch_offset_lower: u8,
        branch_offset_upper: u8,
    ) {
        let vec_iterator = self.get_vec_iterator_header_ptr_from_reg(vec_iterator_header_reg);

        unsafe {
            let vec_header_addr = (*vec_iterator).vec_header_heap_ptr;
            let vec_header_ptr =
                self.memory.get_heap_const_ptr(vec_header_addr as usize) as *const VecHeader;
            let vec_header = &*vec_header_ptr;
            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                let iter_addr = get_reg!(self, vec_iterator_header_reg);
                let index = (*vec_iterator).index;
                eprintln!(
                    "vec_iter_next: iter_addr: {iter_addr:04X} addr:{vec_header_addr:04X} index:{index} len: {}, capacity: {}",
                    vec_header.element_count, vec_header.capacity
                );
            }

            // Check if we've reached the end
            if (*vec_iterator).index >= vec_header.element_count {
                // Jump to the provided address if we're done
                let branch_offset = i16_from_u8s!(branch_offset_lower, branch_offset_upper);

                #[cfg(feature = "debug_vm")]
                {
                    if self.debug_operations_enabled {
                        eprintln!(
                            "vec_iter_next_pair complete. jumping with offset {branch_offset}"
                        );
                    }
                }

                self.pc = (self.pc as i32 + branch_offset as i32) as usize;

                return;
            }

            // Calculate the address of the current element
            let element_addr = (*vec_iterator).vec_header_heap_ptr
                + VEC_HEADER_PAYLOAD_OFFSET.0 as u32
                + (*vec_iterator).index as u32 * (*vec_iterator).element_size as u32;

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                eprintln!(
                    "vec_iter_next: element_addr {element_addr:04X} to reg {target_value_reg}"
                );
            }

            set_reg!(self, target_key_reg, (*vec_iterator).index);
            set_reg!(self, target_value_reg, element_addr);

            (*vec_iterator).index += 1;
        }
    }

    pub fn vec_header_from_heap(heap: &Memory, heap_offset: u32) -> VecHeader {
        unsafe { *(heap.get_heap_const_ptr(heap_offset as usize) as *const VecHeader) }
    }

    pub fn read_vec_header_from_ptr_reg(&self, vec_header_ptr_reg: u8) -> VecHeader {
        let vec_header_const_ptr =
            self.get_const_ptr_from_reg(vec_header_ptr_reg) as *const VecHeader;
        unsafe { *vec_header_const_ptr }
    }

    pub fn get_vec_header_ptr_from_reg(&self, vec_header_ptr_reg: u8) -> *mut VecHeader {
        self.get_ptr_from_reg(vec_header_ptr_reg) as *mut VecHeader
    }

    #[inline]
    pub fn execute_vec_get(
        &mut self,
        element_target_reg: u8,
        vec_header_ptr_reg: u8,
        int_reg: u8,
        element_size_lower: u8,
        element_size_upper: u8,
    ) {
        let vec_addr = get_reg!(self, vec_header_ptr_reg);

        let vec_header = Self::vec_header_from_heap(&self.memory, vec_addr);
        let index = get_reg!(self, int_reg);

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "vec_get: vec_header_addr: {vec_addr:04X} index: {index} count: {}, capacity: {} ",
                vec_header.element_count, vec_header.capacity
            );
        }

        #[cfg(feature = "debug_vm")]
        {
            if self.debug_operations_enabled {
                eprintln!(
                    "vec_get {} {} (capacity: {}) ",
                    index, vec_header.element_count, vec_header.capacity
                );
            }
        }

        if index >= vec_header.element_count as u32 {
            return self.internal_trap(TrapCode::VecBoundsFail);
        }

        let element_size = u16_from_u8s!(element_size_lower, element_size_upper);

        let address_of_element =
            vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 as u32 + index * element_size as u32;

        set_reg!(self, element_target_reg, address_of_element);
    }

    #[inline]
    pub fn execute_vec_set(&mut self, vec_header_ptr_reg: u8, int_index_reg: u8, item_ptr_reg: u8) {
        #[cfg(feature = "debug_vm")]
        {
            eprintln!("vec_set ");
        }
    }

    #[inline]
    pub fn execute_vec_push_addr(
        &mut self,
        destination_entry_addr_reg: u8,
        src_vec_header_ptr_reg: u8,
        size_of_elements_lower: u8,
        size_of_elements_upper: u8,
    ) {
        let size_of_each_element =
            u8s_to_u16!(size_of_elements_lower, size_of_elements_upper) as u32;

        let vec_addr = get_reg!(self, src_vec_header_ptr_reg);

        let mut_vec_ptr = self.memory.get_heap_ptr(vec_addr as usize) as *mut VecHeader;

        let mut len = 0;

        unsafe {
            len = (*mut_vec_ptr).element_count;
            if len >= (*mut_vec_ptr).capacity {
                return self.internal_trap(TrapCode::VecBoundsFail);
            }
            (*mut_vec_ptr).element_count += 1;
        }

        let address_of_new_element =
            vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 as u32 + len as u32 * size_of_each_element;

        set_reg!(self, destination_entry_addr_reg, address_of_new_element);
    }

    #[inline]
    pub fn execute_vec_pop(
        &mut self,
        dst_reg: u8,
        vec_header_ptr_reg: u8,
        size_of_elements_lower: u8,
        size_of_elements_upper: u8,
    ) {
        let size_of_each_element =
            u8s_to_u16!(size_of_elements_lower, size_of_elements_upper) as u32;

        let vec_addr = get_reg!(self, vec_header_ptr_reg);
        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(vec_addr as usize)
            .cast::<VecHeader>();

        unsafe {
            let header = &mut *mut_vec_ptr;

            // Check if vector is empty
            if header.element_count == 0 {
                return self.internal_trap(TrapCode::VecBoundsFail);
            }
            // Get the last element index
            let last_index = u32::from(header.element_count) - 1;

            // Calculate address of the element to be popped
            let address_of_element_to_pop = vec_addr
                + u32::from(VEC_HEADER_PAYLOAD_OFFSET.0)
                + last_index * size_of_each_element;

            header.element_count -= 1;

            set_reg!(self, dst_reg, address_of_element_to_pop);
        }
    }

    #[inline]
    pub fn execute_vec_remove_index(
        &mut self,
        vec_header_ptr_reg: u8,
        remove_index_reg: u8,
        size_of_elements_lower: u8,
        size_of_elements_upper: u8,
    ) {
        let size_of_each_element =
            u8s_to_u16!(size_of_elements_lower, size_of_elements_upper) as u32;

        let vec_addr = get_reg!(self, vec_header_ptr_reg);
        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(vec_addr as usize)
            .cast::<VecHeader>();

        let index = get_reg!(self, remove_index_reg);

        unsafe {
            if index >= u32::from((*mut_vec_ptr).element_count) {
                return self.internal_trap(TrapCode::VecBoundsFail);
            }
        }

        let address_of_element_to_be_removed =
            vec_addr + u32::from(VEC_HEADER_PAYLOAD_OFFSET.0) + index * size_of_each_element;

        unsafe {
            let header = &mut *mut_vec_ptr;
            let count = u32::from(header.element_count);

            if index < count - 1 {
                let src_addr = address_of_element_to_be_removed + size_of_each_element;
                let dst_addr = address_of_element_to_be_removed;
                let elems_after = (count - index - 1) as usize;
                let bytes_to_move = elems_after * size_of_each_element as usize;

                let src_ptr = self.memory.get_heap_ptr(src_addr as usize).cast_const();
                let dst_ptr = self.memory.get_heap_ptr(dst_addr as usize);

                // MemMove (copy *with* overlap)
                ptr::copy(src_ptr, dst_ptr, bytes_to_move);
            }

            header.element_count -= 1;
        }
    }
}
