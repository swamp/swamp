/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::Memory;
use crate::{Vm, get_reg, u16_from_u8s};
use crate::{VmState, set_reg, u8s_to_u16};
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
        let mut_vec_ptr = self.memory.get_heap_ptr(vec_addr as usize) as *mut VecHeader;
        let capacity = u16_from_u8s!(capacity_lower, capacity_upper);
        unsafe {
            (*mut_vec_ptr).count = capacity;
            (*mut_vec_ptr).capacity = capacity;
        }

        let element_addr = vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 as u32;
        set_reg!(self, element_ptr_reg, element_addr);
    }

    #[inline]
    pub fn execute_vec_init(
        &mut self,
        target_vec_ptr_reg: u8,
        element_ptr_reg: u8,
        len_lower: u8,
        len_upper: u8,
        capacity_lower: u8,
        capacity_upper: u8,
    ) {
        let vec_addr = get_reg!(self, target_vec_ptr_reg);
        let mut_vec_ptr = self.memory.get_heap_ptr(vec_addr as usize) as *mut VecHeader;
        let len = u16_from_u8s!(len_lower, len_upper);
        let capacity = u16_from_u8s!(capacity_lower, capacity_upper);
        unsafe {
            (*mut_vec_ptr).count = len;
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
        jump: u8,
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
                    vec_header.count, vec_header.capacity
                );
            }

            // Check if we've reached the end
            if (*vec_iterator).index >= vec_header.count {
                // Jump to the provided address if we're done
                self.pc = jump as usize;
                #[cfg(feature = "debug_vm")]
                if self.debug_operations_enabled {
                    eprintln!("vec_iter_next done!");
                }

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
                vec_header.count, vec_header.capacity
            );
        }

        #[cfg(feature = "debug_vm")]
        {
            if self.debug_operations_enabled {
                eprintln!(
                    "vec_get {} {} (capacity: {}) ",
                    index, vec_header.count, vec_header.capacity
                );
            }
        }

        assert!(index < vec_header.count as u32, "subscript error ");

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

    fn trap(&mut self, code: u8) {
        self.execution_complete = true;
        self.state = VmState::Trap(code);
    }

    #[inline]
    pub fn execute_vec_push_addr(
        &mut self,
        dst_reg: u8,
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
            len = (*mut_vec_ptr).count;
            if len >= (*mut_vec_ptr).capacity {
                return self.trap(5);
            }
            (*mut_vec_ptr).count += 1;
        }

        let address_of_new_element =
            vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 as u32 + len as u32 * size_of_each_element;

        set_reg!(self, dst_reg, address_of_new_element);
    }

    #[inline]
    pub fn execute_vec_iter_next_pair(
        &mut self,
        vec_iterator_header_reg: u8,
        target_key_reg: u8,
        target_value_reg: u8,
        jump: u8,
    ) {
    }
}
