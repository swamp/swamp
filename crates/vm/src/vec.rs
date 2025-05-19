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
    pub fn execute_vec_from_slice(
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
            (*mut_vec_ptr).capacity = capacity;
            (*mut_vec_ptr).count = len;
        }

        let element_addr = vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 as u32;
        set_reg!(self, element_ptr_reg, element_addr);
    }

    #[inline]
    pub fn execute_vec_iter_init(
        &mut self,
        target_vec_iterator_header_reg: u8,
        vec_header_reg: u8,
    ) {
        let vec_header_addr = get_reg!(self, vec_header_reg);

        let vec_iterator = VecIterator {
            vec_header_heap_ptr: vec_header_addr,
            index: 0,
        };

        let vec_iterator_mut_ptr =
            self.get_ptr_from_reg(target_vec_iterator_header_reg) as *mut VecIterator;

        unsafe {
            ptr::copy_nonoverlapping(&vec_iterator, vec_iterator_mut_ptr, 1);
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
    pub fn execute_vec_get(&mut self, element_target_reg: u8, vec_header_ptr_reg: u8, int_reg: u8) {
        #[cfg(feature = "debug_vm")]
        {
            eprintln!("vec_get ");
        }
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

    #[inline]
    pub fn execute_vec_iter_next(
        &mut self,
        vec_iterator_header_reg: u8,
        target_variable: u8,
        jump: u8,
    ) {
    }
}
