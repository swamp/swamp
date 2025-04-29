/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::Vm;
use std::ptr;
use swamp_vm_types::{VEC_HEADER_SIZE, VEC_ITERATOR_SIZE, VecHeader, VecIterator};

impl Vm {
    #[inline]
    pub fn execute_vec_from_slice(
        &mut self,
        target_vec_pointer: u16,
        source_slice_addr: u16,
        element_size: u16,
        element_count: u16,
    ) {
        let slice_size = element_size as usize * element_count as usize;

        // Allocate space on the heap for the slice data.
        let heap_offset: u32 = self.heap_allocate(slice_size);

        // Copy the slice from Frame to Heap.
        unsafe {
            let source_ptr = self.get_frame_ptr(source_slice_addr);
            let dest_ptr = self.get_heap_ptr(heap_offset as usize);

            ptr::copy_nonoverlapping(source_ptr, dest_ptr, slice_size);
        }

        let vec_header = VecHeader {
            count: element_count,
            capacity: element_count,
            element_size,
            heap_offset,
        };

        // Allocated heap memory for Vec header.
        let header_offset: u32 = self.heap_allocate(VEC_HEADER_SIZE.0 as usize);

        // Copy the Vec header to Heap.
        unsafe {
            let header_ptr = self.get_heap_ptr(header_offset as usize) as *mut VecHeader;
            ptr::write(header_ptr, vec_header);
        }

        // Copy the heap offset of the vec header to the frame.
        unsafe {
            let target_ptr = self.get_frame_ptr_as_u32(target_vec_pointer);
            ptr::write(target_ptr, header_offset);
        }
    }
    #[inline]
    pub fn execute_vec_iter_init(&mut self, target_iterator_addr: u16, vec_indirect: u16) {
        let vec_header_heap_ptr_as_offset = self.get_heap_offset_via_frame(vec_indirect);
        unsafe {
            let vec_iterator = VecIterator {
                vec_header_heap_ptr: vec_header_heap_ptr_as_offset,
                index: 0,
            };

            ptr::copy_nonoverlapping(
                &vec_iterator,
                self.get_frame_ptr(target_iterator_addr) as *mut VecIterator,
                VEC_ITERATOR_SIZE.0 as usize,
            );
        }
    }

    pub fn vec_header_from_heap(&self, heap_offset: u32) -> VecHeader {
        unsafe { *(self.get_heap_const_ptr(heap_offset as usize) as *const VecHeader) }
    }

    pub fn vec_header_from_indirect_heap(&self, frame_offset: u16) -> VecHeader {
        let heap_offset = self.get_heap_offset_via_frame(frame_offset);
        unsafe { *(self.get_heap_const_ptr(heap_offset as usize) as *const VecHeader) }
    }

    pub fn vec_header_from_indirect_heap_mut(&self, frame_offset: u16) -> *mut VecHeader {
        let heap_offset = self.get_heap_offset_via_frame(frame_offset);
        self.get_heap_const_ptr(heap_offset as usize) as *mut VecHeader
    }

    #[inline]
    pub fn execute_vec_len(&mut self, int_target: u16, frame_source: u16) {
        let vec_header = self.vec_header_from_indirect_heap(frame_source);
        unsafe {
            *self.get_frame_ptr_as_i32(int_target) = vec_header.count as i32;
        }
    }

    #[inline]
    pub fn execute_vec_push(&mut self, vec_frame_target: u16, item_to_push: u16) {
        let vec_header = self.vec_header_from_indirect_heap_mut(vec_frame_target);
        let (count, capacity) = unsafe { ((*vec_header).count, (*vec_header).capacity) };

        if count == capacity {
            if capacity > 16384 {
                panic!("capacity overrun");
            }
            let new_capacity = (capacity * 2);
            let new_ptr = self.heap_allocate(new_capacity as usize);
            unsafe {
                let source_items = self.get_heap_const_ptr((*vec_header).heap_offset as usize);
                let target_items = self.get_heap_ptr(new_ptr as usize);
                let element_size = (*vec_header).element_size;
                ptr::copy_nonoverlapping(
                    source_items,
                    target_items,
                    (count * element_size) as usize,
                );
                (*vec_header).heap_offset = new_ptr;
                (*vec_header).capacity = new_capacity;
                (*vec_header).count += 1;
            }
        } else {
            unsafe {
                (*vec_header).count += 1;
            }
        }
    }

    #[inline]
    pub fn execute_vec_iter_next_pair(
        &mut self,
        target_iterator_addr: u16,
        key_variable: u16,
        value_variable: u16,
        jump: u16,
    ) {
        let vec_iterator = self.get_frame_ptr(target_iterator_addr) as *mut VecIterator;
        let (data_heap_offset, index) =
            unsafe { ((*vec_iterator).vec_header_heap_ptr, (*vec_iterator).index) };

        let vec_header = self.vec_header_from_heap(data_heap_offset);

        if index == vec_header.count {
            self.ip = jump as usize;
        } else {
            let new_index = index + 1;
            let heap_data_offset = data_heap_offset + vec_header.element_size as u32 * index as u32;
            let source = self.get_heap_const_ptr(heap_data_offset as usize);
            let index_ptr = self.get_frame_ptr_as_i32(key_variable);
            let target_ptr = self.get_frame_ptr(value_variable);

            unsafe {
                ptr::copy_nonoverlapping(source, target_ptr, vec_header.element_size as usize);
                (*vec_iterator).index = new_index;
                *index_ptr = index as i32;
            }
        }
    }

    #[inline]
    pub fn execute_vec_iter_next(
        &mut self,
        target_iterator_addr: u16,
        target_variable: u16,
        jump: u16,
    ) {
        let vec_iterator = self.get_frame_ptr(target_iterator_addr) as *mut VecIterator;
        let (data_heap_offset, index) =
            unsafe { ((*vec_iterator).vec_header_heap_ptr, (*vec_iterator).index) };

        let vec_header = self.vec_header_from_heap(data_heap_offset);

        if index == vec_header.count {
            self.ip = jump as usize;
        } else {
            let new_index = index + 1;
            let heap_data_offset = data_heap_offset + vec_header.element_size as u32 * index as u32;
            let source = self.get_heap_const_ptr(heap_data_offset as usize);
            let target_ptr = self.get_frame_ptr(target_variable);

            unsafe {
                ptr::copy_nonoverlapping(source, target_ptr, vec_header.element_size as usize);
                (*vec_iterator).index = new_index;
            }
        }
    }
}
