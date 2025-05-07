/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::Memory;
use crate::set_reg;
use crate::{Vm, get_reg};
use std::ptr;
use swamp_vm_types::{VEC_HEADER_SIZE, VEC_ITERATOR_SIZE, VecHeader, VecIterator};

impl Vm {
    pub fn get_vec_iterator_header_ptr_from_reg(&self, vec_iterator_reg: u8) -> *mut VecIterator {
        self.get_ptr_from_reg(vec_iterator_reg) as *mut VecIterator
    }

    #[inline]
    pub fn execute_vec_from_slice(&mut self, target_vec_ptr_reg: u8, source_slice_ptr_reg: u8) {
        let slice_header = self.slice_header_from_reg(source_slice_ptr_reg);

        let slice_size = slice_header.element_size as usize * slice_header.element_count as usize;
        // Allocate space on the heap for the slice data.
        let heap_offset: u32 = self.memory.heap_allocate(slice_size);

        // Copy the slice from Frame to Heap.
        unsafe {
            let dest_ptr = self.memory.get_heap_ptr(heap_offset as usize);
            let src_ptr = self
                .memory
                .get_heap_const_ptr(slice_header.heap_offset as usize);

            ptr::copy_nonoverlapping(src_ptr, dest_ptr, slice_size);
        }

        let vec_header = VecHeader {
            count: slice_header.element_count,
            capacity: slice_header.element_count,
            element_size: slice_header.element_size,
            heap_offset,
        };

        let vec_header_addr: u32 = self.memory.heap_allocate(VEC_HEADER_SIZE.0 as usize);

        let vec_header_ptr = self.memory.get_heap_ptr(vec_header_addr as usize) as *mut VecHeader;

        unsafe {
            ptr::write(vec_header_ptr, vec_header);
        }

        set_reg!(self, target_vec_ptr_reg, vec_header_addr);

        #[cfg(feature = "debug_vm")]
        {
            eprintln!(
                "creating vec from slice count:{} of capacity {} element_size: {}",
                vec_header.count, vec_header.capacity, vec_header.element_size
            );
        }
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
        let vec_index = get_reg!(self, int_reg);
        let index = vec_index as usize;

        let vec_header = self.read_vec_header_from_ptr_reg(vec_header_ptr_reg);

        debug_assert!(
            index < vec_header.count as usize,
            "out of bounds for vector. index:{vec_index} out of {}",
            vec_header.count
        );

        let item_target_ptr = self.get_ptr_from_reg(element_target_reg);

        unsafe {
            let base_ptr = self
                .memory
                .get_heap_const_ptr(vec_header.heap_offset as usize);

            ptr::copy_nonoverlapping(
                base_ptr.add(index * vec_header.element_size as usize),
                item_target_ptr,
                vec_header.element_size as usize,
            );
        }

        #[cfg(feature = "debug_vm")]
        {
            eprintln!("vec_get {vec_index}");
        }
    }

    #[inline]
    pub fn execute_vec_set(&mut self, vec_header_ptr_reg: u8, int_index_reg: u8, item_ptr_reg: u8) {
        let vec_index = get_reg!(self, int_index_reg);
        let index = vec_index as usize;

        let vec_header = self.read_vec_header_from_ptr_reg(vec_header_ptr_reg);

        debug_assert!(index < vec_header.count as usize);

        let item_source_ptr = self.get_ptr_from_reg(item_ptr_reg);
        let base_ptr = self.memory.get_heap_ptr(vec_header.heap_offset as usize);

        unsafe {
            ptr::copy_nonoverlapping(
                item_source_ptr,
                base_ptr.add(index * vec_header.count as usize),
                vec_header.element_size as usize,
            );
        }

        #[cfg(feature = "debug_vm")]
        {
            eprintln!("vec_set {vec_index}");
        }
    }

    #[inline]
    pub fn execute_vec_push(&mut self, vec_header_ptr_reg: u8, element_to_push_reg: u8) {
        let vec_header = self.get_vec_header_ptr_from_reg(vec_header_ptr_reg);
        let (count, capacity) = unsafe { ((*vec_header).count, (*vec_header).capacity) };
        let element_size = unsafe { (*vec_header).element_size } as usize;

        if count == capacity {
            assert!(capacity <= 16384, "capacity overrun");
            #[cfg(feature = "debug_vm")]
            {
                eprintln!("reallocating vector {count} of capacity {capacity}");
            }
            let new_capacity = capacity * 2;
            let new_ptr = self
                .memory
                .heap_allocate(new_capacity as usize * element_size);
            unsafe {
                let source_items = self
                    .memory
                    .get_heap_const_ptr((*vec_header).heap_offset as usize);
                let target_items = self.memory.get_heap_ptr(new_ptr as usize);
                ptr::copy_nonoverlapping(source_items, target_items, count as usize * element_size);
                (*vec_header).heap_offset = new_ptr;
                (*vec_header).capacity = new_capacity;
            }
        }
        unsafe {
            let target_buckets = self.memory.get_heap_ptr((*vec_header).heap_offset as usize);
            let single_item_to_push = self.get_const_ptr_from_reg(element_to_push_reg);
            ptr::copy_nonoverlapping(
                single_item_to_push,
                target_buckets.add(count as usize * element_size),
                element_size,
            );
            (*vec_header).count += 1;
        }
    }

    #[inline]
    pub fn execute_vec_iter_next_pair(
        &mut self,
        vec_iterator_header_reg: u8,
        target_key_reg: u8,
        target_value_reg: u8,
        jump: u8,
    ) {
        let vec_iterator = self.get_vec_iterator_header_ptr_from_reg(vec_iterator_header_reg);
        let (data_heap_offset, index) =
            unsafe { ((*vec_iterator).vec_header_heap_ptr, (*vec_iterator).index) };

        let vec_header = Self::vec_header_from_heap(&self.memory, data_heap_offset);

        if index == vec_header.count {
            self.ip = jump as usize;
        } else {
            let new_index = index + 1;
            let heap_data_offset = data_heap_offset + vec_header.element_size as u32 * index as u32;
            let source = self.memory.get_heap_const_ptr(heap_data_offset as usize);
            //let index_ptr = self.get_ptr_from_reg(target_key_reg);
            set_reg!(self, target_key_reg, index);

            // TODO: Must either set_reg or get a pointer to the struct inside the vec
            unsafe {
                (*vec_iterator).index = new_index;
            }
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
        let (data_heap_offset, index) =
            unsafe { ((*vec_iterator).vec_header_heap_ptr, (*vec_iterator).index) };

        let vec_header = Self::vec_header_from_heap(&self.memory, data_heap_offset);

        if index == vec_header.count {
            self.ip = jump as usize;
        } else {
            let new_index = index + 1;
            let heap_data_offset = data_heap_offset + vec_header.element_size as u32 * index as u32;
            let source = self.memory.get_heap_const_ptr(heap_data_offset as usize);
            let target_ptr = self.get_ptr_from_reg(target_variable);

            unsafe {
                ptr::copy_nonoverlapping(source, target_ptr, vec_header.element_size as usize);
                (*vec_iterator).index = new_index;
            }
        }
    }
}
