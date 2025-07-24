/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::Memory;
use crate::set_reg;
use crate::{get_reg, i16_from_u8s, u16_from_u8s, u32_from_u8s, TrapCode, Vm};
use std::ptr;
use swamp_vm_types::{
    VecHeader, VecIterator, VEC_HEADER_MAGIC_CODE, VEC_HEADER_PAYLOAD_OFFSET, VEC_HEADER_SIZE,
};

impl Vm {
    pub fn get_vec_iterator_header_ptr_from_reg(&self, vec_iterator_reg: u8) -> *mut VecIterator {
        self.get_ptr_from_reg(vec_iterator_reg) as *mut VecIterator
    }

    #[inline]
    pub fn execute_array_init(
        &mut self,
        target_vec_ptr_reg: u8,
        capacity_lower: u8,
        capacity_upper: u8,
        element_size_0: u8,
        element_size_1: u8,
        element_size_2: u8,
        element_size_3: u8,
    ) {
        let vec_addr = get_reg!(self, target_vec_ptr_reg);
        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(vec_addr as usize)
            .cast::<VecHeader>();
        let capacity = u16_from_u8s!(capacity_lower, capacity_upper);
        let element_size = u32_from_u8s!(
            element_size_0,
            element_size_1,
            element_size_2,
            element_size_3
        );
        debug_assert_ne!(capacity, 0, "illegal capacity");
        unsafe {
            (*mut_vec_ptr).element_count = capacity;
            (*mut_vec_ptr).capacity = capacity;
            (*mut_vec_ptr).element_size = element_size;
            (*mut_vec_ptr).padding = VEC_HEADER_MAGIC_CODE;
        }

        if self.debug_operations_enabled {
            eprintln!("array init element_size:{element_size} into vec_addr: {vec_addr:X}");
        }
    }

    #[inline]
    pub fn execute_vec_cmp(
        &mut self,
        bool_target_reg: u8,
        left_vec_ptr_reg: u8,
        right_vec_ptr_reg: u8,
    ) {
        let left_vec_addr = get_reg!(self, left_vec_ptr_reg);
        let right_vec_addr = get_reg!(self, right_vec_ptr_reg);

        let left_vec_ptr = self
            .memory
            .get_heap_const_ptr(left_vec_addr as usize)
            .cast::<VecHeader>();

        let right_vec_ptr = self
            .memory
            .get_heap_const_ptr(right_vec_addr as usize)
            .cast::<VecHeader>();

        unsafe {
            if (*left_vec_ptr).padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }
            if (*left_vec_ptr).capacity == 0 {
                eprintln!("TARGET IS NOT INITIALIZED");
                return self.internal_trap(TrapCode::VecNeverInitialized);
            }
            if (*right_vec_ptr).padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }
            if (*right_vec_ptr).capacity == 0 {
                eprintln!("SOURCE IS NOT INITIALIZED");
                return self.internal_trap(TrapCode::VecNeverInitialized);
            }

            if (*left_vec_ptr).element_count != (*right_vec_ptr).element_count {
                set_reg!(self, bool_target_reg, 0);
            }

            let left_ptr = self
                .memory
                .get_heap_const_ptr(left_vec_addr as usize)
                .add(VEC_HEADER_PAYLOAD_OFFSET.0 as usize);

            let right_ptr = self
                .memory
                .get_heap_const_ptr(right_vec_addr as usize)
                .add(VEC_HEADER_PAYLOAD_OFFSET.0 as usize);

            let byte_size =
                ((*left_vec_ptr).element_count as usize) * (*left_vec_ptr).element_size as usize;
            let slice_a = std::slice::from_raw_parts(left_ptr, byte_size);
            let slice_b = std::slice::from_raw_parts(right_ptr, byte_size);

            set_reg!(self, bool_target_reg, slice_a == slice_b);
        }
    }

    #[inline]
    pub fn execute_vec_copy(&mut self, target_vec_ptr_reg: u8, source_vec_ptr_reg: u8) {
        let target_vec_addr = get_reg!(self, target_vec_ptr_reg);
        let source_vec_addr = get_reg!(self, source_vec_ptr_reg);

        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(target_vec_addr as usize)
            .cast::<VecHeader>();

        let src_vec_ptr = self
            .memory
            .get_heap_const_ptr(source_vec_addr as usize)
            .cast::<VecHeader>();

        unsafe {
            if (*mut_vec_ptr).padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }
            if (*mut_vec_ptr).capacity == 0 {
                eprintln!("TARGET IS NOT INITIALIZED");
                return self.internal_trap(TrapCode::VecNeverInitialized);
            }
            if (*src_vec_ptr).padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }
            if (*src_vec_ptr).capacity == 0 {
                eprintln!("SOURCE IS NOT INITIALIZED");
                return self.internal_trap(TrapCode::VecNeverInitialized);
            }

            if (*mut_vec_ptr).capacity < (*src_vec_ptr).element_count {
                return self.internal_trap(TrapCode::VecOutOfCapacity {
                    encountered: (*src_vec_ptr).element_count,
                    capacity: (*mut_vec_ptr).capacity,
                });
            }

            let target_capacity = (*mut_vec_ptr).capacity;

            let target_tail = (target_vec_addr + 2) as usize; // Skip capacity
            let target_raw = self.memory.get_heap_ptr(target_tail);
            let source_tail = (source_vec_addr + 2) as usize; // Skip capacity
            let source_raw = self.memory.get_heap_const_ptr(source_tail);

            let total_bytes_to_copy = (VEC_HEADER_SIZE.0 - 2)
                + ((*src_vec_ptr).element_count as u32) * (*src_vec_ptr).element_size;

            ptr::copy_nonoverlapping(source_raw, target_raw, total_bytes_to_copy as usize);

            debug_assert_eq!(
                (*mut_vec_ptr).element_count,
                (*src_vec_ptr).element_count,
                "element count differs"
            );
            debug_assert_eq!(
                (*mut_vec_ptr).capacity,
                target_capacity,
                "capacity has been modified"
            );
        }
    }


    #[inline]
    pub fn execute_vec_copy_range(&mut self, target_vec_ptr_reg: u8, source_vec_ptr_reg: u8, range_reg: u8) {
        let target_vec_addr = get_reg!(self, target_vec_ptr_reg);
        let source_vec_addr = get_reg!(self, source_vec_ptr_reg);
        let range_header = self.range_header_from_reg(range_reg);

        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(target_vec_addr as usize)
            .cast::<VecHeader>();

        let src_vec_ptr = self
            .memory
            .get_heap_const_ptr(source_vec_addr as usize)
            .cast::<VecHeader>();

        unsafe {
            if (*mut_vec_ptr).padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }
            if (*mut_vec_ptr).capacity == 0 {
                eprintln!("TARGET IS NOT INITIALIZED");
                return self.internal_trap(TrapCode::VecNeverInitialized);
            }
            if (*src_vec_ptr).padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }
            if (*src_vec_ptr).capacity == 0 {
                return self.internal_trap(TrapCode::VecNeverInitialized);
            }

            if range_header.max < range_header.min {
                return self.internal_trap(TrapCode::ReverseRangeNotAllowedHere);
            }


            debug_assert!(range_header.max >= range_header.min);

            let num_elements_to_copy = if range_header.inclusive { (range_header.max - range_header.min + 1) as u32 } else { (range_header.max - range_header.min) as u32 };
            let source_element_index = range_header.min as u32;
            let required_source_element_count = source_element_index + num_elements_to_copy;

            if (*mut_vec_ptr).capacity < num_elements_to_copy as u16 {
                return self.internal_trap(TrapCode::VecOutOfCapacity {
                    encountered: (*src_vec_ptr).element_count,
                    capacity: (*mut_vec_ptr).capacity,
                });
            }

            if (*src_vec_ptr).element_count < required_source_element_count as u16 {
                return self.internal_trap(TrapCode::VecBoundsFail {
                    encountered: required_source_element_count as usize,
                    element_count: (*src_vec_ptr).element_count as usize,
                });
            }

            let target_capacity = (*mut_vec_ptr).capacity;

            let target_payload = (target_vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0) as usize;
            let target_raw = self.memory.get_heap_ptr(target_payload);

            let source_slice_start = (source_vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 + source_element_index * (*src_vec_ptr).element_size) as usize;
            let source_raw = self.memory.get_heap_const_ptr(source_slice_start);

            let total_bytes_to_copy = num_elements_to_copy * (*src_vec_ptr).element_size;

            // HACK: For now allow overlapping, since the slice target can be self
            //ptr::copy_nonoverlapping(source_raw, target_raw, total_bytes_to_copy as usize);
            ptr::copy(source_raw, target_raw, total_bytes_to_copy as usize);
            (*mut_vec_ptr).element_count = num_elements_to_copy as u16;

            debug_assert_eq!(
                (*mut_vec_ptr).capacity,
                target_capacity,
                "capacity has been modified"
            );
        }
    }

    #[inline]
    pub fn execute_vec_init(
        &mut self,
        target_vec_ptr_reg: u8,
        capacity_lower: u8,
        capacity_upper: u8,
        element_size_0: u8,
        element_size_1: u8,
        element_size_2: u8,
        element_size_3: u8,
    ) {
        let vec_addr = get_reg!(self, target_vec_ptr_reg);
        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(vec_addr as usize)
            .cast::<VecHeader>();
        let capacity = u16_from_u8s!(capacity_lower, capacity_upper);
        let element_size = u32_from_u8s!(
            element_size_0,
            element_size_1,
            element_size_2,
            element_size_3
        );
        debug_assert_ne!(capacity, 0, "illegal capacity");
        unsafe {
            //(*mut_vec_ptr).element_count = 0; // zero is default, so shouldn't be needed
            (*mut_vec_ptr).capacity = capacity;
            (*mut_vec_ptr).element_size = element_size;
            (*mut_vec_ptr).padding = VEC_HEADER_MAGIC_CODE;
        }
    }

    #[inline]
    pub fn execute_vec_iter_init(
        &mut self,
        target_vec_iterator_header_reg: u8,
        vec_header_reg: u8,
    ) {
        let vec_header_addr = get_reg!(self, vec_header_reg);

        // Check that vec header is correct
        let vec_header_ptr = self
            .memory
            .get_heap_const_ptr(vec_header_addr as usize)
            .cast::<VecHeader>();
        let vec_header = unsafe { &*vec_header_ptr };

        if vec_header.padding != VEC_HEADER_MAGIC_CODE {
            return self.internal_trap(TrapCode::MemoryCorruption);
        }
        if vec_header.capacity == 0 {
            return self.internal_trap(TrapCode::VecNeverInitialized);
        }

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            let iter_addr = get_reg!(self, target_vec_iterator_header_reg);
            eprintln!(
                "vec_iter_init: iter_addr: {iter_addr:04X} vec_header_addr:{vec_header_addr:04X} element_size: {}",
                vec_header.element_size
            );
        }

        // Assert that element_size is reasonable
        debug_assert!(vec_header.element_size > 0, "Element size cannot be zero");

        let vec_iterator = VecIterator {
            vec_header_heap_ptr: vec_header_addr,
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
            if vec_header.padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                let iter_addr = get_reg!(self, vec_iterator_header_reg);
                let index = (*vec_iterator).index;
                eprintln!(
                    "vec_iter_next: iter_addr: {iter_addr:04X} addr:{vec_header_addr:04X} index:{index} len: {}, capacity: {}, element_size: {}",
                    vec_header.element_count, vec_header.capacity, vec_header.element_size
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
                + VEC_HEADER_PAYLOAD_OFFSET.0
                + (*vec_iterator).index as u32 * vec_header.element_size;

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
            if vec_header.padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }
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
                + VEC_HEADER_PAYLOAD_OFFSET.0
                + (*vec_iterator).index as u32 * vec_header.element_size;

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
    pub fn execute_vec_get(&mut self, element_target_reg: u8, vec_header_ptr_reg: u8, int_reg: u8) {
        let vec_addr = get_reg!(self, vec_header_ptr_reg);

        let vec_header = Self::vec_header_from_heap(&self.memory, vec_addr);
        let index = get_reg!(self, int_reg);
        if vec_header.padding != VEC_HEADER_MAGIC_CODE {
            return self.internal_trap(TrapCode::MemoryCorruption);
        }

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
            return self.internal_trap(TrapCode::VecBoundsFail {
                encountered: index as usize,
                element_count: vec_header.element_count as usize,
            });
        }

        let address_of_element =
            vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 + index * vec_header.element_size;

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
    ) {
        let vec_addr = get_reg!(self, src_vec_header_ptr_reg);

        let mut_vec_ptr = self.memory.get_heap_ptr(vec_addr as usize) as *mut VecHeader;
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            unsafe {
                eprintln!(
                    "vec_push_addr: vec_addr: {vec_addr:08X}, payload_offset: {:?}",
                    (*mut_vec_ptr)
                );
            }
        }

        let len = unsafe { (*mut_vec_ptr).element_count };

        unsafe {
            if (*mut_vec_ptr).padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }

            if (*mut_vec_ptr).capacity == 0 {
                return self.internal_trap(TrapCode::VecNeverInitialized);
            }
            if len >= (*mut_vec_ptr).capacity {
                return self.internal_trap(TrapCode::VecOutOfCapacity {
                    encountered: len,
                    capacity: (*mut_vec_ptr).capacity,
                });
            }
            (*mut_vec_ptr).element_count += 1;
        }

        let address_of_new_element = unsafe {
            vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 + len as u32 * (*mut_vec_ptr).element_size
        };

        set_reg!(self, destination_entry_addr_reg, address_of_new_element);
    }

    #[inline]
    pub fn execute_vec_pop(&mut self, dst_reg: u8, vec_header_ptr_reg: u8) {
        let vec_addr = get_reg!(self, vec_header_ptr_reg);
        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(vec_addr as usize)
            .cast::<VecHeader>();

        unsafe {
            let header = &mut *mut_vec_ptr;

            // Check if vector is empty
            if header.element_count == 0 {
                return self.internal_trap(TrapCode::VecEmpty);
            }
            // Get the last element index
            let last_index = u32::from(header.element_count) - 1;

            // Calculate address of the element to be popped
            let address_of_element_to_pop =
                vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 + last_index * header.element_size;

            header.element_count -= 1;

            set_reg!(self, dst_reg, address_of_element_to_pop);
        }
    }

    #[inline]
    pub fn execute_vec_remove_index(&mut self, vec_header_ptr_reg: u8, remove_index_reg: u8) {
        let vec_addr = get_reg!(self, vec_header_ptr_reg);
        let mut_vec_ptr = self
            .memory
            .get_heap_ptr(vec_addr as usize)
            .cast::<VecHeader>();

        let index = get_reg!(self, remove_index_reg);

        unsafe {
            if index >= u32::from((*mut_vec_ptr).element_count) {
                return self.internal_trap(TrapCode::VecBoundsFail {
                    encountered: index as usize,
                    element_count: (*mut_vec_ptr).element_count as usize,
                });
            }
        }

        let size_of_each_element = unsafe { (*mut_vec_ptr).element_size };
        let address_of_element_to_be_removed =
            vec_addr + VEC_HEADER_PAYLOAD_OFFSET.0 + index * size_of_each_element;

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
