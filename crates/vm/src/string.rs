/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::Memory;
use crate::{Vm, set_reg};
use swamp_vm_types::StringHeader;

impl Vm {
    /*
    #[inline]
    pub fn execute_string_from_constant_slice(
        &mut self,
        target_string_addr: u16,
        constant_lower: u16,
        constant_upper: u16,
        byte_count: u16,
    ) {
        let constant_offset = ((constant_upper as u32) << 16) | (constant_lower as u32);

        let heap_offset = self.memory_allocate(byte_count as usize);

        // Copy the constant slice to heap
        unsafe {
            let source_constant = self.memory_ptr_immut_at(constant_offset as usize);
            let dest_ptr = self.memory_ptr_at(heap_offset as usize);

            ptr::copy_nonoverlapping(source_constant, dest_ptr, byte_count as usize);
        }

        let string_header = StringHeader {
            byte_count,
            capacity: byte_count,
            heap_offset,
        };

        let header_offset = self.memory_allocate(STRING_HEADER_SIZE.into());

        // Copy String header to heap
        unsafe {
            let header_ptr = self.memory_ptr_at(header_offset as usize) as *mut StringHeader;
            ptr::write(header_ptr, string_header);
        }

        // Copy the heap offset of the string header to the frame.
        unsafe {
            let target_ptr = self.frame_ptr_at(target_string_addr) as *mut u32;
            ptr::write(target_ptr, header_offset);
        }

    }



         */
    pub fn read_string_header_from_ptr_reg(&self, vec_header_ptr_reg: u8) -> StringHeader {
        let header_storage_ptr_value =
            self.get_const_ptr_from_reg(vec_header_ptr_reg) as *const StringHeader;

        let vec_header_const_ptr_typed = header_storage_ptr_value as *const StringHeader;

        let header_value: StringHeader =
            unsafe { std::ptr::read_unaligned(vec_header_const_ptr_typed) };

        header_value
    }

    #[inline]
    fn get_string(&self, reg: u8) -> &str {
        let header = self.read_string_header_from_ptr_reg(reg);
        let byte_count = header.byte_count;
        unsafe {
            let runes = self
                .memory()
                .get_heap_const_ptr(header.heap_offset as usize);

            let bytes = std::slice::from_raw_parts(runes, byte_count as usize);

            std::str::from_utf8_unchecked(bytes)
        }
    }
    #[inline]
    pub fn execute_string_append(&mut self, target_string_addr: u8, string_a: u8, string_b: u8) {
        let str_a = self.get_string(string_a);
        let str_b = self.get_string(string_b);
        let result = str_a.to_string() + str_b;

        self.create_string(target_string_addr, &result);
    }

    #[inline]
    pub fn execute_string_cmp(&mut self, dest_reg: u8, string_a: u8, string_b: u8) {
        let str_a = self.get_string(string_a).to_string();
        let str_b = self.get_string(string_b).to_string();

        set_reg!(self, dest_reg, str_a == str_b);
    }

    pub fn read_string(heap_addr: u32, heap: &Memory) -> &str {
        let string_header =
            unsafe { *(heap.get_heap_const_ptr(heap_addr as usize) as *const StringHeader) };

        let runes = unsafe { heap.get_heap_const_ptr(string_header.heap_offset as usize) };

        unsafe {
            let bytes = std::slice::from_raw_parts(runes, string_header.byte_count as usize);

            std::str::from_utf8_unchecked(bytes)
        }
    }

    /// Strings immutable, can not be altered after it has been created.
    /// They can be safely shared and the pointer can be blittable when inside composite types.
    /// It is stored in the heap, and the pointer serves as the 'handle' to the string.
    pub(crate) fn create_string(&mut self, dst_reg: u8, string: &str) {
        let rune_bytes = string.as_bytes();
        let runes_in_heap = self.memory.heap_allocate_with_data(rune_bytes);

        let string_header = StringHeader {
            heap_offset: runes_in_heap,
            byte_count: rune_bytes.len() as u32,
        };

        let header_addr_in_heap = self.memory.heap_allocate_secret(size_of::<StringHeader>());

        let header_ptr_in_heap =
            self.memory.get_heap_ptr(header_addr_in_heap as usize) as *mut StringHeader;

        unsafe {
            *header_ptr_in_heap = string_header;
        }

        set_reg!(self, dst_reg, header_addr_in_heap);
    }
}
