/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::ExecutionMode;
use crate::memory::Memory;
use crate::{get_reg, i16_from_u8s, set_reg, TrapCode, Vm};
use std::num::ParseIntError;
use std::{mem::size_of, ptr};
use swamp_vm_isa::{
    StringIterator, VecHeader, MAX_STRING_LEN, VEC_HEADER_MAGIC_CODE, VEC_HEADER_PAYLOAD_OFFSET,
};

impl Vm {
    pub fn get_string_iterator_header_ptr_from_reg(
        &self,
        vec_iterator_reg: u8,
    ) -> *mut StringIterator {
        self.get_ptr_from_reg(vec_iterator_reg) as *mut StringIterator
    }

    #[inline]
    fn get_string(&self, reg: u8) -> &str {
        let string_header_addr = get_reg!(self, reg);
        let header_ptr =
            self.memory()
                .get_heap_const_ptr(string_header_addr as usize) as *const VecHeader;
        let header = unsafe { *header_ptr };
        let byte_count = header.element_count;

        #[cfg(feature = "debug_vm")]
        if true || self.debug_operations_enabled {
            eprintln!(
                "get string Memory layout: constants: 0x0-0x{:X}, stack: 0x{:X}-0x{:X}, heap: 0x{:X}-0x{:X}",
                self.memory().constant_memory_size,
                self.memory().stack_start,
                self.memory().stack_offset,
                self.memory().heap_start,
                self.memory().heap_alloc_offset
            );
        }

        if byte_count != 0 {
            debug_assert_eq!(
                header.padding, VEC_HEADER_MAGIC_CODE,
                "string is corrupt. it is saying it has length {byte_count}, left: {}, right: {VEC_HEADER_MAGIC_CODE}",
                header.padding
            );
            debug_assert!(
                header.element_count < MAX_STRING_LEN,
                "string of strange length. it is saying it has length {byte_count}, left: {}, right: {VEC_HEADER_MAGIC_CODE}",
                header.padding
            );
        }

        let runes_ptr = self.memory().get_heap_const_ptr(
            (string_header_addr as usize) + VEC_HEADER_PAYLOAD_OFFSET.0 as usize,
        );

        unsafe {
            let bytes = std::slice::from_raw_parts(runes_ptr, byte_count as usize);

            if byte_count > 0 {
                let s = std::str::from_utf8(bytes).unwrap_or("INVALID_UTF8");
                #[cfg(feature = "debug_vm")]
                if true || self.debug_operations_enabled {
                    eprintln!("String content: \"{s}\" at addr {string_header_addr:X}");
                }
            }

            std::str::from_utf8_unchecked(bytes)
        }
    }

    #[inline]
    pub fn execute_string_duplicate(&mut self, target_string_view_reg: u8, string_storage: u8) {
        #[cfg(feature = "debug_vm")]
        if true || self.debug_operations_enabled {
            eprintln!("=== STRING DUPLICATE OPERATION ===");
        }
        let str_a = self.get_string(string_storage).to_string();
        self.create_string(target_string_view_reg, &str_a);
    }

    #[inline]
    pub fn execute_string_append(&mut self, target_string_reg: u8, string_a: u8, string_b: u8) {
        #[cfg(feature = "debug_vm")]
        if true || self.debug_operations_enabled {
            eprintln!("=== STRING_APPEND OPERATION ===");
            eprintln!(
                "Memory layout: constants: 0x0-0x{:X}, stack: 0x{:X}-0x{:X}, heap: 0x{:X}-0x{:X}",
                self.memory().constant_memory_size,
                self.memory().stack_start,
                self.memory().stack_offset,
                self.memory().heap_start,
                self.memory().heap_alloc_offset
            );

            // Debug: Print register values using the get_reg macro
            let reg_a_value = get_reg!(self, string_a);
            let reg_b_value = get_reg!(self, string_b);

            eprintln!("String A register {string_a}: 0x{reg_a_value:X}");
            eprintln!("String B register {string_b}: 0x{reg_b_value:X}");
            eprintln!("Target register {target_string_reg}");
        }

        let str_a = self.get_string(string_a);
        let str_b = self.get_string(string_b);

        let result = str_a.to_string() + str_b;

        #[cfg(feature = "debug_vm")]
        if true || self.debug_operations_enabled {
            eprintln!(
                "Concatenated string: \"{}\" (length: {})",
                result,
                result.len()
            );
        }

        self.create_string(target_string_reg, &result);
        // Debug: Print final register value
        let final_reg_value = get_reg!(self, target_string_reg);
        #[cfg(feature = "debug_vm")]
        if true || self.debug_operations_enabled {
            eprintln!("Final target register value: 0x{final_reg_value:X}");
        }
    }

    #[inline]
    pub fn execute_string_repeat(&mut self, target_string_reg: u8, string_a: u8, repeat_reg: u8) {
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("=== STRING_REPEAT OPERATION ===");
            eprintln!(
                "Memory layout: constants: 0x0-0x{:X}, stack: 0x{:X}-0x{:X}, heap: 0x{:X}-0x{:X}",
                self.memory().constant_memory_size,
                self.memory().stack_start,
                self.memory().stack_offset,
                self.memory().heap_start,
                self.memory().heap_alloc_offset
            );

            // Debug: Print register values
            let reg_a_value = get_reg!(self, string_a);
            let repeat_value = get_reg!(self, repeat_reg);
            eprintln!("String A register {string_a}: 0x{reg_a_value:X}");
            eprintln!("Repeat count register {repeat_reg}: {}", repeat_value);
            eprintln!("Target register {target_string_reg}");
        }

        // Load the input string
        let str_a = self.get_string(string_a);

        let count = get_reg!(self, repeat_reg) as usize;

        // Perform the repeat
        let result = str_a.repeat(count);

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "Repeated string: \"{}\" (length: {}, repeated {} times)",
                result,
                result.len(),
                count
            );
        }

        // Store the result back into the target register
        self.create_string(target_string_reg, &result);

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            let final_reg_value = get_reg!(self, target_string_reg);
            eprintln!("Final target register value: 0x{final_reg_value:X}");
        }
    }

    #[inline]
    pub fn execute_string_cmp(&mut self, dest_reg: u8, string_a: u8, string_b: u8) {
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("=== STRING_COMPARE OPERATION ===");
            eprintln!(
                "Memory layout: constants: 0x0-0x{:X}, stack: 0x{:X}-0x{:X}, heap: 0x{:X}-0x{:X}",
                self.memory().constant_memory_size,
                self.memory().stack_start,
                self.memory().stack_offset,
                self.memory().heap_start,
                self.memory().heap_alloc_offset
            );
        }

        // Debug: Print register values
        let reg_a_value = get_reg!(self, string_a);
        let reg_b_value = get_reg!(self, string_b);

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("String A register {string_a}: 0x{reg_a_value:X}");
            eprintln!("String B register {string_b}: 0x{reg_b_value:X}");
        }

        let str_a = self.get_string(string_a);
        let str_b = self.get_string(string_b);

        let result = str_a == str_b;

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("String comparison result: {result}");
        }

        // Store the result
        set_reg!(self, dest_reg, result as u32);
    }

    /// Return the same string but with quotes.
    #[inline]
    pub fn execute_string_starts_with(
        &mut self,
        dest_reg: u8,
        source_string: u8,
        other_string_reg: u8,
    ) {
        let source_str = self.get_string(source_string);
        let other_str = self.get_string(other_string_reg);

        set_reg!(self, dest_reg, source_str.starts_with(other_str))
    }

    fn str_to_int(text: &str) -> Result<i32, ParseIntError> {
        let text = text.replace('_', "");
        text.strip_prefix("0x").map_or_else(
            || {
                text.strip_prefix("-0x").map_or_else(
                    || text.parse::<i32>(),
                    |rest| i32::from_str_radix(rest, 16).map(|x| -x),
                )
            },
            |rest| i32::from_str_radix(rest, 16),
        )
    }

    /// Parses the string to float and returns the tuple with result
    #[inline]
    pub fn execute_string_to_float(&mut self, dest_tuple_reg: u8, source_string: u8) {
        let source_str = self.get_string(source_string).to_string();

        let tuple_addr = get_reg!(self, dest_tuple_reg);

        let tuple_ptr = self.memory_mut().get_heap_ptr(tuple_addr as usize);

        let float_value = source_str.parse::<f32>();

        if let Ok(value) = float_value {
            unsafe {
                let fp = fixed32::Fp::from(value);
                *(tuple_ptr as *mut u32) = fp.inner() as u32;
                *tuple_ptr.add(4) = 0x01;
            }
        } else {
            unsafe {
                *(tuple_ptr as *mut u32) = 0;
                *tuple_ptr.add(4) = 0x00;
            }
        }
    }

    #[inline]
    pub fn execute_string_to_int(&mut self, dest_tuple_reg: u8, source_string: u8) {
        let source_str = self.get_string(source_string).to_string();

        let tuple_addr = get_reg!(self, dest_tuple_reg);

        let tuple_ptr = self.memory_mut().get_heap_ptr(tuple_addr as usize);

        let int_value = Self::str_to_int(&source_str);

        if let Ok(value) = int_value {
            unsafe {
                *(tuple_ptr as *mut i32) = value;
                *tuple_ptr.add(4) = 0x01;
            }
        } else {
            unsafe {
                *(tuple_ptr as *mut u32) = 0;
                *tuple_ptr.add(4) = 0x00;
            }
        }
    }

    /// Return the same string but with quotes.
    #[inline]
    pub fn execute_string_to_string(&mut self, dest_reg: u8, source_string: u8) {
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("=== STRING_TO_STRING OPERATION ===");
            eprintln!(
                "Memory layout: constants: 0x0-0x{:X}, stack: 0x{:X}-0x{:X}, heap: 0x{:X}-0x{:X}",
                self.memory().constant_memory_size,
                self.memory().stack_start,
                self.memory().stack_offset,
                self.memory().heap_start,
                self.memory().heap_alloc_offset
            );

            let source_reg_value = get_reg!(self, source_string);
            eprintln!("Source string register {source_string}: 0x{source_reg_value:X}");
        }

        let source_str = self.get_string(source_string);

        // Create the formatted string with quotes
        let mut formatted_string = String::with_capacity(source_str.len() + 2);
        formatted_string.push('"');
        formatted_string.push_str(source_str);
        formatted_string.push('"');

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "Formatted string: \"{}\" (length: {})",
                formatted_string,
                formatted_string.len()
            );
        }

        self.create_string(dest_reg, &formatted_string);

        let final_reg_value = get_reg!(self, dest_reg);

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("Final destination register value: 0x{final_reg_value:X}");
        }
    }

    pub fn read_string(&self, heap_addr: u32, heap: &Memory) -> &str {
        let string_header_ptr = heap.get_heap_const_ptr(heap_addr as usize) as *const VecHeader;
        let string_header = unsafe { *string_header_ptr };

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "read_string: addr=0x{heap_addr:X}, capacity={}, byte_count={}, padding=0x{:X}",
                string_header.capacity, string_header.element_count, string_header.padding
            );
        }

        let byte_count = string_header.element_count as usize;

        #[cfg(feature = "debug_vm")]
        if string_header.element_count != 0 {
            debug_assert_eq!(
                string_header.padding, VEC_HEADER_MAGIC_CODE,
                "CORRUPTION DETECTED in read_string: String header at 0x{heap_addr:X} has invalid padding 0x{:X}, should be 0x{VEC_HEADER_MAGIC_CODE:X}",
                string_header.padding
            );
            debug_assert_eq!(
                string_header.capacity, string_header.element_count,
                "Corruption. strings should never change"
            );
            // TODO: just a hack for now to see if it is plausible.
            debug_assert!(
                byte_count < 1024,
                "Strange. string byte_count {byte_count} is unreasonably large"
            );
        }

        // String data follows directly after the header
        let string_data_ptr = unsafe {
            heap.get_heap_const_ptr(heap_addr as usize + VEC_HEADER_PAYLOAD_OFFSET.0 as usize)
        };

        unsafe {
            let bytes = std::slice::from_raw_parts(string_data_ptr, byte_count);
            match std::str::from_utf8(bytes) {
                Ok(s) => s,
                Err(e) => {
                    panic!("ERROR: Invalid UTF-8 string data at 0x{heap_addr:X}: {e}");
                    ""
                }
            }
        }
    }

    /// Strings are immutable, can not be altered after they have been created.
    /// They can be safely shared and the pointer can be blittable when inside composite types.
    /// The string data is stored directly after the header in memory.
    pub(crate) fn create_string(&mut self, dst_reg: u8, string: &str) {
        let rune_bytes = string.as_bytes();
        let byte_count = rune_bytes.len();
        let cap_bytes = if byte_count == 0 { 1 } else { byte_count };

        debug_assert!(
            byte_count <= MAX_STRING_LEN as usize,
            "String too large: {byte_count} bytes"
        );

        // Calculate total size needed: header + string data
        // We assume that StringHeader is aligned to u32
        let total_size = size_of::<VecHeader>() + byte_count;

        let header_addr_in_heap = self.memory.heap_allocate_secret(total_size);

        #[cfg(feature = "debug_vm")]
        match self.memory.execution_mode {
            ExecutionMode::ConstantEvaluation => {
                // In constant evaluation, strings should be in heap which is directly after constant area
                debug_assert!(
                    header_addr_in_heap >= self.memory.heap_start as u32
                        && header_addr_in_heap < self.memory.heap_alloc_offset as u32,
                    "String allocation at 0x{header_addr_in_heap:X} is not in heap during constant evaluation",
                );
            }
            ExecutionMode::NormalExecution => {
                // In normal execution, strings should be in heap which is after stack
                debug_assert!(
                    header_addr_in_heap >= self.memory.heap_start as u32
                        && header_addr_in_heap < self.memory.heap_alloc_offset as u32,
                    "String allocation at 0x{header_addr_in_heap:X} is not in heap during normal execution",
                );
            }
        }

        let string_header = VecHeader {
            capacity: cap_bytes as u16,
            element_count: byte_count as u16,
            element_size: 1,
            padding: VEC_HEADER_MAGIC_CODE,
        };

        unsafe {
            let header_ptr =
                self.memory.get_heap_ptr(header_addr_in_heap as usize) as *mut VecHeader;
            ptr::write(header_ptr, string_header);

            let string_data_ptr = self
                .memory
                .get_heap_ptr(header_addr_in_heap as usize + VEC_HEADER_PAYLOAD_OFFSET.0 as usize);
            ptr::copy_nonoverlapping(rune_bytes.as_ptr(), string_data_ptr, byte_count);
        }

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "Creating string: '{string}', header at 0x{header_addr_in_heap:X}, capacity={byte_count}, byte_count={byte_count}, padding=0x{VEC_HEADER_MAGIC_CODE:X}"
            );
        }

        set_reg!(self, dst_reg, header_addr_in_heap);
    }

    #[inline]
    pub fn execute_string_iter_init(
        &mut self,
        target_string_iterator_header_reg: u8,
        string_header_reg: u8,
    ) {
        let string_header_addr = get_reg!(self, string_header_reg);

        // Check that vec header is correct
        let string_header_ptr = self
            .memory
            .get_heap_const_ptr(string_header_addr as usize)
            .cast::<VecHeader>();
        let string_header = unsafe { &*string_header_ptr };

        if string_header.padding != VEC_HEADER_MAGIC_CODE {
            return self.internal_trap(TrapCode::MemoryCorruption);
        }

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            let iter_addr = get_reg!(self, target_string_iterator_header_reg);
            eprintln!(
                "string_iter_init: iter_addr: {iter_addr:04X} string_header_addr:{string_header_addr:04X} element_size: {}",
                string_header.element_size
            );
        }
        let string_iterator = StringIterator {
            string_heap_ptr: string_header_addr,
            byte_index: 0,
            index: 0,
        };

        let string_iterator_mut_ptr =
            self.get_ptr_from_reg(target_string_iterator_header_reg) as *mut StringIterator;

        unsafe {
            ptr::write(string_iterator_mut_ptr, string_iterator);
        }
    }

    #[inline]
    pub fn execute_string_iter_next(
        &mut self,
        string_iterator_header_reg: u8,
        target_variable: u8,
        branch_offset_lower: u8,
        branch_offset_upper: u8,
    ) {
        let string_iterator =
            self.get_string_iterator_header_ptr_from_reg(string_iterator_header_reg);

        unsafe {
            let string_header_addr = (*string_iterator).string_heap_ptr;
            let string_header_ptr = self
                .memory
                .get_heap_const_ptr(string_header_addr as usize)
                .cast::<VecHeader>();

            let string_header_raw_ptr = self.memory.get_heap_const_ptr(string_header_addr as usize);

            let string_header = &*string_header_ptr;
            if string_header.padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                let iter_addr = get_reg!(self, string_iterator_header_reg);
                let index = (*string_iterator).byte_index;
                eprintln!(
                    "string_iter_next: iter_addr: {iter_addr:04X} addr:{string_header_addr:04X} index:{index} len: {}, capacity: {}",
                    string_header.element_count, string_header.capacity
                );
            }

            // Check if we've reached the end
            if (*string_iterator).byte_index >= string_header.element_count {
                // Jump to the provided address if we're done
                let branch_offset = i16_from_u8s!(branch_offset_lower, branch_offset_upper);

                #[cfg(feature = "debug_vm")]
                {
                    if self.debug_operations_enabled {
                        eprintln!("string_iter_next complete. jumping with offset {branch_offset}");
                    }
                }

                self.pc = (self.pc as i32 + branch_offset as i32) as usize;

                return;
            }

            let current_byte_index = (*string_iterator).byte_index as usize;
            let remaining_byte_count = (string_header.element_count as usize) - current_byte_index;
            let payload_ptr = string_header_raw_ptr.add(VEC_HEADER_PAYLOAD_OFFSET.0 as usize);

            let remaining_bytes = std::slice::from_raw_parts(
                payload_ptr.add(current_byte_index),
                remaining_byte_count,
            );

            match std::str::from_utf8(remaining_bytes) {
                Ok(valid_str) => {
                    if let Some(c) = valid_str.chars().next() {
                        // Place the decoded character (a Char - u32) into the target register
                        // Advance the iterator by the actual byte width of the character
                        let advancement = c.len_utf8() as u16;
                        (*string_iterator).byte_index += advancement;

                        let raw_u32 = c as u32;
                        set_reg!(self, target_variable, raw_u32);
                    } else {
                        self.internal_trap(TrapCode::InvalidUtf8Sequence);
                    }
                }
                Err(_) => {
                    // The string data in the VM memory is corrupted/invalid
                    self.internal_trap(TrapCode::InvalidUtf8Sequence);
                }
            }
        }
    }

    #[inline]
    pub fn execute_string_iter_next_pair(
        &mut self,
        string_iterator_header_reg: u8,
        target_key_reg: u8,
        target_value_reg: u8,
        branch_offset_lower: u8,
        branch_offset_upper: u8,
    ) {
        let string_iterator =
            self.get_string_iterator_header_ptr_from_reg(string_iterator_header_reg);

        unsafe {
            let string_header_addr = (*string_iterator).string_heap_ptr;
            let string_header_ptr = self
                .memory
                .get_heap_const_ptr(string_header_addr as usize)
                .cast::<VecHeader>();

            let string_header_raw_ptr = self.memory.get_heap_const_ptr(string_header_addr as usize);

            let string_header = &*string_header_ptr;
            if string_header.padding != VEC_HEADER_MAGIC_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                let iter_addr = get_reg!(self, string_iterator_header_reg);
                let index = (*string_iterator).byte_index;
                eprintln!(
                    "string_iter_next: iter_addr: {iter_addr:04X} addr:{string_header_addr:04X} index:{index} len: {}, capacity: {}",
                    string_header.element_count, string_header.capacity
                );
            }

            // Check if we've reached the end
            if (*string_iterator).byte_index >= string_header.element_count {
                // Jump to the provided address if we're done
                let branch_offset = i16_from_u8s!(branch_offset_lower, branch_offset_upper);

                #[cfg(feature = "debug_vm")]
                {
                    if self.debug_operations_enabled {
                        eprintln!("string_iter_next complete. jumping with offset {branch_offset}");
                    }
                }

                self.pc = (self.pc as i32 + branch_offset as i32) as usize;

                return;
            }

            let current_byte_index = (*string_iterator).byte_index as usize;
            let remaining_byte_count = (string_header.element_count as usize) - current_byte_index;
            let payload_ptr = string_header_raw_ptr.add(VEC_HEADER_PAYLOAD_OFFSET.0 as usize);

            let remaining_bytes = std::slice::from_raw_parts(
                payload_ptr.add(current_byte_index),
                remaining_byte_count,
            );

            match std::str::from_utf8(remaining_bytes) {
                Ok(valid_str) => {
                    if let Some(c) = valid_str.chars().next() {
                        // Place the decoded character (a Char - u32) into the target register
                        // Advance the iterator by the actual byte width of the character
                        let advancement = c.len_utf8() as u16;
                        (*string_iterator).byte_index += advancement;

                        let raw_u32 = c as u32;
                        eprintln!(
                            "raw: {raw_u32} advancement {advancement} -> r{target_value_reg}"
                        );
                        set_reg!(self, target_key_reg, (*string_iterator).index);
                        set_reg!(self, target_value_reg, raw_u32);

                        (*string_iterator).index += 1;
                    } else {
                        self.internal_trap(TrapCode::InvalidUtf8Sequence);
                    }
                }
                Err(_) => {
                    // The string data in the VM memory is corrupted/invalid
                    self.internal_trap(TrapCode::InvalidUtf8Sequence);
                }
            }
        }
    }
}
