/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::{ExecutionMode, Memory};
use crate::{Vm, get_reg, set_reg};
use std::{mem::size_of, ptr};
use swamp_vm_types::{STRING_PAYLOAD_OFFSET, STRING_SECRET, StringHeader};

impl Vm {
    pub fn read_string_header_from_ptr_reg(&self, string_ptr_reg: u8) -> StringHeader {
        let header_addr = get_reg!(self, string_ptr_reg) as usize;
        let header_ptr = self.get_map_header_const(header_addr as u32) as *const StringHeader;

        // Debug: Check if the pointer is within valid memory regions
        let memory_size = self.memory().memory_size;
        let is_in_constant = header_addr < self.memory().constant_memory_size;
        let is_in_heap = header_addr >= self.memory().heap_start
            && header_addr < self.memory().heap_alloc_offset;
        let is_in_stack =
            header_addr >= self.memory().stack_start && header_addr < self.memory().stack_offset;

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "String header ptr: 0x{header_addr:X}, in_constant: {is_in_constant}, in_heap: {is_in_heap}, in_stack: {is_in_stack}"
            );
        }

        debug_assert!(
            header_addr < memory_size,
            "String header pointer 0x{header_addr:X} is outside total memory bounds (0x0-0x{memory_size:X})"
        );

        #[cfg(feature = "debug_vm")]
        match self.memory().execution_mode {
            ExecutionMode::ConstantEvaluation => {
                // In constant evaluation mode, strings should be in constant memory or heap
                assert!(
                    (is_in_constant || is_in_heap),
                    "String header at 0x{header_addr:X} is not in constant memory or heap during constant evaluation"
                );
            }
            ExecutionMode::NormalExecution => {}
        }

        let header_value: StringHeader = unsafe { std::ptr::read_unaligned(header_ptr) };

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "String header values: capacity={}, byte_count={}, padding=0x{:X}",
                header_value.capacity, header_value.byte_count, header_value.padding
            );
        }

        // Only validate the padding if the string has content
        if header_value.byte_count != 0 {
            debug_assert_eq!(
                header_value.padding, STRING_SECRET,
                "CORRUPTION DETECTED: String header at 0x{header_addr:X} has invalid padding 0x{:X}, should be 0x{:X}",
                header_value.padding, STRING_SECRET
            );
        }

        header_value
    }

    #[inline]
    fn get_string(&self, reg: u8) -> &str {
        let string_header_addr = get_reg!(self, reg);
        let header_ptr =
            self.memory()
                .get_heap_const_ptr(string_header_addr as usize) as *const StringHeader;
        let header = unsafe { *header_ptr };
        let byte_count = header.byte_count;

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "Memory layout: constants: 0x0-0x{:X}, stack: 0x{:X}-0x{:X}, heap: 0x{:X}-0x{:X}",
                self.memory().constant_memory_size,
                self.memory().stack_start,
                self.memory().stack_offset,
                self.memory().heap_start,
                self.memory().heap_alloc_offset
            );
        }

        if byte_count != 0 {
            debug_assert_eq!(
                header.padding, STRING_SECRET,
                "string is corrupt. it is saying it has length {byte_count}, left: {}, right: {STRING_SECRET}",
                header.padding
            );
        }

        let runes_ptr = self
            .memory()
            .get_heap_const_ptr((string_header_addr as usize) + STRING_PAYLOAD_OFFSET.0 as usize);

        unsafe {
            let bytes = std::slice::from_raw_parts(runes_ptr, byte_count as usize);

            if byte_count > 0 {
                let s = std::str::from_utf8(bytes).unwrap_or("INVALID_UTF8");
                #[cfg(feature = "debug_vm")]
                if self.debug_operations_enabled {
                    eprintln!("String content: \"{s}\"");
                }
            }

            std::str::from_utf8_unchecked(bytes)
        }
    }
    #[inline]
    pub fn execute_string_append(&mut self, target_string_addr: u8, string_a: u8, string_b: u8) {
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
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
            let target_reg = get_reg!(self, target_string_addr);

            eprintln!("String A register {string_a}: 0x{reg_a_value:X}");
            eprintln!("String B register {string_b}: 0x{reg_b_value:X}");
            eprintln!("Target register {target_string_addr}: 0x{target_reg:X}");
        }

        let str_a = self.get_string(string_a);
        let str_b = self.get_string(string_b);

        let result = str_a.to_string() + str_b;

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "Concatenated string: \"{}\" (length: {})",
                result,
                result.len()
            );
        }

        self.create_string(target_string_addr, &result);
        // Debug: Print final register value
        let final_reg_value = get_reg!(self, target_string_addr);
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("Final target register value: 0x{:X}", final_reg_value);
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
        let string_header_ptr = heap.get_heap_const_ptr(heap_addr as usize) as *const StringHeader;
        let mut string_header = unsafe { *string_header_ptr };

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "read_string: addr=0x{heap_addr:X}, capacity={}, byte_count={}, padding=0x{:X}",
                string_header.capacity, string_header.byte_count, string_header.padding
            );
        }

        let byte_count = string_header.byte_count as usize;

        #[cfg(feature = "debug_vm")]
        if string_header.byte_count != 0 {
            debug_assert_eq!(
                string_header.padding, STRING_SECRET,
                "CORRUPTION DETECTED in read_string: String header at 0x{heap_addr:X} has invalid padding 0x{:X}, should be 0x{STRING_SECRET:X}",
                string_header.padding
            );
            debug_assert_eq!(
                string_header.capacity, string_header.byte_count,
                "Corruption. strings should never change"
            );
            // TODO: just a hack for now to see if it is plausible.
            debug_assert!(
                byte_count < 512,
                "Strange. string byte_count {byte_count} is unreasonably large"
            );
        }

        // String data follows directly after the header
        let string_data_ptr = unsafe {
            heap.get_heap_const_ptr(heap_addr as usize + STRING_PAYLOAD_OFFSET.0 as usize)
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

        debug_assert!(
            byte_count <= u16::MAX as usize,
            "String too large: {byte_count} bytes"
        );

        // Calculate total size needed: header + string data
        // We assume that StringHeader is aligned to u32
        let total_size = size_of::<StringHeader>() + byte_count;

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

        let string_header = StringHeader {
            capacity: byte_count as u16,
            byte_count: byte_count as u16,
            padding: STRING_SECRET,
        };

        unsafe {
            let header_ptr =
                self.memory.get_heap_ptr(header_addr_in_heap as usize) as *mut StringHeader;
            ptr::write(header_ptr, string_header);

            let string_data_ptr = self
                .memory
                .get_heap_ptr(header_addr_in_heap as usize + STRING_PAYLOAD_OFFSET.0 as usize);
            ptr::copy_nonoverlapping(rune_bytes.as_ptr(), string_data_ptr, byte_count);
        }

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "Creating string: '{string}', header at 0x{header_addr_in_heap:X}, capacity={byte_count}, byte_count={byte_count}, padding=0x{STRING_SECRET:X}"
            );
        }

        set_reg!(self, dst_reg, header_addr_in_heap);
    }
}
