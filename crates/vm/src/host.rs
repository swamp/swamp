/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::RegContents;
use std::{mem, slice};
use swamp_vm_types::StringHeader;

pub struct HostArgs {
    register_index: usize, // Current register being processed
    // references into the Vm
    all_memory: *const u8,
    all_memory_len: usize,
    registers: Vec<u32>,
    stack_offset: usize,
    pub function_id: u16,
}

impl HostArgs {
    #[must_use]
    pub unsafe fn new(
        function_id: u16,
        all_memory: *const u8,
        all_memory_len: usize,
        stack_offset: usize,
        registers: *const RegContents,
        register_count: usize,
    ) -> Self {
        // Ensure alignment
        debug_assert_eq!(
            all_memory.addr() % mem::align_of::<u64>(),
            0,
            "Unaligned frame pointer",
        );

        unsafe {
            Self {
                all_memory,
                all_memory_len,
                registers: slice::from_raw_parts(registers, register_count).to_vec(),
                stack_offset,
                register_index: 1, // skip return for now
                function_id,
            }
        }
    }

    pub fn get_i32(&mut self) -> i32 {
        let val = self.registers[self.register_index] as i32;
        self.register_index += 1;
        val
    }

    pub fn get_str(&mut self) -> &str {
        let string_header_addr = self.registers[self.register_index];
        self.register_index += 1;
        unsafe {
            let string_header =
                *(self.all_memory.add(string_header_addr as usize) as *const StringHeader);

            let static_rune_offset = string_header.heap_offset as usize;
            let string_byte_length = string_header.byte_count as usize;

            debug_assert!(
                static_rune_offset + string_byte_length <= self.all_memory_len,
                "String read out-of-bounds in heap memory"
            );

            let ptr_to_utf8 = self.all_memory.add(static_rune_offset);

            let bytes = slice::from_raw_parts(ptr_to_utf8, string_byte_length);

            std::str::from_utf8_unchecked(bytes)
        }
    }
}

pub trait HostFunctionCallback {
    fn dispatch_host_call(&mut self, args: HostArgs);
}
