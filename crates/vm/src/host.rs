/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::Memory;
use std::mem;
use swamp_vm_types::{HEAP_PTR_ON_FRAME_SIZE, StringHeader, VEC_PTR_SIZE};

pub struct HostArgs {
    current_frame_relative_offset: usize, // Current argument being processed
    // references into the Vm
    all_memory: *const u8,
    all_memory_len: usize,
    stack_offset: usize,
}

impl HostArgs {
    #[must_use]
    pub fn new(
        all_memory: *const u8,
        all_memory_len: usize,
        stack_offset: usize,
        argument_memory_size: usize,
    ) -> Self {
        // Ensure alignment
        debug_assert_eq!(
            all_memory.addr() % mem::align_of::<u64>(),
            0,
            "Unaligned frame pointer",
        );

        Self {
            all_memory: all_memory,
            all_memory_len: all_memory_len,
            stack_offset,
            current_frame_relative_offset: stack_offset,
        }
    }

    pub fn get_i32(&mut self) -> i32 {
        let size = size_of::<i32>();
        debug_assert!(
            self.current_frame_relative_offset + size <= self.all_memory_len,
            "Attempted to read beyond the end of frame memory"
        );

        unsafe {
            let int_ptr = self.all_memory.add(self.current_frame_relative_offset) as *const i32;
            self.current_frame_relative_offset += size;
            *int_ptr
        }
    }

    pub fn get_str(&mut self) -> &str {
        unsafe {
            let string_heap_addr =
                *(self.all_memory.add(self.current_frame_relative_offset) as *const u32);
            eprintln!("found string_heap_addr: {string_heap_addr} {string_heap_addr:X}");
            self.current_frame_relative_offset += HEAP_PTR_ON_FRAME_SIZE.0 as usize;

            let string_header =
                *(self.all_memory.add(string_heap_addr as usize) as *const StringHeader);
            eprintln!("found string_heap_addr: {string_header:?}");

            let rune_offset = string_header.heap_offset as usize;
            let string_byte_length = string_header.byte_count as usize;

            debug_assert!(
                rune_offset + string_byte_length <= self.all_memory_len,
                "String read out-of-bounds in heap memory"
            );

            let ptr_to_utf8 = self.all_memory.add(rune_offset);

            let bytes = std::slice::from_raw_parts(ptr_to_utf8, string_byte_length);

            std::str::from_utf8_unchecked(bytes)
        }
    }
}

pub type HostFunctionCallback = Box<dyn FnMut(HostArgs)>;
