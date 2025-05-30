/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::RegContents;
use std::{mem, ptr, slice};
use swamp_vm_types::StringHeader;

pub struct HostArgs {
    register_index: usize, // Current register being processed
    // references into the Vm
    all_memory: *mut u8,
    all_memory_len: usize,
    registers: Vec<u32>,
    stack_offset: usize,
    pub function_id: u16,
}

impl HostArgs {
    #[must_use]
    pub unsafe fn new(
        function_id: u16,
        all_memory: *mut u8,
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

    pub fn get_ptr(&mut self, register: u8) -> *mut u8 {
        let addr = self.registers[register as usize];

        let p: *mut u8 = unsafe { self.all_memory.add(addr as usize) };
        p
    }

    pub fn print_bytes(label: &str, bytes: &[u8]) {
        print!("{label}: [");
        for (i, &b) in bytes.iter().enumerate() {
            print!("{b:02X}");
            if i < bytes.len() - 1 {
                print!(" ");
            }
        }
        println!("]");
    }

    pub unsafe fn ptr_to_slice<'a>(ptr: *const u8, len: usize) -> &'a [u8] {
        std::slice::from_raw_parts(ptr, len)
    }

    pub unsafe fn ptr_to_slice_mut<'a>(ptr: *mut u8, len: usize) -> &'a mut [u8] {
        std::slice::from_raw_parts_mut(ptr, len)
    }

    pub fn write_to_register<T>(&mut self, register_id: u8, data: &T) {
        let dest_ptr = self.get_ptr(register_id) as *mut T;

        let src_ptr = data as *const T;

        unsafe {
            ptr::copy_nonoverlapping(src_ptr, dest_ptr, 1);
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
