/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::RegContents;
use std::{
    mem::{align_of, size_of},
    ptr, slice,
};
use swamp_vm_types::StringHeader;

pub struct HostArgs {
    register_index: usize, // Current register being processed
    // references into the Vm
    all_memory: *mut u8,
    all_memory_len: usize,
    registers: *mut RegContents,
    register_count: usize,
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
        registers: *mut RegContents,
        register_count: usize,
    ) -> Self {
        // Ensure alignment
        debug_assert_eq!(
            all_memory.addr() % align_of::<u64>(),
            0,
            "Unaligned frame pointer",
        );

        Self {
            all_memory,
            all_memory_len,
            registers,
            stack_offset,
            register_count,
            register_index: 1, // skip return for now
            function_id,
        }
    }

    pub fn get_ptr(&mut self, register: u8) -> *mut u8 {
        // Get the address stored in the register
        let addr = unsafe { *self.registers.add(register as usize) };
        // Convert the address to a pointer into all_memory
        unsafe { self.all_memory.add(addr as usize) }
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
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }

    pub unsafe fn ptr_to_slice_mut<'a>(ptr: *mut u8, len: usize) -> &'a mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(ptr, len) }
    }

    pub fn read_from_register<T>(&mut self, register_id: u8) -> *const T {
        let src_ptr = self.get_ptr(register_id) as *const T;
        src_ptr
    }

    /// Safe and performant way to get a reference to T from a register
    /// Panics on bounds violations or alignment issues - no defensive programming
    pub fn read_ref_from_register<T>(&self, register_id: u8) -> &T {
        assert!(
            (register_id as usize) < self.register_count,
            "Host call register out of bounds: register {} requested, but only {} registers available",
            register_id,
            self.register_count
        );

        let addr = unsafe { *self.registers.add(register_id as usize) } as usize;
        let size_of_t = size_of::<T>();

        // Bounds check: ensure the entire T fits within memory
        assert!(
            addr + size_of_t <= self.all_memory_len,
            "Host call bounds violation: trying to read {} bytes at address {:#x}, but memory size is {:#x}",
            size_of_t,
            addr,
            self.all_memory_len
        );

        assert_eq!(
            addr % align_of::<T>(),
            0,
            "Host call alignment violation: address {:#x} is not aligned for type {} (requires {}-byte alignment)",
            addr,
            std::any::type_name::<T>(),
            align_of::<T>()
        );

        unsafe { &*(self.all_memory.add(addr) as *const T) }
    }

    /// Unchecked version for a bit extra performance
    pub unsafe fn read_ref_from_register_unchecked<T>(&mut self, register_id: u8) -> &T {
        let addr = unsafe { *self.registers.add(register_id as usize) } as usize;
        unsafe { &*(self.all_memory.add(addr) as *const T) }
    }

    pub fn read_mut_ref_from_register<T>(&mut self, register_id: u8) -> &mut T {
        assert!(
            (register_id as usize) < self.register_count,
            "Host call register out of bounds: register {} requested, but only {} registers available",
            register_id,
            self.register_count
        );

        let addr = unsafe { *self.registers.add(register_id as usize) } as usize;
        let size_of_t = size_of::<T>();

        // Bounds check: ensure the entire T fits within memory
        assert!(
            addr + size_of_t <= self.all_memory_len,
            "Host call bounds violation: trying to read {} bytes at address {:#x}, but memory size is {:#x}",
            size_of_t,
            addr,
            self.all_memory_len
        );

        // Alignment check: ensure T is properly aligned
        assert_eq!(
            addr % align_of::<T>(),
            0,
            "Host call alignment violation: address {:#x} is not aligned for type {} (requires {}-byte alignment)",
            addr,
            std::any::type_name::<T>(),
            align_of::<T>()
        );

        // Safe to create mutable reference - all checks passed
        unsafe { &mut *self.all_memory.add(addr).cast::<T>() }
    }

    /// Unchecked mutable version for extra performance
    pub unsafe fn read_mut_ref_from_register_unchecked<T>(&mut self, register_id: u8) -> &mut T {
        let addr = unsafe { *self.registers.add(register_id as usize) } as usize;
        unsafe { &mut *(self.all_memory.add(addr) as *mut T) }
    }

    pub fn get_register(&self, register_id: u8) -> u32 {
        unsafe { *self.registers.add(register_id as usize) }
    }

    pub fn set_register(&mut self, register_id: u8, data: u32) {
        unsafe {
            *self.registers.add(register_id as usize) = data;
        }
    }

    pub fn write_to_register<T>(&mut self, register_id: u8, data: &T) {
        let dest_ptr = self.get_ptr(register_id) as *mut T;
        let src_ptr = data as *const T;

        unsafe {
            ptr::copy_nonoverlapping(src_ptr, dest_ptr, 1);
        }
    }

    pub fn get_i32(&mut self) -> i32 {
        let val = unsafe { *self.registers.add(self.register_index) } as i32;
        self.register_index += 1;
        val
    }

    pub fn read_string(&self, register_id: u8) -> &str {
        let string_header_addr = unsafe { *self.registers.add(register_id as usize) };
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

    pub fn next_str(&mut self) -> &str {
        self.register_index += 1;
        self.read_string((self.register_index - 1) as u8)
    }
}

pub trait HostFunctionCallback {
    fn dispatch_host_call(&mut self, args: HostArgs);
}
