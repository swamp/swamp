/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::RegContents;
use std::{
    mem::{align_of, size_of},
    ptr, slice,
};
use swamp_vm_types::{STRING_PAYLOAD_OFFSET, StringHeader};

pub struct HostArgs {
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
            function_id,
        }
    }

    /// Get a raw pointer from a register value
    pub fn ptr(&mut self, register: u8) -> *mut u8 {
        let addr = unsafe { *self.registers.add(register as usize) };
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

    /// Get a typed pointer from a register
    pub fn ptr_as<T>(&mut self, register_id: u8) -> *const T {
        self.ptr(register_id) as *const T
    }

    /// Get a safe reference to T from a register with bounds and alignment checks
    pub fn get<T>(&self, register_id: u8) -> &T {
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

    /// Get a reference to T from a register without safety checks (for performance)
    pub unsafe fn get_unchecked<T>(&mut self, register_id: u8) -> &T {
        let addr = unsafe { *self.registers.add(register_id as usize) } as usize;
        unsafe { &*(self.all_memory.add(addr) as *const T) }
    }

    /// Get a mutable reference to T from a register with bounds and alignment checks
    pub fn get_mut<T>(&mut self, register_id: u8) -> &mut T {
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

    /// Get a mutable reference to T from a register without safety checks (for performance)
    pub unsafe fn get_mut_unchecked<T>(&mut self, register_id: u8) -> &mut T {
        let addr = unsafe { *self.registers.add(register_id as usize) } as usize;
        unsafe { &mut *(self.all_memory.add(addr) as *mut T) }
    }

    /// Get the raw register value as u32
    pub fn register(&self, register_id: u8) -> u32 {
        unsafe { *self.registers.add(register_id as usize) }
    }

    /// Get the register value as i32
    pub fn register_i32(&self, register_id: u8) -> i32 {
        unsafe { *self.registers.add(register_id as usize) as i32 }
    }

    /// Set a register to a u32 value
    pub fn set_register(&mut self, register_id: u8, data: u32) {
        unsafe {
            *self.registers.add(register_id as usize) = data;
        }
    }

    /// Write data to the memory location pointed to by a register
    pub fn write<T>(&mut self, register_id: u8, data: &T) {
        let dest_ptr = self.ptr(register_id) as *mut T;
        let src_ptr = data as *const T;

        unsafe {
            ptr::copy_nonoverlapping(src_ptr, dest_ptr, 1);
        }
    }

    /// Get a string from a register
    pub fn string(&self, register_id: u8) -> &str {
        let string_header_addr = unsafe { *self.registers.add(register_id as usize) };
        unsafe {
            let string_header =
                *(self.all_memory.add(string_header_addr as usize) as *const StringHeader);

            // String data follows directly after the header
            let string_data_ptr = self
                .all_memory
                .add(string_header_addr as usize + STRING_PAYLOAD_OFFSET.0 as usize);
            let string_byte_length = string_header.byte_count as usize;

            debug_assert!(
                string_header_addr as usize + size_of::<StringHeader>() + string_byte_length
                    <= self.all_memory_len,
                "String read out-of-bounds in memory"
            );

            let bytes = slice::from_raw_parts(string_data_ptr, string_byte_length);

            std::str::from_utf8_unchecked(bytes)
        }
    }
}

pub trait HostFunctionCallback {
    fn dispatch_host_call(&mut self, args: HostArgs);
}
