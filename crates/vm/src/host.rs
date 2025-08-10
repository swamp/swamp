/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::RegContents;
use std::{
    mem::{align_of, size_of},
    ptr, slice,
};
use swamp_vm_isa::{AnyHeader, VecHeader, VEC_HEADER_MAGIC_CODE, VEC_HEADER_PAYLOAD_OFFSET};

#[derive(Debug)]
pub struct AnyValue {
    pub bytes: Vec<u8>,
    pub type_hash: u32,
}

#[derive(Debug)]
pub struct AnyValueMut {
    pub data_ptr: *mut u8,
    pub size: usize,
    pub type_hash: u32,
}

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
                *(self.all_memory.add(string_header_addr as usize) as *const VecHeader);

            // String data follows directly after the header
            let string_data_ptr = self
                .all_memory
                .add(string_header_addr as usize + VEC_HEADER_PAYLOAD_OFFSET.0 as usize);
            let string_byte_length = string_header.element_count as usize;

            debug_assert!(
                string_header_addr as usize + size_of::<VecHeader>() + string_byte_length
                    <= self.all_memory_len,
                "String read out-of-bounds in memory"
            );

            let bytes = slice::from_raw_parts(string_data_ptr, string_byte_length);

            std::str::from_utf8_unchecked(bytes)
        }
    }

    pub fn any(&self, register_id: u8) -> AnyValue {
        let any_header = self.get::<AnyHeader>(register_id);

        unsafe {
            let any_data_ptr = self.all_memory.add(any_header.data_ptr as usize);

            let data_slice = slice::from_raw_parts(any_data_ptr, any_header.size as usize);

            AnyValue {
                bytes: data_slice.to_vec(),
                type_hash: any_header.type_hash,
            }
        }
    }

    pub fn any_mut(&self, register_id: u8) -> AnyValueMut {
        let any_header = self.get::<AnyHeader>(register_id);
        let any_data_ptr = unsafe { self.all_memory.add(any_header.data_ptr as usize) };
        AnyValueMut {
            data_ptr: any_data_ptr,
            size: any_header.size as usize,
            type_hash: any_header.type_hash,
        }
    }

    /// Write multiple elements to the end of a vector (bulk append)
    ///
    /// # Arguments
    /// * `vec_register` - Register containing the vector header address, usually r0
    /// * `data_slice` - Slice of data to append to the vector
    ///
    /// # Panics
    /// * If the vector header is invalid or corrupted
    /// * If there's insufficient capacity for all elements
    /// * If the element size doesn't match the vector's element size
    pub fn write_to_vector_bulk<T>(&mut self, vec_register: u8, data_slice: &[T])
    where
        T: Copy,
    {
        // Get the vector header address and read the header
        let vec_header_addr = self.register(vec_register);
        let vec_header_ptr =
            unsafe { self.all_memory.add(vec_header_addr as usize) as *mut VecHeader };

        let (element_count, capacity, element_size) = unsafe {
            let header = &*vec_header_ptr;

            // Validate the vector header
            if header.padding != VEC_HEADER_MAGIC_CODE {
                panic!("Invalid vector header - memory corruption detected");
            }

            if header.capacity == 0 {
                panic!("Vector was never initialized");
            }

            (header.element_count, header.capacity, header.element_size)
        };

        // Check if there's enough capacity
        let required_capacity = element_count as usize + data_slice.len();
        if required_capacity > capacity as usize {
            panic!("Not enough capacity: need {required_capacity}, have {capacity}");
        }

        // Verify element size matches
        if size_of::<T>() != element_size as usize {
            panic!(
                "Element size mismatch: expected {}, got {}",
                element_size,
                size_of::<T>()
            );
        }

        // Calculate the starting address for the new elements
        let start_addr =
            vec_header_addr + VEC_HEADER_PAYLOAD_OFFSET.0 + (element_count as u32) * element_size;

        // Write all elements
        unsafe {
            let dest_ptr = self.all_memory.add(start_addr as usize) as *mut T;
            ptr::copy_nonoverlapping(data_slice.as_ptr(), dest_ptr, data_slice.len());

            // Update the element count
            (*vec_header_ptr).element_count += data_slice.len() as u16;
        }
    }

    /// Write a single element to a specific index in a vector
    ///
    /// # Arguments
    /// * `vec_register` - Register containing the vector header address, usually r0
    /// * `index` - Index where to write the element (must be `< element_count`)
    /// * `data` - Data to write at the specified index
    ///
    /// # Panics
    /// * If the vector header is invalid or corrupted
    /// * If the index is out of bounds
    /// * If the element size doesn't match the vector's element size
    pub fn write_to_vector_at_index<T>(&mut self, vec_register: u8, index: u16, data: &T)
    where
        T: Copy,
    {
        // Get the vector header address and read the header
        let vec_header_addr = self.register(vec_register);
        let vec_header =
            unsafe { *(self.all_memory.add(vec_header_addr as usize) as *const VecHeader) };

        // Validate the vector header
        if vec_header.padding != VEC_HEADER_MAGIC_CODE {
            panic!("Invalid vector header - memory corruption detected");
        }

        if vec_header.capacity == 0 {
            panic!("Vector was never initialized");
        }

        // Bounds check
        if index >= vec_header.element_count {
            panic!(
                "Index {} out of bounds for vector of length {}",
                index, vec_header.element_count
            );
        }

        // Verify element size matches
        if size_of::<T>() != vec_header.element_size as usize {
            panic!(
                "Element size mismatch: expected {}, got {}",
                vec_header.element_size,
                size_of::<T>()
            );
        }

        // Calculate the address of the element to write
        let element_addr = vec_header_addr
            + VEC_HEADER_PAYLOAD_OFFSET.0
            + (index as u32) * vec_header.element_size;

        // Write the data
        unsafe {
            let element_ptr = self.all_memory.add(element_addr as usize) as *mut T;
            ptr::write(element_ptr, *data);
        }
    }

    /// Append a single element to the end of a vector
    ///
    /// # Arguments
    /// * `vec_register` - Register containing the vector header address, usually r0
    /// * `data` - Data to append to the vector
    ///
    /// # Panics
    /// * If the vector header is invalid or corrupted
    /// * If there's insufficient capacity for the new element
    /// * If the element size doesn't match the vector's element size
    pub fn push_to_vector<T>(&mut self, vec_register: u8, data: &T)
    where
        T: Copy,
    {
        self.write_to_vector_bulk(vec_register, slice::from_ref(data));
    }

    /// Read an element from a vector at a specific index
    ///
    /// # Arguments
    /// * `vec_register` - Register containing the vector header address, usually r0
    /// * `index` - Index of the element to read
    ///
    /// # Returns
    /// A reference to the element at the specified index
    ///
    /// # Panics
    /// * If the vector header is invalid or corrupted
    /// * If the index is out of bounds
    /// * If the element size doesn't match the requested type size
    pub fn read_from_vector_at_index<T>(&self, vec_register: u8, index: u16) -> &T
    where
        T: Copy,
    {
        // Get the vector header
        let vec_header = self.get::<VecHeader>(vec_register);

        // Validate the vector header
        if vec_header.padding != VEC_HEADER_MAGIC_CODE {
            panic!("Invalid vector header - memory corruption detected");
        }

        if vec_header.capacity == 0 {
            panic!("Vector was never initialized");
        }

        // Bounds check
        if index >= vec_header.element_count {
            panic!(
                "Index {} out of bounds for vector of length {}",
                index, vec_header.element_count
            );
        }

        // Verify element size matches
        if size_of::<T>() != vec_header.element_size as usize {
            panic!(
                "Element size mismatch: expected {}, got {}",
                vec_header.element_size,
                size_of::<T>()
            );
        }

        // Calculate the address of the element to read
        let vec_header_addr = self.register(vec_register);
        let element_addr = vec_header_addr
            + VEC_HEADER_PAYLOAD_OFFSET.0
            + (index as u32) * vec_header.element_size;

        // Read the data
        unsafe {
            let element_ptr = self.all_memory.add(element_addr as usize) as *const T;
            &*element_ptr
        }
    }

    /// Get the current length (element count) of a vector
    ///
    /// # Arguments
    /// * `vec_register` - Register containing the vector header address
    ///
    /// # Returns
    /// The number of elements currently in the vector
    ///
    /// # Panics
    /// * If the vector header is invalid or corrupted
    pub fn vector_len(&self, vec_register: u8) -> u16 {
        let vec_header = self.get::<VecHeader>(vec_register);

        // Validate the vector header
        if vec_header.padding != VEC_HEADER_MAGIC_CODE {
            panic!("Invalid vector header - memory corruption detected");
        }

        if vec_header.capacity == 0 {
            panic!("Vector was never initialized");
        }

        vec_header.element_count
    }

    /// Get the capacity of a vector
    ///
    /// # Arguments
    /// * `vec_register` - Register containing the vector header address
    ///
    /// # Returns
    /// The maximum number of elements the vector can hold
    ///
    /// # Panics
    /// * If the vector header is invalid or corrupted
    pub fn vector_capacity(&self, vec_register: u8) -> u16 {
        let vec_header = self.get::<VecHeader>(vec_register);

        // Validate the vector header
        if vec_header.padding != VEC_HEADER_MAGIC_CODE {
            panic!("Invalid vector header - memory corruption detected");
        }

        if vec_header.capacity == 0 {
            panic!("Vector was never initialized");
        }

        vec_header.capacity
    }

    /// Check if a vector is empty
    ///
    /// # Arguments
    /// * `vec_register` - Register containing the vector header address
    ///
    /// # Returns
    /// True if the vector has no elements, false otherwise
    pub fn vector_is_empty(&self, vec_register: u8) -> bool {
        self.vector_len(vec_register) == 0
    }

    /// Check if a vector is full (at capacity)
    ///
    /// # Arguments
    /// * `vec_register` - Register containing the vector header address
    ///
    /// # Returns
    /// True if the vector is at capacity, false otherwise
    pub fn vector_is_full(&self, vec_register: u8) -> bool {
        let vec_header = self.get::<VecHeader>(vec_register);
        vec_header.element_count >= vec_header.capacity
    }
}

pub trait HostFunctionCallback {
    fn dispatch_host_call(&mut self, args: HostArgs);
}
