/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::ALIGNMENT;
use std::{alloc, mem, ptr, slice};
use swamp_vm_types::aligner::{SAFE_ALIGNMENT, align};
use swamp_vm_types::{HeapMemoryAddress, HeapMemoryRegion, MemoryAlignment, MemorySize};

/// Execution mode for the VM memory
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionMode {
    /// Constant evaluation mode - strings should be in constant memory or heap
    ConstantEvaluation,
    /// Normal execution mode - strings should be in heap
    NormalExecution,
}

/// VM Memory Layout (from lower to higher addresses):
///
/// 1. **Constant Memory**: Pre-compiled constant data (read-only)
/// 2. **Constant Heap Allocations**: Strings and other heap data allocated during constant evaluation
///    - These allocations must be preserved between function calls as they contain constant string data
///    - String pointers in constant memory reference this area
/// 3. **Preserved Structs**: Data structures that need to persist between engine ticks
///    - Currently unused but reserved for future use
/// 4. **Stack Space**: Frame-placed variables and function call frames
///    - Grows upward with each function call
///    - Reset on function entry/exit
/// 5. **Heap**: Dynamic allocations during program execution
///    - Reset after each tick to prevent memory leaks
///    - Starts after the preserved area to avoid corrupting constant data
pub struct Memory {
    pub(crate) memory: *mut u8,
    pub(crate) memory_size: usize,
    pub stack_memory_size: usize,
    pub stack_offset: usize,        // Current stack position
    pub(crate) frame_offset: usize, // Current frame position
    pub stack_start: usize,
    pub heap_start: usize,
    pub heap_alloc_offset: usize,
    pub constant_memory_size: usize,
    pub execution_mode: ExecutionMode, // Track whether we're in constant evaluation or normal execution
}

impl Memory {}

impl Memory {}

impl Memory {}

impl Drop for Memory {
    fn drop(&mut self) {
        unsafe {
            alloc::dealloc(
                self.memory,
                alloc::Layout::from_size_align(self.memory_size, ALIGNMENT).unwrap(),
            );
        }
    }
}

impl Memory {
    pub fn new(constant_memory: &[u8], stack_memory_size: usize, heap_memory_size: usize) -> Self {
        let total_memory_size = constant_memory.len() + stack_memory_size + heap_memory_size;
        let memory = unsafe {
            alloc::alloc(alloc::Layout::from_size_align(total_memory_size, ALIGNMENT).unwrap())
        };
        unsafe {
            ptr::write_bytes(memory, 0, total_memory_size);
            ptr::copy_nonoverlapping(constant_memory.as_ptr(), memory, constant_memory.len());
        }

        let aligned_start_of_stack = align(constant_memory.len(), ALIGNMENT);

        let aligned_start_of_heap = align(aligned_start_of_stack + stack_memory_size, ALIGNMENT);

        assert!(aligned_start_of_heap > aligned_start_of_stack + 128 * 1024);

        Self {
            memory,
            stack_memory_size,
            memory_size: total_memory_size,
            stack_offset: aligned_start_of_stack,
            heap_start: aligned_start_of_heap,
            frame_offset: aligned_start_of_stack,
            heap_alloc_offset: aligned_start_of_heap,
            constant_memory_size: aligned_start_of_stack,
            stack_start: aligned_start_of_stack,
            execution_mode: ExecutionMode::NormalExecution,
        }
    }

    pub fn reset_offset(&mut self) {
        self.frame_offset = self.stack_start;
    }

    pub fn reset(&mut self) {
        assert!(self.stack_offset >= self.constant_memory_size);
        self.stack_offset = self.stack_start;
        self.frame_offset = self.stack_offset;
    }

    pub fn reset_allocator(&mut self) {
        self.heap_alloc_offset = self.heap_start;
    }

    pub fn reset_stack_and_fp(&mut self) {
        self.stack_offset = self.stack_start;
        self.frame_offset = self.stack_offset;
    }

    pub fn set_heap_directly_after_constant_area(&mut self) {
        // Set the heap_start to be just after the constant memory area (aligned)
        let original_constant_memory_size = self.constant_memory_size;
        let aligned_heap_start = align(original_constant_memory_size, ALIGNMENT);
        self.heap_start = aligned_heap_start;
        self.heap_alloc_offset = aligned_heap_start;

        // When setting heap directly after constant area, we're in constant evaluation mode
        self.execution_mode = ExecutionMode::ConstantEvaluation;
    }

    pub fn incorporate_heap_into_constant_area(&mut self) {
        let constant_heap_end = self.heap_alloc_offset;

        //        eprintln!("=== INCORPORATE_HEAP_INTO_CONSTANT_AREA ===");
        //      eprintln!("Before: constant_memory_size=0x{:X}, heap_alloc_offset=0x{:X}",
        //              self.constant_memory_size, self.heap_alloc_offset);

        // The original code to incorporate heap into constant area
        let new_stack_start = align(constant_heap_end, ALIGNMENT);

        self.stack_start = new_stack_start;
        self.stack_offset = new_stack_start;
        self.frame_offset = new_stack_start;
        self.constant_memory_size = new_stack_start;

        let new_heap_start = align(new_stack_start + self.stack_memory_size, ALIGNMENT);
        self.heap_start = new_heap_start;
        self.heap_alloc_offset = new_heap_start;

        // After incorporating heap into constant area, we're switching to normal execution mode
        self.execution_mode = ExecutionMode::NormalExecution;
        //        eprintln!("After: constant_memory_size=0x{:X}, new_stack_start=0x{:X}, new_heap_start=0x{:X}",
        //                self.constant_memory_size, new_stack_start, new_heap_start);
    }

    pub fn alloc_before_stack(
        &mut self,
        size: &MemorySize,
        alignment: &MemoryAlignment,
    ) -> HeapMemoryRegion {
        let start = align(self.stack_start, SAFE_ALIGNMENT);
        let end = start + size.0 as usize;
        let new_start = align(end, SAFE_ALIGNMENT);

        unsafe {
            ptr::write_bytes(self.get_heap_ptr(start), 0, size.0 as usize);
        }

        self.stack_start = new_start;

        HeapMemoryRegion {
            addr: HeapMemoryAddress(start as u32),
            size: *size,
        }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_ptr(&self, offset: usize) -> *mut u8 {
        debug_assert!(
            offset < self.memory_size,
            "out of bounds for heap. requested {offset} out of {}",
            self.memory_size,
        );
        unsafe { self.memory.add(offset) }
    }

    pub fn get_heap_const_ptr(&self, offset: usize) -> *const u8 {
        debug_assert!(
            offset < self.memory_size,
            "Memory access out of bounds: offset 0x{:X} >= memory_size 0x{:X}",
            offset,
            self.memory_size
        );

        unsafe { self.memory.add(offset) }
    }

    pub unsafe fn get_heap_offset(&self, ptr: *const u8) -> u32 {
        // Assuming ptr is guaranteed to be within bounds or this
        // will cause a panic if subtraction results in overflow (ptr < heap_base)
        // or if cast to u32 overflows (for extremely large heaps on 64-bit)
        (ptr as usize - self.memory as usize) as u32
    }

    pub fn heap_allocate_secret(&mut self, size: usize) -> u32 {
        let aligned_size = align(size, ALIGNMENT);
        let aligned_offset = self.heap_alloc_offset + aligned_size;

        debug_assert!(
            aligned_offset <= self.memory_size,
            "out of heap memory {aligned_offset:X} > {}",
            self.memory_size
        );

        let result_offset = self.heap_alloc_offset;
        self.heap_alloc_offset = aligned_offset;

        result_offset as u32
    }

    pub fn heap_allocate_with_data(&mut self, data: &[u8]) -> u32 {
        let aligned_size = align(data.len(), ALIGNMENT);
        let aligned_offset = self.heap_alloc_offset + aligned_size;

        debug_assert!(
            aligned_offset <= self.memory_size,
            "out of heap memory {aligned_offset:X} > {}",
            self.memory_size
        );

        let result_offset = self.heap_alloc_offset;

        unsafe {
            let dest_ptr = self.memory.add(result_offset);
            ptr::copy_nonoverlapping(data.as_ptr(), dest_ptr, data.len());
        }

        self.heap_alloc_offset = aligned_offset;

        result_offset as u32
    }

    pub fn frame_offset(&self) -> usize {
        self.frame_offset
    }

    /// Usually called on `Enter`
    /// reserving space for local variables and arguments
    /// it is a bit of a hack, but the current return values and arguments are not part of the stack
    /// (it should be in theory), but that is to slightly increase performance to not having to update
    /// SP to reflect the "pushed" return space and arguments.
    #[inline(always)]
    pub(crate) const fn inc_sp(&mut self, aligned_size: usize) {
        self.stack_offset += aligned_size;
    }

    /// Usually called on a call
    /// It sets the FP to the current SP. The stack pointer includes the current function frame size
    /// but doesn't include return values and arguments.
    #[inline(always)]
    pub const fn set_fp_from_sp(&mut self) {
        self.frame_offset = self.stack_offset;
    }

    pub(crate) fn set_stack_and_frame(&mut self, addr: usize) {
        assert!(
            addr > self.constant_memory_size,
            "must be greater than the constant area"
        );
        assert!(addr > self.stack_start);
        self.frame_offset = addr;
        self.stack_offset = addr;
    }

    #[inline]
    pub(crate) const fn pop(&mut self, previous_frame_offset: usize, previous_stack_offset: usize) {
        self.frame_offset = previous_frame_offset;
        self.stack_offset = previous_stack_offset;
    }

    pub(crate) fn read_debug_stack_slice(&self, start_offset: u32, size: u16) -> Vec<u8> {
        let slice = unsafe {
            slice::from_raw_parts(
                self.get_stack_const_ptr(start_offset as usize),
                size as usize,
            )
        };

        slice.to_vec()
    }

    // ---------------- FP relative ----------------------------------
    #[must_use]
    pub fn frame_ptr(&self) -> *mut u8 {
        self.get_frame_ptr(0)
    }

    #[inline(always)]
    pub fn get_frame_ptr_as_u32(&self, offset: u32) -> *mut u32 {
        debug_assert!(
            (self.frame_offset + offset as usize) < self.memory_size,
            "out of stack space frame base:{} offset:{offset} total: {}",
            self.frame_offset,
            self.memory_size,
        );
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {offset}");

        unsafe { self.get_heap_ptr(offset as usize + self.frame_offset) as *mut u32 }
    }

    #[inline(always)]
    pub fn get_frame_ptr_as_u16(&self, offset: u32) -> *mut u16 {
        debug_assert!(
            (self.frame_offset + offset as usize) < self.memory_size,
            "wrong frame addr"
        );
        // Ensure alignment
        debug_assert_eq!(
            (self.frame_offset + offset as usize) % 2,
            0,
            "Unaligned u16 access at offset {offset}",
        );

        unsafe { self.get_frame_ptr(offset) as *mut u16 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_ptr(&self, fp_offset: u32) -> *mut u8 {
        debug_assert!(
            (self.frame_offset + fp_offset as usize) < self.memory_size,
            "wrong frame addr"
        );

        unsafe { self.get_heap_ptr(fp_offset as usize + self.frame_offset) }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr(&self, fp_offset: u32) -> *mut u8 {
        debug_assert!(
            (self.frame_offset + fp_offset as usize) < self.memory_size,
            "wrong frame addr"
        );

        unsafe { self.get_heap_ptr(fp_offset as usize + self.frame_offset) }
    }
    pub(crate) fn read_frame_debug_slice(&self, start_offset: u32, size: u16) -> Vec<u8> {
        let slice =
            unsafe { slice::from_raw_parts(self.get_frame_const_ptr(start_offset), size as usize) };

        slice.to_vec()
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_ptr_as_i32(&self, some_addressing: u32) -> *mut i32 {
        // Ensure alignment
        debug_assert_eq!(
            some_addressing % 4,
            0,
            "Unaligned i32 access at offset {some_addressing}"
        );
        // Inline ptr_at functionality

        unsafe { self.get_frame_ptr(some_addressing) as *mut i32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_i32(&self, addressing: u32) -> *const i32 {
        // Ensure alignment
        debug_assert_eq!(
            addressing % 4,
            0,
            "Unaligned i32 access at offset {addressing}"
        );
        debug_assert!(
            (self.frame_offset + addressing as usize) < self.memory_size,
            "wrong frame addr"
        );

        unsafe { self.get_frame_const_ptr(addressing) as *const i32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_u32(&self, offset: u32) -> *const u32 {
        let absolute_offset = self.frame_offset + offset as usize;
        debug_assert!(
            (self.frame_offset + offset as usize) < self.memory_size,
            "wrong frame addr"
        );

        // Ensure alignment
        debug_assert_eq!(
            absolute_offset % mem::align_of::<u32>(),
            0,
            "Unaligned u32 access at absolute offset {absolute_offset} (frame: {}, offset: {})",
            self.frame_offset,
            offset
        );

        // Inline ptr_at functionality
        unsafe { self.get_frame_const_ptr(offset) as *const u32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_u16(&self, addressing: u32) -> *const u16 {
        let absolute_offset = self.frame_offset + addressing as usize;
        debug_assert!(
            (self.frame_offset + addressing as usize) < self.memory_size,
            "wrong frame addr"
        );

        // Ensure alignment
        debug_assert_eq!(
            absolute_offset % mem::align_of::<u32>(),
            0,
            "Unaligned u32 access at absolute offset {absolute_offset} (frame: {}, offset: {})",
            self.frame_offset,
            addressing
        );

        // Inline ptr_at functionality
        unsafe { self.get_frame_const_ptr(addressing) as *const u16 }
    }

    #[must_use]
    pub fn read_frame_i32(&self, offset: u32) -> i32 {
        unsafe { *(self.get_frame_const_ptr_as_i32(offset)) }
    }

    #[inline(always)]
    #[must_use]
    pub fn read_frame_u8(&self, offset: u32) -> u8 {
        unsafe { *self.get_frame_const_ptr(offset) }
    }

    #[inline(always)]
    #[must_use]
    pub fn read_frame_bool(&self, offset: u32) -> bool {
        unsafe { *self.get_frame_const_ptr(offset) != 0 }
    }

    #[inline(always)]
    #[must_use]
    pub fn read_frame_u16(&self, offset: u32) -> u16 {
        unsafe { *self.get_frame_const_ptr_as_u16(offset) }
    }

    #[inline(always)]
    #[must_use]
    pub fn read_frame_u32(&self, offset: u32) -> u32 {
        unsafe { *self.get_frame_const_ptr_as_u32(offset) }
    }

    // ---------- Stack ---------------
    #[inline(always)]
    #[must_use]
    pub const fn get_stack_const_ptr(&self, stack_offset: usize) -> *const u8 {
        debug_assert!(stack_offset < self.memory_size, "wrong stack addr");
        unsafe { self.memory.add(stack_offset) }
    }

    pub(crate) fn read_debug_slice(&self, start_offset: u32, size: u16) -> Vec<u8> {
        let slice =
            unsafe { slice::from_raw_parts(self.memory.add(start_offset as usize), size as usize) };

        slice.to_vec()
    }

    #[must_use]
    #[inline(always)]
    pub fn read_heap_offset_via_frame(&self, frame_offset: u32) -> u32 {
        self.read_frame_u32(frame_offset)
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_ptr_via_frame(&self, frame_offset: u32) -> *mut u8 {
        let heap_offset = self.read_frame_u32(frame_offset);
        self.get_heap_ptr(heap_offset as usize)
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_u32_ptr_via_frame(&self, frame_offset: u32) -> *mut u32 {
        let heap_offset = self.read_frame_u32(frame_offset);
        self.get_heap_ptr(heap_offset as usize) as *mut u32
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_ptr_via_frame_with_offset(
        &self,
        frame_offset: u32,
        heap_ptr_offset: u32,
    ) -> *mut u8 {
        let heap_offset = self.read_heap_offset_via_frame(frame_offset);
        self.get_heap_ptr(heap_offset as usize + heap_ptr_offset as usize)
    }
}
