use crate::{ALIGNMENT, ALIGNMENT_MASK, ALIGNMENT_REST};
use std::{alloc, mem, ptr, slice};
use swamp_vm_types::aligner::align;

pub struct Memory {
    pub(crate) memory: *mut u8,
    pub(crate) memory_size: usize,
    pub(crate) stack_offset: usize, // Current stack position
    pub(crate) frame_offset: usize, // Current frame position
    pub stack_start: usize,
    pub heap_alloc_offset: usize,
    pub constant_memory_size: usize,
}

impl Drop for Memory {
    fn drop(&mut self) {
        unsafe {
            // Free the memory that was allocated in new()
            let layout = alloc::Layout::from_size_align(self.memory_size, ALIGNMENT).unwrap();
            alloc::dealloc(self.memory, layout);
        }
    }
}

impl Memory {
    pub fn new(memory_size: usize, constant_memory: &[u8]) -> Self {
        let memory = unsafe {
            alloc::alloc(alloc::Layout::from_size_align(memory_size, ALIGNMENT).unwrap())
        };
        unsafe {
            ptr::write_bytes(memory, 0, memory_size);
            ptr::copy_nonoverlapping(constant_memory.as_ptr(), memory, constant_memory.len());
        }

        let aligned_start_of_stack = align(constant_memory.len(), ALIGNMENT);

        let aligned_start_of_heap = align(memory_size * 3 / 4, ALIGNMENT);

        eprintln!("START: heap_start: {aligned_start_of_heap:X} stack: {aligned_start_of_stack:X}");

        Self {
            memory,
            memory_size,
            stack_offset: aligned_start_of_stack,
            frame_offset: aligned_start_of_stack,
            heap_alloc_offset: aligned_start_of_heap,
            constant_memory_size: aligned_start_of_heap,
            stack_start: aligned_start_of_stack,
        }
    }

    pub fn reset_offset(&mut self) {
        self.frame_offset = self.stack_start;
    }

    pub fn reset(&mut self) {
        self.stack_offset = self.stack_start;
        self.frame_offset = self.stack_offset;
    }

    pub fn reset_allocator(&mut self) {
        self.heap_alloc_offset = self.constant_memory_size;
    }

    pub(crate) fn protect_up_to_allocator(&mut self) {
        self.constant_memory_size = self.heap_alloc_offset;
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

    #[inline(always)]
    #[must_use]
    pub fn get_heap_const_ptr(&self, offset: usize) -> *const u8 {
        debug_assert!(
            offset < self.memory_size,
            "out of bounds for heap {offset:X}"
        );
        unsafe { self.memory.add(offset) }
    }

    pub unsafe fn get_heap_offset(&self, ptr: *const u8) -> u32 {
        // Assuming ptr is guaranteed to be within bounds or this
        // will cause a panic if subtraction results in overflow (ptr < heap_base)
        // or if cast to u32 overflows (for extremely large heaps on 64-bit)
        (ptr as usize - self.memory as usize) as u32
    }

    #[inline]
    pub(crate) fn heap_allocate(&mut self, size: usize) -> u32 {
        todo!()
    }
    pub(crate) fn heap_allocate_secret(&mut self, size: usize) -> u32 {
        if size == 0 {
            eprintln!("heap_allocate zero");
            return 0;
        }
        let aligned_size = (size + ALIGNMENT_REST) & ALIGNMENT_MASK;
        let aligned_offset = (self.heap_alloc_offset + ALIGNMENT_REST) & ALIGNMENT_MASK;

        /*
        eprintln!(
            "heap_allocate original_size:{size}, aligned_size: {aligned_size} offset: {aligned_offset:08X} ({aligned_offset}) max_heap_size: {}",
            self.memory_size
        );

         */

        debug_assert!(
            aligned_offset + aligned_size <= self.memory_size,
            "Out of memory"
        );

        self.heap_alloc_offset = aligned_offset + aligned_size;

        aligned_offset as u32
    }

    pub(crate) fn heap_allocate_with_data(&mut self, octets: &[u8]) -> u32 {
        let offset = self.heap_allocate_secret(octets.len());
        debug_assert_ne!(offset, 0);
        {
            unsafe {
                ptr::copy_nonoverlapping(
                    octets.as_ptr(),
                    self.get_heap_ptr(offset as usize),
                    octets.len(),
                );
            }
        }
        offset
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
    pub const fn set_fp(&mut self) {
        self.frame_offset = self.stack_offset;
    }

    #[inline]
    pub(crate) const fn pop(&mut self, previous_frame_offset: usize, previous_stack_offset: usize) {
        self.frame_offset = previous_frame_offset;
        self.stack_offset = previous_stack_offset;
    }

    #[must_use]
    pub fn frame_ptr(&self) -> *mut u8 {
        self.get_frame_ptr(0)
    }

    #[must_use]
    pub fn stack_ptr(&self) -> *mut u8 {
        self.get_frame_ptr(0)
    }

    #[inline(always)]
    pub fn get_frame_ptr_as_u32(&self, addressing: u16) -> *mut u32 {
        debug_assert!(
            (self.frame_offset + addressing as usize) < self.memory_size,
            "out of stack space frame base:{} offset:{addressing} total: {}",
            self.frame_offset,
            self.memory_size,
        );
        debug_assert_eq!(
            addressing % 4,
            0,
            "Unaligned i32 access at offset {addressing}"
        );

        unsafe { self.get_memory_ptr(addressing) as *mut u32 }
    }

    #[inline(always)]
    pub fn get_frame_ptr_as_u16(&self, addressing: u16) -> *mut u16 {
        debug_assert!(
            (self.frame_offset + addressing as usize) < self.memory_size,
            "wrong frame addr"
        );
        // Ensure alignment
        debug_assert_eq!(
            addressing % 2,
            0,
            "Unaligned u16 access at offset {addressing}",
        );

        unsafe { self.get_memory_ptr(addressing) as *mut u16 }
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

    #[inline(always)]
    #[must_use]
    pub fn get_frame_ptr(&self, addressing: u16) -> *mut u8 {
        debug_assert!(
            (self.frame_offset + addressing as usize) < self.memory_size,
            "wrong frame addr"
        );

        unsafe { self.get_memory_ptr(addressing) }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr(&self, addressing: u16) -> *const u8 {
        debug_assert!(
            (self.frame_offset + addressing as usize) < self.memory_size,
            "wrong frame addr"
        );
        unsafe { self.get_memory_const_ptr(addressing) }
    }

    #[inline(always)]
    #[must_use]
    pub const fn get_stack_const_ptr(&self, stack_offset: usize) -> *const u8 {
        debug_assert!(stack_offset < self.memory_size, "wrong stack addr");
        unsafe { self.memory.add(stack_offset) }
    }

    pub(crate) fn read_frame_debug_slice(&self, start_offset: u16, size: u16) -> Vec<u8> {
        let slice =
            unsafe { slice::from_raw_parts(self.get_frame_const_ptr(start_offset), size as usize) };

        slice.to_vec()
    }

    pub(crate) fn read_debug_slice(&self, start_offset: u32, size: u16) -> Vec<u8> {
        let slice =
            unsafe { slice::from_raw_parts(self.memory.add(start_offset as usize), size as usize) };

        slice.to_vec()
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_ptr_as_i32(&self, some_addressing: u16) -> *mut i32 {
        // Ensure alignment
        debug_assert_eq!(
            some_addressing % 4,
            0,
            "Unaligned i32 access at offset {some_addressing}"
        );
        // Inline ptr_at functionality

        unsafe { self.get_memory_ptr(some_addressing) as *mut i32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_memory_const_ptr(&self, some_addressing: u16) -> *const u8 {
        if (some_addressing & 0x1000) != 0 {
            self.get_heap_const_ptr((some_addressing & 0x07fff) as usize)
        } else {
            unsafe {
                self.memory
                    .add(self.frame_offset + some_addressing as usize) as *const u8
            }
        }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_memory_ptr(&self, some_addressing: u16) -> *mut u8 {
        if (some_addressing & 0x1000) != 0 {
            debug_assert!(
                ((some_addressing & 0x07fff) as usize) < self.memory_size,
                "wrong frame addr"
            );
            self.get_heap_ptr((some_addressing & 0x07fff) as usize)
        } else {
            debug_assert!(
                (self.frame_offset + some_addressing as usize) < self.memory_size,
                "wrong frame addr"
            );
            unsafe {
                self.memory
                    .add(self.frame_offset + some_addressing as usize) as *mut u8
            }
        }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_i32(&self, addressing: u16) -> *const i32 {
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

        unsafe { self.get_memory_const_ptr(addressing) as *const i32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_u32(&self, offset: u16) -> *const u32 {
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
        unsafe { self.memory.add(self.frame_offset + offset as usize) as *const u32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_u16(&self, addressing: u16) -> *const u16 {
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
        unsafe { self.get_memory_const_ptr(addressing) as *const u16 }
    }

    #[must_use]
    pub fn read_frame_i32(&self, offset: u16) -> i32 {
        unsafe { *(self.get_frame_const_ptr_as_i32(offset)) }
    }

    #[inline(always)]
    #[must_use]
    pub fn read_frame_u8(&self, offset: u16) -> u8 {
        unsafe { *self.get_frame_const_ptr(offset) }
    }

    #[inline(always)]
    #[must_use]
    pub fn read_frame_bool(&self, offset: u16) -> bool {
        unsafe { *self.get_frame_const_ptr(offset) != 0 }
    }

    #[inline(always)]
    #[must_use]
    pub fn read_frame_u16(&self, offset: u16) -> u16 {
        unsafe { *self.get_frame_const_ptr_as_u16(offset) }
    }

    #[inline(always)]
    #[must_use]
    pub fn read_frame_u32(&self, offset: u16) -> u32 {
        unsafe { *self.get_frame_const_ptr_as_u32(offset) }
    }

    #[must_use]
    #[inline(always)]
    pub fn read_heap_offset_via_frame(&self, frame_offset: u16) -> u32 {
        self.read_frame_u32(frame_offset)
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_ptr_via_frame(&self, frame_offset: u16) -> *mut u8 {
        let heap_offset = self.read_frame_u32(frame_offset);
        self.get_heap_ptr(heap_offset as usize)
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_u32_ptr_via_frame(&self, frame_offset: u16) -> *mut u32 {
        let heap_offset = self.read_frame_u32(frame_offset);
        self.get_heap_ptr(heap_offset as usize) as *mut u32
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_ptr_via_frame_with_offset(
        &self,
        frame_offset: u16,
        heap_ptr_offset: u32,
    ) -> *mut u8 {
        let heap_offset = self.read_heap_offset_via_frame(frame_offset);
        self.get_heap_ptr(heap_offset as usize + heap_ptr_offset as usize)
    }
}
