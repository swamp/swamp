use crate::frame::FrameMemory;
use crate::{ALIGNMENT, ALIGNMENT_MASK, ALIGNMENT_REST, Vm};
use std::{alloc, ptr, slice};

pub struct HeapMemory {
    pub(crate) heap_memory: *mut u8,
    pub(crate) heap_memory_size: usize,

    // Memory regions (offsets)
    pub(crate) heap_alloc_offset: usize, // Current allocation point
    pub(crate) constant_memory_size: usize,
}

impl HeapMemory {
    pub(crate) fn read_debug_slice(&self, start_offset: u32, size: u16) -> Vec<u8> {
        let slice = unsafe {
            slice::from_raw_parts(
                self.get_heap_const_ptr(start_offset as usize),
                size as usize,
            )
        };

        slice.to_vec()
    }
}

impl Drop for HeapMemory {
    fn drop(&mut self) {
        unsafe {
            // Free the memory that was allocated in new()
            let layout = alloc::Layout::from_size_align(self.heap_memory_size, ALIGNMENT).unwrap();
            alloc::dealloc(self.heap_memory, layout);
        }
    }
}

impl HeapMemory {
    pub fn new(heap_memory_size: usize, constant_memory: &[u8]) -> Self {
        let heap_memory = unsafe {
            alloc::alloc(alloc::Layout::from_size_align(heap_memory_size, ALIGNMENT).unwrap())
        };
        unsafe {
            ptr::write_bytes(heap_memory, 0, heap_memory_size);
            ptr::copy_nonoverlapping(constant_memory.as_ptr(), heap_memory, constant_memory.len());
        }
        Self {
            heap_memory,
            heap_memory_size,
            heap_alloc_offset: constant_memory.len(),
            constant_memory_size: constant_memory.len(),
        }
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
            offset < self.heap_memory_size,
            "out of bounds for heap. requested {offset} out of {}",
            self.heap_memory_size,
        );
        unsafe { self.heap_memory.add(offset) }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_const_ptr(&self, offset: usize) -> *const u8 {
        debug_assert!(
            offset < self.heap_memory_size,
            "out of bounds for heap {offset:X}"
        );
        unsafe { self.heap_memory.add(offset) }
    }

    #[inline]
    pub(crate) fn heap_allocate(&mut self, size: usize) -> u32 {
        if size == 0 {
            eprintln!("heap_allocate zero");
            return 0;
        }
        let aligned_size = (size + ALIGNMENT_REST) & ALIGNMENT_MASK;
        let aligned_offset = (self.heap_alloc_offset + ALIGNMENT_REST) & ALIGNMENT_MASK;

        eprintln!(
            "heap_allocate original_size:{size}, aligned_size: {aligned_size} offset: {aligned_offset:08X} ({aligned_offset}) max_heap_size: {}",
            self.heap_memory_size
        );

        debug_assert!(
            aligned_offset + aligned_size <= self.heap_memory_size,
            "Out of memory"
        );

        self.heap_alloc_offset = aligned_offset + aligned_size;

        aligned_offset as u32
    }

    #[inline]
    pub(crate) fn heap_allocate_with_data(&mut self, octets: &[u8]) -> u32 {
        let offset = self.heap_allocate(octets.len());
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
}
