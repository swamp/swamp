use crate::{ALIGNMENT_MASK, ALIGNMENT_REST, Vm};
use std::ptr;

impl Vm {
    #[inline(always)]
    #[must_use]
    pub const fn heap_ptr_at(&self, offset: usize) -> *mut u8 {
        unsafe { self.heap_memory.add(offset) }
    }

    #[inline(always)]
    #[must_use]
    pub const fn heap_ptr_immut_at(&self, offset: usize) -> *const u8 {
        unsafe { self.heap_memory.add(offset) }
    }

    #[inline]
    pub(crate) fn heap_allocate(&mut self, size: usize) -> u32 {
        let aligned_size = (size + ALIGNMENT_REST) & ALIGNMENT_MASK;
        let aligned_offset = (self.heap_alloc_offset + ALIGNMENT_REST) & ALIGNMENT_MASK;

        eprintln!(
            "heap_allocate original_size:{size}, aligned_size: {aligned_size} offset: {aligned_offset:08X} ({aligned_offset})"
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
        {
            unsafe {
                ptr::copy_nonoverlapping(
                    octets.as_ptr(),
                    self.heap_ptr_at(offset as usize),
                    octets.len(),
                );
            }
        }
        offset
    }
}
