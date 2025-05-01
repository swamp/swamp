use crate::heap::HeapMemory;
use crate::{ALIGNMENT, Vm};
use std::{alloc, mem, ptr};

pub struct FrameMemory {
    pub(crate) stack_memory: *mut u8,
    pub(crate) stack_memory_size: usize,
    pub(crate) stack_offset: usize, // Current stack position
    pub(crate) frame_offset: usize, // Current frame position
}

impl Drop for FrameMemory {
    fn drop(&mut self) {
        unsafe {
            // Free the memory that was allocated in new()
            let layout = alloc::Layout::from_size_align(self.stack_memory_size, ALIGNMENT).unwrap();
            alloc::dealloc(self.stack_memory, layout);
        }
    }
}

impl FrameMemory {
    pub fn new(frame_memory_size: usize) -> Self {
        let frame_memory = unsafe {
            alloc::alloc(alloc::Layout::from_size_align(frame_memory_size, ALIGNMENT).unwrap())
        };
        unsafe {
            ptr::write_bytes(frame_memory, 0, frame_memory_size);
        }

        Self {
            stack_memory: frame_memory,
            stack_memory_size: frame_memory_size,
            stack_offset: 0,
            frame_offset: 0,
        }
    }

    pub fn reset_offset(&mut self) {
        self.frame_offset = 0;
    }

    pub fn reset(&mut self) {
        self.stack_offset = 0;
        self.frame_offset = self.stack_offset;
    }

    #[inline]
    pub(crate) const fn push(&mut self, aligned_size: usize) {
        // the functions frame of reference should be the stack offset
        self.frame_offset = self.stack_offset;

        // and we push the stack with the space of the local variables
        self.stack_offset += aligned_size;
    }

    #[inline]
    pub(crate) const fn pop(&mut self, previous_frame_offset: usize, frame_size_to_pop: usize) {
        // Bring back the frame to the old frame
        self.frame_offset = previous_frame_offset;

        // "pop" the space for the local variables of the stack
        self.stack_offset -= frame_size_to_pop;
    }

    #[must_use]
    pub const fn frame_ptr(&self) -> *mut u8 {
        self.get_frame_ptr(0)
    }

    #[must_use]
    pub const fn stack_ptr(&self) -> *mut u8 {
        self.get_frame_ptr(0)
    }

    #[inline(always)]
    pub fn get_frame_ptr_as_u32(&self, offset: u16) -> *mut u32 {
        debug_assert!(
            (self.frame_offset + offset as usize) < self.stack_memory_size,
            "out of stack space frame base:{} offset:{offset} total: {}",
            self.frame_offset,
            self.stack_memory_size,
        );
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {offset}");
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(self.frame_offset + offset as usize) as *mut u32 }
    }

    #[inline(always)]
    pub fn get_frame_ptr_as_u16(&self, offset: u16) -> *mut u16 {
        debug_assert!(
            (self.frame_offset + offset as usize) < self.stack_memory_size,
            "wrong frame addr"
        );
        // Ensure alignment
        debug_assert_eq!(offset % 2, 0, "Unaligned u16 access at offset {offset}",);
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(self.frame_offset + offset as usize) as *mut u16 }
    }

    #[inline(always)]
    #[must_use]
    pub const fn get_frame_ptr(&self, offset: u16) -> *mut u8 {
        debug_assert!(
            (self.frame_offset + offset as usize) < self.stack_memory_size,
            "wrong frame addr"
        );
        unsafe { self.stack_memory.add(self.frame_offset + offset as usize) }
    }

    #[inline(always)]
    #[must_use]
    pub const fn get_frame_const_ptr(&self, offset: u16) -> *const u8 {
        debug_assert!(
            (self.frame_offset + offset as usize) < self.stack_memory_size,
            "wrong frame addr"
        );
        unsafe { self.stack_memory.add(self.frame_offset + offset as usize) }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_ptr_as_i32(&self, offset: u16) -> *mut i32 {
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {offset}");
        // Inline ptr_at functionality
        debug_assert!(
            (self.frame_offset + offset as usize) < self.stack_memory_size,
            "wrong frame addr"
        );
        unsafe { self.stack_memory.add(self.frame_offset + offset as usize) as *mut i32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_i32(&self, offset: u16) -> *const i32 {
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {offset}");
        debug_assert!(
            (self.frame_offset + offset as usize) < self.stack_memory_size,
            "wrong frame addr"
        );

        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(self.frame_offset + offset as usize) as *const i32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_u32(&self, offset: u16) -> *const u32 {
        let absolute_offset = self.frame_offset + offset as usize;
        debug_assert!(
            (self.frame_offset + offset as usize) < self.stack_memory_size,
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
        unsafe { self.stack_memory.add(self.frame_offset + offset as usize) as *const u32 }
    }

    #[inline(always)]
    #[must_use]
    pub fn get_frame_const_ptr_as_u16(&self, offset: u16) -> *const u16 {
        let absolute_offset = self.frame_offset + offset as usize;
        debug_assert!(
            (self.frame_offset + offset as usize) < self.stack_memory_size,
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
        unsafe { self.stack_memory.add(self.frame_offset + offset as usize) as *const u16 }
    }

    #[must_use]
    pub fn read_frame_i32(&self, offset: u16) -> i32 {
        unsafe { *(self.get_frame_const_ptr_as_i32(offset)) }
    }

    #[inline(always)]
    #[must_use]
    pub const fn read_frame_u8(&self, offset: u16) -> u8 {
        unsafe { *self.get_frame_const_ptr(offset) }
    }

    #[inline(always)]
    #[must_use]
    pub const fn read_frame_bool(&self, offset: u16) -> bool {
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
    pub fn get_heap_ptr_via_frame(&self, frame_offset: u16, heap_memory: &HeapMemory) -> *mut u8 {
        let heap_offset = self.read_frame_u32(frame_offset);
        heap_memory.get_heap_ptr(heap_offset as usize)
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_u32_ptr_via_frame(
        &self,
        frame_offset: u16,
        heap_memory: &HeapMemory,
    ) -> *mut u32 {
        let heap_offset = self.read_frame_u32(frame_offset);
        heap_memory.get_heap_ptr(heap_offset as usize) as *mut u32
    }

    #[inline(always)]
    #[must_use]
    pub fn get_heap_ptr_via_frame_with_offset(
        &self,
        frame_offset: u16,
        heap_ptr_offset: u32,
        heap_memory: &HeapMemory,
    ) -> *mut u8 {
        let heap_offset = self.read_heap_offset_via_frame(frame_offset);
        heap_memory.get_heap_ptr(heap_offset as usize + heap_ptr_offset as usize)
    }
}
