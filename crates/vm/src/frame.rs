use crate::Vm;
use std::mem;

impl Vm {
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
    pub fn read_frame_u32(&self, offset: u16) -> u32 {
        unsafe { *self.get_frame_const_ptr_as_u32(offset) }
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

    #[must_use]
    #[inline(always)]
    pub fn read_heap_offset_via_frame(&self, frame_offset: u16) -> u32 {
        self.read_frame_u32(frame_offset)
    }
}
