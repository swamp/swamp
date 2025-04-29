use crate::Vm;

impl Vm {
    #[inline(always)]
    pub fn ptr_at_i32(&self, offset: usize) -> *mut i32 {
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(offset) as *mut i32 }
    }

    #[inline(always)]
    pub fn ptr_at_u32(&self, offset: usize) -> *mut u32 {
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(offset) as *mut u32 }
    }

    #[inline(always)]
    pub fn ptr_at_u16(&self, offset: usize) -> *mut u16 {
        // Ensure alignment
        debug_assert_eq!(offset % 2, 0, "Unaligned u16 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(offset) as *mut u16 }
    }

    #[inline(always)]
    pub fn ptr_at_u8(&self, offset: usize) -> *mut u8 {
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(offset) }
    }

    #[inline(always)]
    pub fn heap_ptr_at(&self, offset: usize) -> *mut u8 {
        unsafe { self.heap_memory.add(offset) }
    }

    pub fn heap_ptr_immut_at(&self, offset: usize) -> *const u8 {
        unsafe { self.heap_memory.add(offset) }
    }

    // Helper to get current frame pointer
    pub fn frame_ptr(&self) -> *mut u8 {
        self.ptr_at_u8(self.frame_offset)
    }

    pub fn stack_ptr(&self) -> *mut u8 {
        self.ptr_at_u8(self.stack_offset)
    }

    #[inline(always)]
    pub fn frame_ptr_i32_at(&self, offset: u16) -> *mut i32 {
        self.ptr_at_i32(self.frame_offset + offset as usize)
    }

    #[inline(always)]
    pub fn frame_ptr_i32_const_at(&self, offset: u16) -> *const i32 {
        self.ptr_at_i32(self.frame_offset + offset as usize)
            .cast_const()
    }

    #[inline(always)]
    pub fn frame_u8_at(&self, offset: u16) -> u8 {
        unsafe { *self.ptr_at_u8(self.frame_offset + offset as usize) }
    }

    #[inline(always)]
    pub fn frame_u32_at(&self, offset: u16) -> u32 {
        unsafe { *self.ptr_at_u32(self.frame_offset + offset as usize) }
    }

    #[inline(always)]
    pub fn frame_ptr_bool_at(&self, offset: u16) -> *mut u8 {
        self.ptr_at_u8(self.frame_offset + offset as usize)
    }

    #[inline(always)]
    pub fn frame_ptr_at(&self, offset: u16) -> *mut u8 {
        self.ptr_at_u8(self.frame_offset + offset as usize)
    }

    #[inline(always)]
    pub fn frame_ptr_indirect_heap_immut_at(&self, frame_offset: u16) -> *const u8 {
        let heap_offset = self.frame_u32_at(frame_offset);
        self.heap_ptr_immut_at(heap_offset as usize)
    }

    #[inline(always)]
    pub fn frame_ptr_indirect_heap_at(&self, frame_offset: u16) -> *mut u8 {
        let heap_offset = self.frame_u32_at(frame_offset);
        self.heap_ptr_at(heap_offset as usize)
    }

    #[inline(always)]
    pub fn frame_ptr_indirect_heap_mut_at(&self, frame_offset: u16) -> *mut u32 {
        let heap_offset = self.frame_u32_at(frame_offset);
        self.heap_ptr_at(heap_offset as usize) as *mut u32
    }

    #[inline(always)]
    pub fn frame_ptr_indirect_heap_at_with_offset(
        &self,
        frame_offset: u16,
        heap_ptr_offset: u32,
    ) -> *mut u8 {
        let heap_offset = self.frame_ptr_indirect_heap_offset_at(frame_offset);
        self.heap_ptr_at(heap_offset as usize + heap_ptr_offset as usize)
    }

    pub fn frame_ptr_indirect_heap_offset_at(&self, frame_offset: u16) -> u32 {
        self.frame_u32_at(frame_offset)
    }

    #[inline(always)]
    pub fn frame_ptr_bool_const_at(&self, offset: u16) -> bool {
        unsafe { *self.ptr_at_u8(self.frame_offset + offset as usize) != 0 }
    }
}
