use crate::Vm;
use swamp_vm_types::{SliceHeader, SlicePairHeader};

impl Vm {
    pub(crate) fn slice_pair_header_from_frame(&self, frame_addr: u16) -> SlicePairHeader {
        unsafe { *(self.get_frame_const_ptr(frame_addr) as *const SlicePairHeader) }
    }

    pub(crate) fn slice_header_from_frame(&self, frame_addr: u16) -> SliceHeader {
        unsafe { *(self.get_frame_const_ptr(frame_addr) as *const SliceHeader) }
    }

    pub(crate) fn get_slice_header_ptr_on_frame(&self, frame_addr: u16) -> *mut SliceHeader {
        self.get_frame_const_ptr(frame_addr) as *mut SliceHeader
    }

    pub(crate) fn get_slice_pair_header_ptr_on_frame(
        &self,
        frame_addr: u16,
    ) -> *mut SlicePairHeader {
        self.get_frame_const_ptr(frame_addr) as *mut SlicePairHeader
    }

    pub fn execute_slice_from_heap(
        &mut self,
        dst_slice_header: u16,
        heap_addr_via_frame: u16,
        element_size: u16,
        element_count: u16,
    ) {
        let dst_ptr = self.get_slice_header_ptr_on_frame(dst_slice_header);
        unsafe {
            (*dst_ptr).element_size = element_size;
            (*dst_ptr).element_count = element_count;
            (*dst_ptr).heap_offset = self.read_heap_offset_via_frame(heap_addr_via_frame);
        }
    }

    pub fn execute_slice_pair_from_heap(
        &mut self,
        dst_slice_header: u16,
        heap_addr_via_frame: u16,
        key_size: u16,
        value_size: u16,
        element_count: u16,
    ) {
        let dst_ptr = self.get_slice_pair_header_ptr_on_frame(dst_slice_header);
        unsafe {
            (*dst_ptr).key_size = key_size;
            (*dst_ptr).value_size = value_size;
            (*dst_ptr).element_count = element_count;
            (*dst_ptr).heap_offset = self.read_heap_offset_via_frame(heap_addr_via_frame);
            debug_assert!((*dst_ptr).heap_offset < self.heap_memory_size as u32);
        }
    }
}
