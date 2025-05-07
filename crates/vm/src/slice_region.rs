use crate::u8s_to_u16;
use crate::{Vm, get_reg};
use swamp_vm_types::{SliceHeader, SlicePairHeader};

impl Vm {
    pub(crate) fn slice_pair_header_from_reg(&self, slice_header_reg: u8) -> SlicePairHeader {
        unsafe { *(self.get_const_ptr_from_reg(slice_header_reg) as *const SlicePairHeader) }
    }

    pub(crate) fn slice_header_from_reg(&self, slice_header_reg: u8) -> SliceHeader {
        unsafe { *(self.get_const_ptr_from_reg(slice_header_reg) as *const SliceHeader) }
    }

    pub(crate) fn get_slice_header_ptr_on_frame(&self, slice_header_reg: u8) -> *mut SliceHeader {
        self.get_ptr_from_reg(slice_header_reg) as *mut SliceHeader
    }

    pub(crate) fn get_slice_pair_header_ptr_from_reg(
        &self,
        slice_pair_reg: u8,
    ) -> *mut SlicePairHeader {
        self.get_ptr_from_reg(slice_pair_reg) as *mut SlicePairHeader
    }

    pub fn execute_slice_from_heap(
        &mut self,
        dst_slice_header_reg: u8,
        memory_addr_reg: u8,
        element_count_reg: u8,
        element_size_reg: u8,
    ) {
        let dst_ptr = self.get_slice_header_ptr_on_frame(dst_slice_header_reg);

        let memory_addr = get_reg!(self, memory_addr_reg);
        let element_count = get_reg!(self, element_count_reg);
        let element_size = get_reg!(self, element_size_reg);

        unsafe {
            (*dst_ptr).element_size = element_size as u16;
            (*dst_ptr).element_count = element_count as u16;
            (*dst_ptr).heap_offset = memory_addr;
        }
    }

    pub fn execute_slice_pair_from_heap(
        &mut self,
        dst_slice_header_reg: u8,
        memory_addr_reg: u8,
        key_size_reg: u8,
        value_size_reg: u8,
        element_count_reg: u8,
    ) {
        let dst_ptr = self.get_slice_pair_header_ptr_from_reg(dst_slice_header_reg);

        let memory_addr = get_reg!(self, memory_addr_reg);
        let element_count = get_reg!(self, element_count_reg);
        let key_size = get_reg!(self, key_size_reg);
        let value_size = get_reg!(self, value_size_reg);

        unsafe {
            (*dst_ptr).key_size = key_size as u16;
            (*dst_ptr).value_size = value_size as u16;
            (*dst_ptr).element_count = element_count as u16;
            (*dst_ptr).heap_offset = memory_addr;
            debug_assert!((*dst_ptr).heap_offset < self.memory.memory_size as u32);
        }
    }
}
