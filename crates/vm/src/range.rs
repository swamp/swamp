use crate::Vm;
use std::ptr;
use swamp_vm_types::{RangeHeader, RangeIterator};

impl Vm {
    #[inline]
    pub fn range_header_from_frame(&self, frame_offset: u8) -> RangeHeader {
        unsafe { *(self.memory.get_frame_const_ptr(frame_offset) as *const RangeHeader) }
    }

    pub fn range_iterator_ptr_from_frame(&self, frame_offset: u8) -> *mut RangeIterator {
        self.memory.get_frame_ptr(frame_offset) as *mut RangeIterator
    }

    #[inline]
    pub fn execute_range_iter_init(&mut self, target_iterator_addr: u8, range_header_on_frame: u8) {
        let range_header = self.range_header_from_frame(range_header_on_frame);

        let extra = i32::from(range_header.inclusive);

        let (start, end, direction) = if range_header.min <= range_header.max {
            (
                range_header.min,
                (range_header.max + extra - 1).max(range_header.min),
                1,
            )
        } else {
            (
                range_header.max,
                (range_header.min - extra + 1).min(range_header.max),
                -1,
            )
        };

        #[cfg(feature = "debug_vm")]
        {
            eprintln!("range_iter_init {start} to {end} dir:{direction}");
        }

        unsafe {
            let vec_iterator = RangeIterator {
                index: start,
                end,
                direction,
            };

            ptr::copy_nonoverlapping(
                &vec_iterator,
                self.memory
                    .get_frame_ptr(target_iterator_addr)
                    .cast::<RangeIterator>(),
                1, // bytes = count * sizeof(T)
            );
        }
    }

    #[inline]
    pub fn execute_range_iter_next(
        &mut self,
        target_iterator_addr: u8,
        target_variable: u8,
        jmp_absolute: u8,
    ) {
        let range_iterator = self.range_iterator_ptr_from_frame(target_iterator_addr);

        unsafe {
            if (*range_iterator).index == (*range_iterator).end {
                #[cfg(feature = "debug_vm")]
                {
                    eprintln!("range_iter_next complete. jumping {jmp_absolute:X}");
                }
                self.ip = jmp_absolute as usize;
            } else {
                let target_int_ptr = self.memory.get_frame_ptr_as_i32(target_variable);
                *target_int_ptr = (*range_iterator).index;
                #[cfg(feature = "debug_vm")]
                {
                    eprintln!("range_iter_next. index: {}", *target_int_ptr);
                }

                (*range_iterator).index += (*range_iterator).direction;
            }
        }
    }
}
