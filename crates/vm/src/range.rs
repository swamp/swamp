use crate::Reg;
use crate::{Vm, set_reg};
use std::ptr;
use swamp_vm_types::{RangeHeader, RangeIterator};

impl Vm {
    #[inline]
    pub fn range_header_from_reg(&self, range_reg: u8) -> RangeHeader {
        unsafe { *(self.get_const_ptr_from_reg(range_reg) as *const RangeHeader) }
    }

    pub fn range_iterator_ptr_from_reg(&self, range_iterator_reg: u8) -> *mut RangeIterator {
        self.get_ptr_from_reg(range_iterator_reg) as *mut RangeIterator
    }

    #[inline]
    pub fn execute_range_iter_init(&mut self, target_iterator_reg: u8, range_header_reg: u8) {
        let range_header = self.range_header_from_reg(range_header_reg);

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

        let iterator_target_ptr = self.range_iterator_ptr_from_reg(target_iterator_reg);

        unsafe {
            let vec_iterator = RangeIterator {
                index: start,
                end,
                direction,
            };

            ptr::copy_nonoverlapping(
                &vec_iterator,
                iterator_target_ptr,
                1, // bytes = count * sizeof(T)
            );
        }
    }

    #[inline]
    pub fn execute_range_iter_next(
        &mut self,
        target_iterator_reg: u8,
        target_int_reg: u8,
        jmp_absolute: u8,
    ) {
        let range_iterator = self.range_iterator_ptr_from_reg(target_iterator_reg);

        unsafe {
            if (*range_iterator).index == (*range_iterator).end {
                #[cfg(feature = "debug_vm")]
                {
                    eprintln!("range_iter_next complete. jumping {jmp_absolute:X}");
                }
                self.ip = jmp_absolute as usize;
            } else {
                set_reg!(self, target_int_reg, as I32 <- (*range_iterator).index);
                (*range_iterator).index += (*range_iterator).direction;
            }
        }
    }
}
