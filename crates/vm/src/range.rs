/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{get_reg, i16_from_u8s, set_reg, Vm};
use std::ptr;
use swamp_vm_isa::{RangeHeader, RangeIterator};

impl Vm {
    #[inline]
    #[must_use]
    pub fn get_range_header_ptr_from_reg(&self, range_reg: u8) -> *mut RangeHeader {
        self.get_ptr_from_reg(range_reg) as *mut RangeHeader
    }

    #[inline]
    #[must_use]
    pub fn range_header_from_reg(&self, range_reg: u8) -> RangeHeader {
        unsafe { *(self.get_const_ptr_from_reg(range_reg) as *const RangeHeader) }
    }

    pub fn range_iterator_ptr_from_reg(&self, range_iterator_reg: u8) -> *mut RangeIterator {
        self.get_ptr_from_reg(range_iterator_reg) as *mut RangeIterator
    }

    #[inline]
    pub fn execute_range_init(
        &mut self,
        range_target_reg: u8,
        min_reg: u8,
        max_reg: u8,
        is_inclusive: u8,
    ) {
        let range_header = self.get_range_header_ptr_from_reg(range_target_reg);

        unsafe {
            (*range_header).min = get_reg!(self, min_reg) as i32;
            (*range_header).max = get_reg!(self, max_reg) as i32;
            (*range_header).inclusive = get_reg!(self, is_inclusive) as i32 != 0;
        }
    }

    #[inline]
    pub fn execute_range_iter_init(&mut self, target_iterator_reg: u8, range_header_reg: u8) {
        let range_header = self.range_header_from_reg(range_header_reg);

        let extra = i32::from(range_header.inclusive);

        let (start, end, direction) = if range_header.min <= range_header.max {
            // Ascending range: [min, max] or [min, max)
            (
                range_header.min,
                range_header.max + extra, // End is one past max for inclusive, or exactly max for exclusive
                1,
            )
        } else {
            // Descending range: [min, max] or [min, max)
            // Here, min is the starting higher value, max is the ending lower value
            (
                range_header.min,
                range_header.max - extra, // End is one below max for inclusive, or exactly max for exclusive
                -1,
            )
        };

        #[cfg(feature = "debug_vm")]
        {}

        let iterator_target_ptr = self.range_iterator_ptr_from_reg(target_iterator_reg);

        unsafe {
            let range_iterator = RangeIterator {
                index: start,
                end,
                direction,
            };

            ptr::write(iterator_target_ptr, range_iterator);
        }
    }

    #[inline]
    pub fn execute_range_iter_next(
        &mut self,
        target_iterator_reg: u8,
        target_int_reg: u8,
        jmp_offset_lower: u8,
        jmp_offset_upper: u8,
    ) {
        let range_iterator = self.range_iterator_ptr_from_reg(target_iterator_reg);
        unsafe {
            #[cfg(feature = "debug_vm")]
            {
                if self.debug_operations_enabled {
                    eprintln!(
                        "range_iterator: index={}, end={}, direction={}",
                        (*range_iterator).index,
                        (*range_iterator).end,
                        (*range_iterator).direction
                    );
                }
            }

            if (*range_iterator).index == (*range_iterator).end {
                let jump_offset = i16_from_u8s!(jmp_offset_lower, jmp_offset_upper);
                #[cfg(feature = "debug_vm")]
                {
                    if self.debug_operations_enabled {
                        eprintln!("range_iter_next complete. jumping with offset {jump_offset}");
                    }
                }
                self.pc = (self.pc as i32 + jump_offset as i32) as usize;
            } else {
                set_reg!(self, target_int_reg, (*range_iterator).index);
                (*range_iterator).index += (*range_iterator).direction;
            }
        }
    }
}
