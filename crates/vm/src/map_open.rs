/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::memory::Memory;
use crate::{TrapCode, Vm, get_reg, i16_from_u8s};
use crate::{set_reg, u16_from_u8s};
use hashmap_mem::MapHeader;
use std::ptr;
use swamp_vm_types::MapIterator;

impl Vm {
    pub fn get_map_header(&self, header_reg: u8) -> *mut MapHeader {
        self.get_ptr_from_reg(header_reg) as *mut MapHeader
    }

    pub fn get_map_header_mut(&self, addr: u32) -> *mut MapHeader {
        self.memory.get_heap_ptr(addr as usize) as *mut MapHeader
    }

    pub fn get_map_header_const(&self, addr: u32) -> *const MapHeader {
        self.memory.get_heap_ptr(addr as usize) as *const MapHeader
    }

    pub fn read_map_header(&self, header_reg: u8) -> (MapHeader, u32) {
        let (map_ptr, map_addr) = self.get_ptr_and_addr_from_reg(header_reg);
        unsafe { (*(map_ptr as *const MapHeader), map_addr) }
    }

    #[must_use]
    pub fn read_map_header_from_heap(map_header_heap_addr: u32, heap: &Memory) -> MapHeader {
        let ptr = heap
            .get_heap_const_ptr(map_header_heap_addr as usize)
            .cast::<MapHeader>();
        unsafe { *ptr }
    }

    pub const ELEMENT_COUNT_FACTOR: f32 = 1.5;

    #[allow(clippy::too_many_lines)]
    fn get_or_reserve_entry(memory: &Memory, header: *mut MapHeader, key_ptr_addr: usize) -> u32 {
        unsafe {
            let key_ptr = memory.get_heap_ptr(key_ptr_addr);
            let entry_ptr = hashmap_mem::get_or_reserve_entry(header as *mut u8, key_ptr);
            if entry_ptr.is_null() {
                return 0;
            }

            memory.get_heap_offset(entry_ptr)
        }
    }
    pub fn execute_map_open_addressing_get_entry_location(
        &mut self,
        dst_entry_address: u8,
        self_map_header_reg: u8,
        key_source: u8,
    ) {
        let map_header_addr = get_reg!(self, self_map_header_reg);
        let map_header_ptr = self.get_map_header_mut(map_header_addr);
        let key_source_address = get_reg!(self, key_source) as usize;
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            unsafe {
                eprintln!(
                    "-- map_get_or_reserve_entry start {map_header_addr:X}, key_source: {key_source_address:X} {:?}",
                    *map_header_ptr
                );
            }
        }

        let address_to_entry: u32;

        unsafe {
            address_to_entry =
                Self::lookup_open_addressing(&self.memory, map_header_ptr, key_source_address);
        }

        if address_to_entry == 0 {
            return self.internal_trap(TrapCode::MapEntryNotFound);
        }

        set_reg!(self, dst_entry_address, address_to_entry);
    }

    // Had a bug where layout_type calculated the alignment and sizes
    // in one way, and the VM in another. Especially the value offset
    // were incorrectly calculated by tuple_offset + key_size.
    #[inline]

    pub fn execute_map_open_addressing_init(
        &mut self,
        self_map_header_reg: u8,
        logical_limit_immediate_lower: u8,
        logical_limit_immediate_upper: u8,
        key_size_reg: u8,
        key_alignment: u8,
        value_size_reg: u8,
        value_alignment: u8,
    ) {
        let map_header_addr = get_reg!(self, self_map_header_reg);
        let map_header = self.memory.get_heap_ptr(map_header_addr as usize);
        let logical_limit =
            u16_from_u8s!(logical_limit_immediate_lower, logical_limit_immediate_upper);
        let key_size = get_reg!(self, key_size_reg);
        let value_size = get_reg!(self, value_size_reg);
        let capacity = logical_limit.next_power_of_two();
        debug_assert_ne!(capacity, 0);
        assert!(capacity.is_power_of_two());

        let (bucket_layout, map_init) = hashmap_mem::layout(
            key_size,
            key_alignment,
            value_size,
            value_alignment,
            logical_limit,
        );

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            let map_header_addr = get_reg!(self, self_map_header_reg);
            eprintln!(
                "map_init {map_header_addr:08X}:  logical_limit:{logical_limit} capacity:{capacity}, key_size:{key_size}, key_alignment:{key_alignment}, value_size:{value_size}, value_alignment:{value_alignment}, bucket_size:{}, value_offset:{}",
                bucket_layout.bucket_size, bucket_layout.value_offset,
            );
        }

        unsafe {
            hashmap_mem::init(map_header, &map_init);
        }
    }

    pub fn execute_map_open_addressing_get_or_reserve_entry(
        &mut self,
        dst_entry_address: u8,
        self_map_header_reg: u8,
        key_source_ptr_reg: u8,
    ) {
        let map_header_addr = get_reg!(self, self_map_header_reg);
        let map_header = self.get_map_header_mut(map_header_addr);

        let key_source_address = get_reg!(self, key_source_ptr_reg) as usize;

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "-- map_get_or_reserve_entry start {map_header_addr:X}, key_source: {key_source_address:X} {map_header:?}",
            );
        }

        let entry_address;

        //if entry_address == 0 {
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("map_get_or_reserve_entry: it didn't exist, so try to find a new entry");
        }
        unsafe {
            entry_address =
                Self::get_or_reserve_entry(&self.memory, map_header, key_source_address);
            if entry_address == 0 {
                return self.internal_trap(TrapCode::MapEntryNotFoundAndCouldNotBeCreated);
            }
        }
        //}

        set_reg!(self, dst_entry_address, entry_address);
    }

    pub fn execute_map_open_addressing_has(
        &mut self,
        dest_reg: u8,
        self_const_map_header_reg: u8,
        key_source_reg: u8,
    ) {
        let map_header_addr = get_reg!(self, self_const_map_header_reg) as usize;
        let map_header_ptr = self.get_map_header_mut(map_header_addr as u32);
        let key_source_address = get_reg!(self, key_source_reg) as usize;

        unsafe {
            let found = Self::has_open_addressing(&self.memory, map_header_ptr, key_source_address);

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                unsafe {
                    eprintln!(
                        "map.has returned: {found} for key pointer:{:X} map:{:?}",
                        key_source_address, *map_header_ptr
                    );
                }
            }

            set_reg!(self, dest_reg, found);
        }
    }

    pub fn execute_map_overwrite(&mut self, target_map_header_reg: u8, source_map_header_reg: u8) {
        let target_map_header_addr = get_reg!(self, target_map_header_reg);
        let target_map_header = self.get_map_header_mut(target_map_header_addr);

        let source_map_header_addr = get_reg!(self, source_map_header_reg);
        let source_map_header = self.get_map_header_const(source_map_header_addr);

        let could_overwrite = unsafe {
            hashmap_mem::overwrite(target_map_header as *mut u8, source_map_header as *const u8)
        };
        if !could_overwrite {
            self.internal_trap(TrapCode::MapCouldNotBeCopied);
        }
    }

    pub fn execute_map_open_addressing_remove(
        &mut self,
        self_map_header_reg: u8,
        key_source_reg: u8,
    ) {
        let map_header_addr = get_reg!(self, self_map_header_reg);
        let map_header = self.get_map_header_mut(map_header_addr);
        let key_source_address = get_reg!(self, key_source_reg) as usize;

        unsafe {
            let found = Self::remove_open_addressing(&self.memory, map_header, key_source_address);
            if !found {
                self.internal_trap(TrapCode::MapEntryNotFoundForRemoval);
            }
        }
    }

    ///
    /// Returns `true` if the key is found within the maximum probe distance,
    /// `false` otherwise.
    unsafe fn has_open_addressing(
        memory: &Memory,
        map_header: *mut MapHeader,
        key_ptr_addr: usize,
    ) -> bool { unsafe {
        let key_ptr = memory.get_heap_const_ptr(key_ptr_addr);
        hashmap_mem::has(map_header as *mut u8, key_ptr)
    }}

    /// Looks up a key in the hash map using open addressing and linear probing.
    ///
    /// If the key is found, its corresponding value is copied into `value_out_ptr`
    /// and the function returns `true`. Otherwise, it returns `false`.
    ///
    unsafe fn lookup_open_addressing(
        memory: &Memory,
        map_header: *mut MapHeader,
        key_ptr_addr: usize,
    ) -> u32 { unsafe {
        let key_ptr = memory.get_heap_const_ptr(key_ptr_addr);

        let value_ptr = hashmap_mem::lookup(map_header as *mut u8, key_ptr);

        if value_ptr.is_null() {
            0
        } else {
            memory.get_heap_offset(value_ptr)
        }
    }}

    /// Removes an entry from the hash map given a key using open addressing.
    ///
    /// If the key is found, its corresponding bucket is marked with a tombstone,
    /// and the function returns `true`. Otherwise, it returns `false`.
    unsafe fn remove_open_addressing(
        memory: &Memory,
        map_header_ptr: *mut MapHeader,
        key_ptr_addr: usize,
    ) -> bool { unsafe {
        let key_ptr = memory.get_heap_const_ptr(key_ptr_addr);

        hashmap_mem::remove(map_header_ptr as *mut u8, key_ptr)
    }}

    pub(crate) fn execute_map_iter_init(
        &mut self,
        target_map_iterator_header_reg: u8,
        map_header_reg: u8,
    ) {
        let map_header_addr = get_reg!(self, map_header_reg);

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            let iter_addr = get_reg!(self, target_map_iterator_header_reg);
            let map_header = Self::read_map_header_from_heap(map_header_addr, &self.memory);
            debug_assert_eq!(
                map_header.padding_and_secret_code,
                hashmap_mem::SECRET_CODE,
                "secret code is not the same"
            );
            eprintln!(
                "map_iter_init: iter_addr: {iter_addr:04X} map_header_addr:{map_header_addr:04X} key_size:{}, value_offset:{}, value_size:{} bucket_size: {}",
                map_header.key_size,
                map_header.value_offset,
                map_header.value_size,
                (map_header.bucket_size)
            );
        }
        let map_iterator = MapIterator {
            map_header_frame_offset: map_header_addr,
            index: 0,
        };

        let map_iterator_mut_ptr =
            self.get_ptr_from_reg(target_map_iterator_header_reg) as *mut MapIterator;

        unsafe {
            ptr::write(map_iterator_mut_ptr, map_iterator);
        }
    }

    pub fn get_map_iterator_header_ptr_from_reg(&self, map_iterator_reg: u8) -> *mut MapIterator {
        self.get_ptr_from_reg(map_iterator_reg) as *mut MapIterator
    }

    pub fn execute_map_iter_next_pair(
        &mut self,
        map_iterator_header_reg: u8,
        target_key_reg: u8,
        target_value_reg: u8,
        branch_offset_lower: u8,
        branch_offset_upper: u8,
    ) {
        let map_iterator = self.get_map_iterator_header_ptr_from_reg(map_iterator_header_reg);

        unsafe {
            let map_header_addr = (*map_iterator).map_header_frame_offset;
            let map_header_ptr =
                self.memory.get_heap_const_ptr(map_header_addr as usize) as *const MapHeader;
            let map_header = &*map_header_ptr;
            if map_header.padding_and_secret_code != hashmap_mem::SECRET_CODE {
                return self.internal_trap(TrapCode::MemoryCorruption);
            }
            debug_assert_eq!(map_header.padding_and_secret_code, hashmap_mem::SECRET_CODE);

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                let iter_addr = get_reg!(self, map_iterator_header_reg);
                let index = (*map_iterator).index;
                eprintln!(
                    "map_iter_next: iter_addr: {iter_addr:04X} addr:{map_header_addr:04X} index:{index} len: {}, capacity: {}",
                    map_header.element_count, map_header.capacity
                );
            }

            let index = (*map_iterator).index;

            let (key_ptr, value_ptr, found_index) =
                hashmap_mem::find_next_valid_entry(map_header_ptr as *mut u8, index as u16);
            if !key_ptr.is_null() {
                let key_offset = self.memory.get_heap_offset(key_ptr);
                let value_offset = self.memory.get_heap_offset(value_ptr);

                set_reg!(self, target_key_reg, key_offset);
                set_reg!(self, target_value_reg, value_offset);
                (*map_iterator).index = (found_index + 1) as u32;
            } else {
                // Jump to the provided address if we're done
                let branch_offset = i16_from_u8s!(branch_offset_lower, branch_offset_upper);

                #[cfg(feature = "debug_vm")]
                {
                    if self.debug_operations_enabled {
                        eprintln!(
                            "map_iter_next_pair complete. jumping with offset {branch_offset}"
                        );
                    }
                }

                self.pc = (self.pc as i32 + branch_offset as i32) as usize;
            }
        }
    }

    pub fn execute_map_iter_next(
        &mut self,
        map_iterator_header_reg: u8,
        target_value_reg: u8,
        branch_offset_lower: u8,
        branch_offset_upper: u8,
    ) {
        let map_iterator = self.get_map_iterator_header_ptr_from_reg(map_iterator_header_reg);

        unsafe {
            let map_header_addr = (*map_iterator).map_header_frame_offset;
            let map_header_ptr =
                self.memory.get_heap_const_ptr(map_header_addr as usize) as *const MapHeader;
            let map_header = &*map_header_ptr;

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                let iter_addr = get_reg!(self, map_iterator_header_reg);
                let index = (*map_iterator).index;
                eprintln!(
                    "map_iter_next: iter_addr: {iter_addr:04X} addr:{map_header_addr:04X} index:{index} len: {}, capacity: {}",
                    map_header.element_count, map_header.capacity
                );
            }

            let index = (*map_iterator).index;

            let (_key_ptr, value_ptr, found_index) =
                hashmap_mem::find_next_valid_entry(map_header_ptr as *mut u8, index as u16);
            if !value_ptr.is_null() {
                let value_offset = self.memory.get_heap_offset(value_ptr);

                set_reg!(self, target_value_reg, value_offset);
                (*map_iterator).index = (found_index + 1) as u32;
            } else {
                // Jump to the provided address if we're done
                let branch_offset = i16_from_u8s!(branch_offset_lower, branch_offset_upper);

                #[cfg(feature = "debug_vm")]
                {
                    if self.debug_operations_enabled {
                        eprintln!(
                            "map_iter_next_pair complete. jumping with offset {branch_offset}"
                        );
                    }
                }

                self.pc = (self.pc as i32 + branch_offset as i32) as usize;
            }
        }
    }
}
