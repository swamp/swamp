/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
extern crate fxhash;
use crate::memory::Memory;
use crate::{TrapCode, Vm, get_reg};
use crate::{set_reg, u16_from_u8s};
use fxhash::FxHasher64;
use std::cmp::min;
use std::hash::Hasher;
use std::{ptr, slice};
use swamp_vm_types::{MAP_BUCKETS_OFFSET, MAP_HEADER_ALIGNMENT, MapHeader};

impl Vm {
    const MAX_PROBES: usize = 8;
    pub const BUCKET_EMPTY: u8 = 0;
    pub const BUCKET_TOMBSTONE: u8 = 1;
    pub const BUCKET_OCCUPIED: u8 = 2;

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

    const MAX_PROBE_DISTANCE: usize = 32; // TODO: tweak this, only guessing for now

    // Looks up the key in the map. If it can not find the key, it reserves an entry and
    // returns the offset to it.
    // Uses linear probing with a max distance. Hopefully it will work out
    // otherwise we have to go through the capacity.
    // https://en.wikipedia.org/wiki/Open_addressing
    unsafe fn get_or_reserve_entry(
        memory: &Memory,
        buckets_ptr_addr: usize,
        header: &MapHeader,
        key_ptr_addr: usize,
    ) -> u32 {
        unsafe {
            let capacity = header.capacity as usize;
            let key_size = header.key_size as usize;
            let element_size = header.tuple_size as usize;
            debug_assert_ne!(key_size, 0);
            debug_assert_ne!(element_size, 0);
            debug_assert_ne!(capacity, 0);
            debug_assert!(Self::is_power_of_two(capacity));

            let buckets_ptr = memory.get_heap_ptr(buckets_ptr_addr);
            let key_ptr = memory.get_heap_ptr(key_ptr_addr);

            // Calculate bucket layout sizes
            let status_size: usize = MAP_HEADER_ALIGNMENT.into();
            let bucket_size: usize = status_size + element_size;

            let key_slice = slice::from_raw_parts(key_ptr, key_size);
            let hash = Self::calculate_hash(key_slice);

            // Use bitwise AND for modulo (capacity is always a power of two)
            let mut index = (hash as usize) & (capacity - 1);
            #[cfg(feature = "debug_vm")]
            {
                eprintln!(
                    "map: wants to insert hash: {hash:08X} starting at: {index} capacity: {}, key_size: {key_size} element_size: {element_size}",
                    header.capacity
                );
            }

            // Keep track of the first tombstone found, if any
            let mut first_tombstone_index: Option<usize> = None;
            let max_probe_count = min(capacity, Self::MAX_PROBE_DISTANCE);

            // Linear probing loop with max distance
            for _ in 0..max_probe_count {
                let bucket_start_ptr = buckets_ptr.add(index * bucket_size);
                let status_ptr = bucket_start_ptr;

                let status = ptr::read(status_ptr);
                #[cfg(feature = "debug_vm")]
                {
                    eprintln!("index: {index}, status {status}, bucket_size {bucket_size}");
                }
                if status == Self::BUCKET_EMPTY {
                    // Found an empty slot. Key is not present.
                    // use tombstone if found on the way. it is closer to the initial index from hash.
                    let insertion_index = first_tombstone_index.unwrap_or(index);

                    // Calculate pointers for the insertion spot.
                    let target_bucket_ptr = buckets_ptr.add(insertion_index * bucket_size);
                    let target_status_ptr = target_bucket_ptr;
                    let target_key_ptr = target_bucket_ptr.add(status_size);
                    let target_value_ptr = target_key_ptr.add(key_size);

                    // Write status, key, and value
                    ptr::write(target_status_ptr, Self::BUCKET_OCCUPIED);
                    ptr::copy_nonoverlapping(key_ptr, target_key_ptr, key_size);
                    //ptr::copy_nonoverlapping(value_ptr, target_value_ptr, value_size);
                    #[cfg(feature = "debug_vm")]
                    {
                        eprintln!("found empty bucket, write to index {index}");
                    }

                    return memory.get_heap_offset(target_value_ptr);
                } else if status == Self::BUCKET_OCCUPIED {
                    // Slot is occupied, check if keys match.
                    let existing_key_ptr = bucket_start_ptr.add(status_size);
                    let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                    if key_slice == existing_key_slice {
                        #[cfg(feature = "debug_vm")]
                        {
                            eprintln!("keys matched, so we found it. overwriting index {index}");
                        }
                        // Keys match, we can just overwrite the value portion
                        let value_dest_ptr = existing_key_ptr.add(key_size);
                        return memory.get_heap_offset(value_dest_ptr);
                    }
                    #[cfg(feature = "debug_vm")]
                    {
                        eprintln!("keys didn't match, so continue searching");
                    }
                    // Keys don't match (collision), just continue the loop
                } else if status == Self::BUCKET_TOMBSTONE {
                    // Found a tombstone. Record its index if it's the first (earliest) one.
                    if first_tombstone_index.is_none() {
                        first_tombstone_index = Some(index);
                    }
                    #[cfg(feature = "debug_vm")]
                    {
                        eprintln!("found tombstone but continue searching");
                    }

                    // Continue probing, as the key might exist later.
                }

                // Linear probing: move to the next index, wrap around.
                index = (index + 1) & (capacity - 1);
            }

            // If we get here, we've exceeded MAX_PROBE_DISTANCE without finding
            // an empty slot or the key. If we found a tombstone, use that, otherwise it failed miserably.
            if let Some(tombstone_index) = first_tombstone_index {
                // calculate pointers to the earliest tombstone
                let target_bucket_ptr = buckets_ptr.add(tombstone_index * bucket_size);
                let target_status_ptr = target_bucket_ptr;
                let target_key_ptr = target_bucket_ptr.add(status_size);
                let target_value_ptr = target_key_ptr.add(key_size);

                #[cfg(feature = "debug_vm")]
                {
                    eprintln!("found tombstone in the end, overwriting");
                }
                // Write status, key, and value
                ptr::write(target_status_ptr, Self::BUCKET_OCCUPIED);
                ptr::copy_nonoverlapping(key_ptr, target_key_ptr, key_size);

                return memory.get_heap_offset(target_value_ptr);
            }

            #[cfg(feature = "debug_vm")]
            {
                eprintln!("MAP IS FULL, CAN NOT INSERT");
            }

            // If we reach here, the map is close to full or completely full and with no tombstones.
            0
        }
    }

    fn calculate_hash(key_bytes: &[u8]) -> u64 {
        let mut hasher = FxHasher64::default();
        hasher.write(key_bytes);
        let unique = hasher.finish();
        eprintln!(
            "calculated hash: {unique:08X} for key of size {}",
            key_bytes.len()
        );
        unique
    }

    pub fn execute_map_open_addressing_get_entry_location(
        &mut self,
        dst_entry_address: u8,
        self_map_header_reg: u8,
        key_source: u8,
    ) {
        let (map_header, map_header_addr) = self.read_map_header(self_map_header_reg);

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("lookup in bucket: {map_header_addr:04X}");
        }

        let buckets_start_addr = (map_header_addr + MAP_BUCKETS_OFFSET.0 as u32) as usize;

        let key_source_address = get_reg!(self, key_source) as usize;

        let address_to_entry: u32;

        unsafe {
            address_to_entry = Self::lookup_open_addressing(
                &self.memory,
                buckets_start_addr,
                &map_header,
                key_source_address,
            );
        }

        if address_to_entry == 0 {
            return self.internal_trap(TrapCode::MapEntryNotFound);
        }

        set_reg!(self, dst_entry_address, address_to_entry);
    }

    const fn is_power_of_two(n: usize) -> bool {
        n > 0 && (n & (n - 1)) == 0
    }

    pub fn execute_map_open_addressing_init(
        &mut self,
        self_map_header_reg: u8,
        capacity_lower: u8,
        capacity_upper: u8,
        key_size_lower: u8,
        key_size_upper: u8,
        tuple_size_lower: u8,
        tuple_size_upper: u8,
    ) {
        let map_header = self.get_map_header(self_map_header_reg);
        let capacity = u16_from_u8s!(capacity_lower, capacity_upper);
        let key_size = u16_from_u8s!(key_size_lower, key_size_upper);
        let tuple_size = u16_from_u8s!(tuple_size_lower, tuple_size_upper);
        assert!(Self::is_power_of_two(capacity as usize));
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            let map_header_addr = get_reg!(self, self_map_header_reg);
            eprintln!(
                "map_init {map_header_addr:08X}:  capacity: {capacity}, key_size: {key_size}, tuple_size: {tuple_size_lower}"
            );
        }
        unsafe {
            (*map_header).capacity = capacity;
            (*map_header).key_size = key_size;
            (*map_header).tuple_size = tuple_size;
        }
    }

    pub fn execute_map_open_addressing_get_or_reserve_entry(
        &mut self,
        dst_entry_address: u8,
        self_map_header_reg: u8,
        key_source_ptr_reg: u8,
    ) {
        let (map_header, map_header_addr) = self.read_map_header(self_map_header_reg);
        let key_source_address = get_reg!(self, key_source_ptr_reg) as usize;
        let buckets_start_addr = (map_header_addr + MAP_BUCKETS_OFFSET.0 as u32) as usize;

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "map_get_or_reserve_entry {map_header_addr:X}, key_source: {key_source_address:08X}"
            );
        }

        let mut entry_address = unsafe {
            Self::lookup_open_addressing(
                &self.memory,
                buckets_start_addr,
                &map_header,
                key_source_address,
            )
        };

        if entry_address == 0 {
            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                eprintln!("map: try to get or a reserve entry");
            }
            unsafe {
                entry_address = Self::get_or_reserve_entry(
                    &self.memory,
                    buckets_start_addr,
                    &map_header,
                    key_source_address,
                );
                if entry_address == 0 {
                    return self.internal_trap(TrapCode::MapEntryNotFoundAndCouldNotBeCreated);
                }
            }
        }

        set_reg!(self, dst_entry_address, entry_address);
    }

    pub fn execute_map_open_addressing_has(
        &mut self,
        dest_reg: u8,
        self_const_map_header_reg: u8,
        key_source_reg: u8,
    ) {
        let (map_header, map_header_addr) = self.read_map_header(self_const_map_header_reg);
        let key_source_address = get_reg!(self, key_source_reg) as usize;
        let buckets_start_addr = (map_header_addr + MAP_BUCKETS_OFFSET.0 as u32) as usize;
        unsafe {
            let found = Self::has_open_addressing(
                &self.memory,
                buckets_start_addr,
                &map_header,
                key_source_address,
            );
            eprintln!("map: has:{}", found);
            set_reg!(self, dest_reg, found);
        }
    }

    pub fn execute_map_open_addressing_remove(
        &mut self,
        self_const_map_header_reg: u8,
        key_source_reg: u8,
    ) {
        let (map_header, map_header_addr) = self.read_map_header(self_const_map_header_reg);
        let key_source_address = get_reg!(self, key_source_reg) as usize;
        let buckets_start_addr = (map_header_addr + MAP_BUCKETS_OFFSET.0 as u32) as usize;
        unsafe {
            let found = Self::remove_open_addressing(
                &self.memory,
                buckets_start_addr,
                &map_header,
                key_source_address,
            );
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
        buckets_ptr_addr: usize,
        header: &MapHeader,
        key_ptr_addr: usize,
    ) -> bool {
        unsafe {
            let capacity = header.capacity as usize;
            let key_size = header.key_size as usize;
            let element_size = header.tuple_size as usize;
            debug_assert_ne!(key_size, 0);
            debug_assert_ne!(element_size, 0);
            debug_assert_ne!(capacity, 0);
            debug_assert!(Self::is_power_of_two(capacity));

            let key_ptr = memory.get_heap_const_ptr(key_ptr_addr);
            let key_slice = slice::from_raw_parts(key_ptr, key_size);
            let hash = Self::calculate_hash(key_slice);

            let buckets_ptr = memory.get_heap_const_ptr(buckets_ptr_addr);

            // Calculate bucket layout sizes
            let status_size: usize = MAP_HEADER_ALIGNMENT.into();
            let bucket_size: usize = status_size + element_size;

            // Use bitwise AND for modulo (capacity is always a power of two)
            let mut index = (hash as usize) & (capacity - 1);

            #[cfg(feature = "debug_vm")]
            {
                eprintln!(
                    "checks if has item with hash: {hash:08X} start index: {index} capacity: {capacity} key_size:{key_size} value_size: {element_size}"
                );
            }
            let max_probe_count = min(capacity, Self::MAX_PROBE_DISTANCE);

            // Linear probing loop with max distance
            for _ in 0..max_probe_count {
                let bucket_start_ptr = buckets_ptr.add(index * bucket_size);
                let status_ptr = bucket_start_ptr; // Status is at the start

                let status = ptr::read(status_ptr);

                if status == Self::BUCKET_EMPTY {
                    // Found an empty slot. The key cannot be present.
                    return false;
                } else if status == Self::BUCKET_OCCUPIED {
                    // Slot is occupied, check if the keys match.
                    let existing_key_ptr = bucket_start_ptr.add(status_size);
                    let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                    if key_slice == existing_key_slice {
                        // Keys match! The key exists.
                        return true;
                    }
                    // Keys don't match (collision), continue probing.
                } else if status == Self::BUCKET_TOMBSTONE {
                    // Found a tombstone. The key might be further down. Continue probing.
                }

                // Linear probing: move to the next index, wrap around.
                index = (index + 1) & (capacity - 1);
            }

            // If we exit the loop, we've probed MAX_PROBE_DISTANCE slots
            // without finding the key or hitting an empty slot.
            false
        }
    }

    /// Looks up a key in the hash map using open addressing and linear probing.
    ///
    /// If the key is found, its corresponding value is copied into `value_out_ptr`
    /// and the function returns `true`. Otherwise, it returns `false`.
    ///
    unsafe fn lookup_open_addressing(
        memory: &Memory,
        buckets_ptr_addr: usize,
        header: &MapHeader,
        key_ptr_addr: usize,
    ) -> u32 {
        unsafe {
            let capacity = header.capacity as usize;
            debug_assert_ne!(capacity, 0);
            debug_assert!(Self::is_power_of_two(capacity));
            let key_size = header.key_size as usize;
            debug_assert_ne!(key_size, 0);
            let element_size = header.tuple_size as usize;
            debug_assert_ne!(element_size, 0);

            // Calculate bucket layout sizes (same as insert)
            let status_size: usize = MAP_HEADER_ALIGNMENT.into();
            let bucket_size: usize = status_size + element_size;

            let key_ptr = memory.get_heap_const_ptr(key_ptr_addr);

            let key_slice = slice::from_raw_parts(key_ptr, key_size);
            let hash = Self::calculate_hash(key_slice);

            // Use bitwise AND for modulo (capacity is always a power of two)
            let mut index = (hash as usize) & (capacity - 1);

            let buckets_ptr = memory.get_heap_const_ptr(buckets_ptr_addr);

            #[cfg(feature = "debug_vm")]
            {
                eprintln!("calculated search hash {hash:08X} starting at {index}");
            }

            let max_probe_count = min(capacity, Self::MAX_PROBE_DISTANCE);
            // Linear probing loop with max distance
            for _ in 0..max_probe_count {
                let bucket_start_ptr = buckets_ptr.add(index * bucket_size);
                let status_ptr = bucket_start_ptr; // Status is at the start

                let status = ptr::read(status_ptr);

                if status == Self::BUCKET_EMPTY {
                    // Found an empty slot. The key cannot be present further down
                    // the probe sequence, because insertion would have stopped here.
                    #[cfg(feature = "debug_vm")]
                    {
                        eprintln!("found empty bucket, so gave up on searching");
                    }
                    return 0;
                } else if status == Self::BUCKET_OCCUPIED {
                    // Slot is occupied, check if the keys match.
                    let existing_key_ptr = bucket_start_ptr.add(status_size);
                    let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                    if key_slice == existing_key_slice {
                        #[cfg(feature = "debug_vm")]
                        {
                            eprintln!(
                                "matching new key {key_slice:?} with existing key {existing_key_slice:?}. returning this existing entry at {index}, adding key_size {key_size}"
                            );
                        }
                        // Keys match! return the pointer.
                        let value_src_ptr = existing_key_ptr.add(key_size);
                        return memory.get_heap_offset(value_src_ptr);
                    }
                    // Keys don't match (collision), continue probing.
                } else if status == Self::BUCKET_TOMBSTONE {
                    // Found a tombstone. The key we are looking for might be
                    // further down the probe sequence, so we must continue searching.
                }

                // Linear probing: move to the next index, wrap around.
                index = (index + 1) & (capacity - 1);
            }

            // If we exit the loop, we've probed MAX_PROBE_DISTANCE slots
            // without finding the key or hitting an empty slot.
            // Therefore, the key is not considered present within the probe limit.
            #[cfg(feature = "debug_vm")]
            {
                eprintln!("lookup failed to find any matches, returning 0");
            }
            0
        }
    }

    /// Removes an entry from the hash map given a key using open addressing.
    ///
    /// If the key is found, its corresponding bucket is marked with a tombstone,
    /// and the function returns `true`. Otherwise, it returns `false`.
    unsafe fn remove_open_addressing(
        memory: &Memory,
        buckets_ptr_addr: usize,
        header: &MapHeader,
        key_ptr_addr: usize,
    ) -> bool {
        unsafe {
            let capacity = header.capacity as usize;
            debug_assert_ne!(capacity, 0);
            debug_assert!(Self::is_power_of_two(capacity));

            let key_size = header.key_size as usize;
            debug_assert_ne!(key_size, 0);

            let element_size = header.tuple_size as usize;
            debug_assert_ne!(element_size, 0);

            let status_size: usize = MAP_HEADER_ALIGNMENT.into();
            let bucket_size: usize = status_size + element_size;

            let key_ptr = memory.get_heap_const_ptr(key_ptr_addr);
            let key_slice = slice::from_raw_parts(key_ptr, key_size);
            let hash = Self::calculate_hash(key_slice);

            let buckets_ptr_mut = memory.get_heap_ptr(buckets_ptr_addr);

            // Use bitwise AND for modulo (capacity is always a power of two)
            let mut index = (hash as usize) & (capacity - 1);

            #[cfg(feature = "debug_vm")]
            {
                eprintln!(
                    "Attempting to remove item with hash: {hash:08X} starting at index: {index}"
                );
            }

            let max_probe_count = min(capacity, Self::MAX_PROBE_DISTANCE);

            for _ in 0..max_probe_count {
                let bucket_start_ptr_mut = buckets_ptr_mut.add(index * bucket_size);
                let status_ptr_mut = bucket_start_ptr_mut; // Status is at the start

                let status = ptr::read(status_ptr_mut);

                if status == Self::BUCKET_EMPTY {
                    // Found an empty slot. The key cannot be present further down
                    // the probe sequence, because insertion would have stopped here.
                    #[cfg(feature = "debug_vm")]
                    {
                        eprintln!("Found empty bucket, key not found for removal.");
                    }
                    return false;
                } else if status == Self::BUCKET_OCCUPIED {
                    let existing_key_ptr = bucket_start_ptr_mut.add(status_size);
                    let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                    if key_slice == existing_key_slice {
                        // We found it! Mark this bucket as a tombstone.
                        ptr::write(status_ptr_mut, Self::BUCKET_TOMBSTONE);
                        #[cfg(feature = "debug_vm")]
                        {
                            eprintln!("Successfully removed (tombstone) entry at index: {index}");
                        }
                        return true;
                    }
                } else if status == Self::BUCKET_TOMBSTONE {
                    // The key we are looking for might be
                    // further down the probe sequence, so we must continue searching.
                }

                // Linear probing: move to the next index, wrap around.
                index = (index + 1) & (capacity - 1);
            }

            // If we exit the loop, we've probed MAX_PROBE_DISTANCE slots
            // without finding the key or hitting an empty slot.
            #[cfg(feature = "debug_vm")]
            {
                eprintln!("Removal failed: key not found within MAX_PROBE_DISTANCE.");
            }
            false
        }
    }
}
