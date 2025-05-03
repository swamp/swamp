/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Vm;
use crate::memory::Memory;
use std::hash::{DefaultHasher, Hasher};
use std::{ptr, slice};
use swamp_vm_types::{MAP_HEADER_SIZE, MapHeader};

impl Vm {
    const MAX_PROBES: usize = 8;
    pub const BUCKET_EMPTY: u8 = 0;
    pub const BUCKET_TOMBSTONE: u8 = 1;
    pub const BUCKET_OCCUPIED: u8 = 2;

    pub fn get_heap_map_header(&self, heap_addr: u32) -> *mut MapHeader {
        self.memory.get_heap_ptr(heap_addr as usize) as *mut MapHeader
    }

    pub fn read_heap_map_header_via_frame(mem: &Memory, frame_addr: u16) -> MapHeader {
        unsafe { *(mem.get_heap_ptr_via_frame(frame_addr) as *const MapHeader) }
    }

    pub fn read_heap_map_header_from_heap(heap_address: u32, heap: &Memory) -> MapHeader {
        unsafe { *(heap.get_heap_const_ptr(heap_address as usize) as *const MapHeader) }
    }

    pub const ELEMENT_COUNT_FACTOR: f32 = 1.5;
    pub(crate) fn execute_map_open_addressing_from_slice(
        &mut self,
        dst_offset: u16,
        slice_addr: u16,
    ) {
        let slice_pairs = self.slice_pair_header_from_frame(slice_addr);

        debug_assert_ne!(slice_pairs.key_size, 0);
        debug_assert_ne!(slice_pairs.value_size, 0);
        //debug_assert_ne!(element_count, 0);

        // TODO: HACK: count should not be adjusted to 128 in the future
        let adjusted_count = slice_pairs.element_count.max(128);
        let min_capacity = ((adjusted_count as u32 * 4 + 2) / 3).max(8) as u16;
        let capacity = min_capacity.next_power_of_two() as u16;

        // The bucket doesn't have to be aligned since we will copy key and value in bytes
        let buckets_heap_addr = self.memory.heap_allocate(
            (capacity * (1 + slice_pairs.key_size + slice_pairs.value_size)) as usize,
        );

        let map_header_on_heap_addr = self.memory.heap_allocate(MAP_HEADER_SIZE.0 as usize);
        let map_header_ptr = self.get_heap_map_header(map_header_on_heap_addr);

        unsafe {
            (*map_header_ptr).heap_offset = buckets_heap_addr;
            (*map_header_ptr).capacity = capacity as u32;
            (*map_header_ptr).element_count = slice_pairs.element_count as u32;
            (*map_header_ptr).key_size = slice_pairs.key_size as u32;
            (*map_header_ptr).value_size = slice_pairs.value_size as u32;
        }

        let buckets_ptr = self.memory.get_heap_ptr(buckets_heap_addr as usize);

        let slice_ptr = self.memory.get_heap_ptr(slice_pairs.heap_offset as usize);

        let pair_size = slice_pairs.key_size + slice_pairs.value_size;
        for i in 0..slice_pairs.element_count {
            eprintln!("inserting {i}");
            let pair_offset = i * pair_size;
            let key_ptr = unsafe { slice_ptr.add(pair_offset as usize) };
            let value_ptr = unsafe { key_ptr.add(slice_pairs.key_size as usize) };

            let worked = unsafe {
                Self::insert_open_addressing(buckets_ptr, &*map_header_ptr, key_ptr, value_ptr)
            };
            assert!(worked, "problem with hashmap");
        }

        unsafe { *self.memory.get_frame_ptr_as_u32(dst_offset) = map_header_on_heap_addr }
    }

    const MAX_PROBE_DISTANCE: usize = 32; // TODO: tweak this, only guessing for now

    // Inserts a key and value into the hash map using open addressing
    // and linear probing with a max distance. Hopefully it will work out
    // otherwise we have to go through the capacity.
    // https://en.wikipedia.org/wiki/Open_addressing
    unsafe fn insert_open_addressing(
        buckets_ptr: *mut u8,
        header: &MapHeader,
        key_ptr: *const u8,
        value_ptr: *const u8,
    ) -> bool {
        let capacity = header.capacity as usize;
        let key_size = header.key_size as usize;
        let value_size = header.value_size as usize;
        debug_assert_ne!(key_size, 0);
        debug_assert_ne!(value_size, 0);

        // Calculate bucket layout sizes
        // Alignment doesn't matter since we are not accessing key and value fields directly,
        // only copy the bytes
        let status_size: usize = 1;
        let bucket_size: usize = status_size + key_size + value_size;

        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = Self::calculate_hash(key_slice);

        // Use bitwise AND for modulo (capacity is always a power of two)
        let mut index = (hash as usize) & (capacity - 1);
        #[cfg(feature = "debug_vm")]
        {
            eprintln!(
                "wants to insert hash: {hash:08X} starting at: {index} capacity: {}, key_size: {key_size} value_size: {value_size}",
                header.capacity
            );
        }

        // Keep track of the first tombstone found, if any
        let mut first_tombstone_index: Option<usize> = None;

        // Linear probing loop with max distance
        for _ in 0..Self::MAX_PROBE_DISTANCE {
            let bucket_start_ptr = buckets_ptr.add(index * bucket_size);
            let status_ptr = bucket_start_ptr;

            let status = ptr::read(status_ptr);
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
                ptr::copy_nonoverlapping(value_ptr, target_value_ptr, value_size);
                #[cfg(feature = "debug_vm")]
                {
                    eprintln!("empty bucket just overwriting index {index}");
                }

                return true;
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
                    ptr::copy_nonoverlapping(value_ptr, value_dest_ptr, value_size);
                    return true;
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
            ptr::copy_nonoverlapping(value_ptr, target_value_ptr, value_size);

            return true;
        }

        #[cfg(feature = "debug_vm")]
        {
            eprintln!("MAP IS FULL, CAN NOT INSERT");
        }

        // If we reach here, the map is close to full or completely full and with no tombstones.
        false
    }

    // TODO: Use the default hasher for now, but maybe use a noice hash or fnv-1a or something
    fn calculate_hash(key_bytes: &[u8]) -> u64 {
        let mut hasher = DefaultHasher::new();
        hasher.write(key_bytes);
        let unique = hasher.finish();
        eprintln!("hash: {unique:08X}");
        unique
    }

    pub fn execute_map_open_addressing_get(
        &mut self,
        dst_value_addr: u16,
        self_map_source_addr: u16,
        key_source: u16,
    ) {
        let map_header = Self::read_heap_map_header_via_frame(&self.memory, self_map_source_addr);
        let buckets_ptr = self
            .memory
            .get_heap_const_ptr(map_header.heap_offset as usize);

        #[cfg(feature = "debug_vm")]
        {
            eprintln!(
                "lookup in bucket: {buckets_ptr:?} {:04X}",
                map_header.heap_offset,
            );
        }

        let key_source_ptr = self.memory.get_frame_const_ptr(key_source);
        let value_dest_ptr = self.memory.get_frame_ptr(dst_value_addr);
        unsafe {
            let worked = Self::lookup_open_addressing(
                buckets_ptr,
                &map_header,
                key_source_ptr,
                value_dest_ptr,
            );
            assert!(worked);
        }
    }

    pub fn execute_map_open_addressing_has(
        &mut self,
        self_const_map_source_addr: u16,
        key_source: u16,
    ) {
        let map_header =
            Self::read_heap_map_header_via_frame(&self.memory, self_const_map_source_addr);
        let buckets_ptr = self.memory.get_heap_ptr(map_header.heap_offset as usize);
        let key_source_ptr = self.memory.get_frame_const_ptr(key_source);
        unsafe {
            let found = Self::has_open_addressing(buckets_ptr, &map_header, key_source_ptr);
            self.flags.z = found;
        }

        #[cfg(feature = "debug_vm")]
        {
            eprintln!("map has: {}", self.flags.z);
        }
    }

    ///
    /// Returns `true` if the key is found within the maximum probe distance,
    /// `false` otherwise.
    unsafe fn has_open_addressing(
        buckets_ptr: *const u8,
        header: &MapHeader,
        key_ptr: *const u8,
    ) -> bool {
        let capacity = header.capacity as usize;
        let key_size = header.key_size as usize;
        let value_size = header.value_size as usize;
        debug_assert_ne!(key_size, 0);
        debug_assert_ne!(value_size, 0);

        // Calculate bucket layout sizes
        let status_size: usize = 1;
        let bucket_size: usize = status_size + key_size + value_size;

        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = Self::calculate_hash(key_slice);

        // Use bitwise AND for modulo (capacity is always a power of two)
        let mut index = (hash as usize) & (capacity - 1);

        #[cfg(feature = "debug_vm")]
        {
            eprintln!(
                "checks if has item with hash: {hash:08X} start index: {index} capacity: {capacity} key_size:{key_size} value_size: {value_size}"
            );
        }

        // Linear probing loop with max distance
        for _ in 0..Self::MAX_PROBE_DISTANCE {
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

    pub fn execute_map_open_addressing_set(
        &mut self,
        self_mut_map_source_addr: u16,
        key_source: u16,
        src_value_addr: u16,
    ) {
        let map_header =
            Self::read_heap_map_header_via_frame(&self.memory, self_mut_map_source_addr);
        let buckets_ptr = self.memory.get_heap_ptr(map_header.heap_offset as usize);
        let key_source_ptr = self.memory.get_frame_const_ptr(key_source);
        let value_source_ptr = self.memory.get_frame_const_ptr(src_value_addr);

        #[cfg(feature = "debug_vm")]
        {
            eprintln!(
                "setting in bucket: {buckets_ptr:?} {:04X}",
                map_header.heap_offset,
            );
        }

        unsafe {
            let worked = Self::insert_open_addressing(
                buckets_ptr,
                &map_header,
                key_source_ptr,
                value_source_ptr,
            );
            assert!(worked);
        }
    }

    /// Looks up a key in the hash map using open addressing and linear probing.
    ///
    /// If the key is found, its corresponding value is copied into `value_out_ptr`
    /// and the function returns `true`. Otherwise, it returns `false`.
    ///
    unsafe fn lookup_open_addressing(
        buckets_ptr: *const u8,
        header: &MapHeader,
        key_ptr: *const u8,
        value_out_ptr: *mut u8,
    ) -> bool {
        let capacity = header.capacity as usize;
        let key_size = header.key_size as usize;
        let value_size = header.value_size as usize;

        // Calculate bucket layout sizes (same as insert)
        let status_size: usize = 1;
        let bucket_size: usize = status_size + key_size + value_size;

        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = Self::calculate_hash(key_slice);

        // Use bitwise AND for modulo (capacity is always a power of two)
        let mut index = (hash as usize) & (capacity - 1);

        #[cfg(feature = "debug_vm")]
        {
            eprintln!("calculated search hash {hash:08X} starting at {index}");
        }

        // Linear probing loop with max distance
        for _ in 0..Self::MAX_PROBE_DISTANCE {
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
                return false;
            } else if status == Self::BUCKET_OCCUPIED {
                // Slot is occupied, check if the keys match.
                let existing_key_ptr = bucket_start_ptr.add(status_size);
                let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                if key_slice == existing_key_slice {
                    #[cfg(feature = "debug_vm")]
                    {
                        eprintln!(
                            "matching key {key_slice:?} {existing_key_slice:?}. copying to value out"
                        );
                    }
                    // Keys match! Copy the value and return true.
                    let value_src_ptr = existing_key_ptr.add(key_size);
                    ptr::copy_nonoverlapping(value_src_ptr, value_out_ptr, value_size);
                    return true;
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
            eprintln!("lookup failed to find anything after max probe distance");
        }
        false
    }

    /*
    fn map_remove_open_addressing(&mut self, dst_offset: u16, key_offset: u16) {
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);

        let capacity = unsafe { *dst_ptr.add(1) };
        let buckets_ptr_addr = unsafe { *dst_ptr.add(2) };
        let key_size = unsafe { *dst_ptr.add(3) };
        let value_size = unsafe { *dst_ptr.add(4) };

        let buckets_ptr = self.ptr_at_u8(buckets_ptr_addr as usize);

        let key_ptr = self.ptr_at_u8(self.frame_offset + key_offset as usize);

        let hash = Self::hash_bytes(key_ptr, key_size as usize);
        let mut bucket_idx = hash % capacity;

        for i in 0..Self::MAX_PROBES {
            let entry_ptr = unsafe {
                buckets_ptr.add(bucket_idx as usize * (1 + key_size as usize + value_size as usize))
            };

            let state = unsafe { *entry_ptr };

            if state == Self::EMPTY {
                return;
            }

            let entry_key_ptr = unsafe { entry_ptr.add(1) };
            if Self::keys_equal(key_ptr, entry_key_ptr, key_size as usize) {
                unsafe {
                    *entry_ptr = Self::DELETED;
                }

                unsafe {
                    *dst_ptr -= 1;
                }

                return;
            }

            // Probe to the next slot
            bucket_idx = (hash + i as u16 + 1) % capacity;
        }

        panic!("could not find key to delete");
    }

     */
}
