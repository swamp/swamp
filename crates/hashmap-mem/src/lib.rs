use fxhash::FxHasher64;
use std::cmp::{max, min};
use std::hash::Hasher;
use std::ptr::null_mut;
use std::{ptr, slice};

fn calculate_hash(key_bytes: &[u8]) -> u64 {
    let mut hasher = FxHasher64::default();
    hasher.write(key_bytes);
    hasher.finish()
}

pub const MAX_PROBES: usize = 8;
pub const BUCKET_EMPTY: u8 = 0; // Must be zero, do not change
pub const BUCKET_TOMBSTONE: u8 = 1;
pub const BUCKET_OCCUPIED: u8 = 2;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct MapHeader {
    /// Do not change the order of the fields!
    ///
    /// Keep the capacity field at the start of the header for consistency across all
    /// container types. Placing it first simplifies copy operations: we can verify
    /// and preserve capacity before copying the remainder of the header in one contiguous operation.
    pub capacity: u16,

    /// Number of live (active) elements currently stored in the collection.
    ///
    /// Always located at offset 2, enabling:
    /// - **Logical size**: Represents the number of valid elements in use.
    /// - **Bounds checking**: Index and assignment checks (`0 <= idx < element_count`)
    ///   can load this field in a single instruction.
    /// - **Iteration**: Iterators read this field to determine the end of the collection.
    /// - **ABI stability**: External tools, debuggers, and serializers can consistently locate
    ///   `capacity` and `element_count` across all container types.
    pub element_count: u16,

    pub key_size: u16,
    pub value_size: u16,    // Size of the value part
    pub logical_limit: u16, // The logical limit set by the user. Capacity is always equal or greater than this value.
    pub bucket_size: u16,   // Size of the whole bucket, including status, key, and value.
    pub value_offset: u16,  // Offset from bucket start to value.
    pub key_offset: u8,     // Offset from the bucket start to the key.
    pub padding2: u8,
}

pub struct MapInit {
    pub key_size: u16,
    pub key_alignment: u8,
    pub value_size: u16,
    pub value_alignment: u8,
    pub capacity: u16,
    pub logical_limit: u16,
}

pub struct BucketLayout {
    pub bucket_size: u16,
    pub key_offset: u8, // typically 1-4
    pub value_offset: u16,
}

#[inline]
#[must_use] pub fn calculate_bucket_layout(
    key_size: u16,
    key_alignment: u8,
    value_size: u16,
    value_alignment: u8,
) -> BucketLayout {
    let status_size: u16 = 1;
    let mut current_offset = status_size;

    let key_align = u16::from(key_alignment);
    let key_offset = current_offset.div_ceil(key_align) * key_align;

    current_offset = key_offset + key_size;

    let value_align = u16::from(value_alignment);
    let value_offset = current_offset.div_ceil(value_align) * value_align;

    current_offset = value_offset + value_size;

    let bucket_content_alignment = max(key_align, value_align);
    let bucket_size = current_offset.div_ceil(bucket_content_alignment)
        * bucket_content_alignment;

    BucketLayout {
        bucket_size,
        key_offset: key_offset as u8,
        value_offset,
    }
}
const MAP_BUCKETS_OFFSET: usize = size_of::<MapHeader>();

pub unsafe fn init(map_base: *mut u8, config: &MapInit) {
    let map_header = map_base.cast::<MapHeader>();
    let layout = calculate_bucket_layout(
        config.key_size,
        config.key_alignment,
        config.value_size,
        config.value_alignment,
    );
    unsafe {
        (*map_header).capacity = config.capacity;
        (*map_header).logical_limit = config.logical_limit;
        (*map_header).key_size = config.key_size;
        (*map_header).value_size = config.value_size;
        (*map_header).bucket_size = layout.bucket_size;
        (*map_header).key_offset = layout.key_offset;
        (*map_header).value_offset = layout.value_offset;
        (*map_header).element_count = 0;
    }

    let buckets_start_ptr = unsafe { map_base.add(MAP_BUCKETS_OFFSET) };

    let capacity = unsafe { u32::from((*map_header).capacity) };
    let bucket_size = u32::from(layout.bucket_size);

    unsafe {
        // TODO: Not really needed if we assumed zeroed memory
        for i in 0..capacity {
            let status_ptr = buckets_start_ptr.add((i * bucket_size) as usize);
            *status_ptr = BUCKET_EMPTY;
        }
    }
}

const MAX_PROBE_DISTANCE: usize = 32; // TODO: tweak this, only guessing for now
#[must_use] pub const fn is_power_of_two(n: usize) -> bool {
    n > 0 && n.is_power_of_two()
}

// Looks up the key in the map. If it can not find the key, it reserves an entry and
// returns the offset to it.
// Uses linear probing with a max distance. Hopefully it will work out
// otherwise we have to go through the capacity.
// https://en.wikipedia.org/wiki/Open_addressing
/// # Safety
///
#[allow(clippy::too_many_lines)]
pub unsafe fn get_or_reserve_entry(base_ptr: *mut u8, key_ptr: *const u8) -> *mut u8 {
    unsafe {
        let header = base_ptr.cast::<MapHeader>();
        let capacity = (*header).capacity as usize;
        let key_size = (*header).key_size as usize;
        let value_size = (*header).value_size as usize;
        debug_assert_ne!(key_size, 0);
        debug_assert_ne!(value_size, 0);
        debug_assert_ne!(capacity, 0);
        debug_assert!(is_power_of_two(capacity));

        let buckets_ptr = base_ptr.add(MAP_BUCKETS_OFFSET);

        let key_offset: usize = (*header).key_offset.into();
        let value_offset: usize = (*header).value_offset.into();
        let bucket_size = (*header).bucket_size as usize;

        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = calculate_hash(key_slice);

        // Use bitwise AND for modulo (capacity is always a power of two)
        let mut index = (hash as usize) & (capacity - 1);
        #[cfg(feature = "debug_vm")]
        if log {
            eprintln!(
                "map_get_or_reserve_entry: wants to insert hash: {hash:016X} starting at: {index} capacity: {}, len: {}, logical_limit:{} key_size: {key_size} value_size: {value_size}",
                (*header).capacity,
                (*header).element_count,
                (*header).logical_limit,
            );
        }

        // Keep track of the first tombstone found, if any
        let mut first_tombstone_index: Option<usize> = None;
        let max_probe_count = min(capacity, MAX_PROBE_DISTANCE);

        // Linear probing loop with max distance
        for _ in 0..max_probe_count {
            let bucket_start_ptr = buckets_ptr.add(index * bucket_size);
            let status_ptr = bucket_start_ptr;

            let status = ptr::read(status_ptr);
            #[cfg(feature = "debug_vm")]
            if log {
                eprintln!(
                    "map_get_or_reserve_entry. checking index: {index}, status {status}, bucket_size {bucket_size}"
                );
            }
            if status == BUCKET_EMPTY {
                // Found an empty slot. Key is not present.
                // use tombstone if found on the way. it is closer to the initial index from hash.
                let insertion_index = first_tombstone_index.unwrap_or(index);

                // Calculate pointers for the insertion spot.
                let target_bucket_ptr = buckets_ptr.add(insertion_index * bucket_size);
                let target_status_ptr = target_bucket_ptr;
                let target_key_ptr = target_bucket_ptr.add(key_offset);

                // Use the pre-calculated value_offset instead of key_ptr + key_size
                let target_value_ptr = target_bucket_ptr.add(value_offset);

                // Write status, key, and value
                ptr::write(target_status_ptr, BUCKET_OCCUPIED);
                (*header).element_count += 1;
                ptr::copy_nonoverlapping(key_ptr, target_key_ptr, key_size);
                #[cfg(feature = "debug_vm")]
                if log {
                    eprintln!(
                        "map_get_or_reserve_entry. found empty bucket, write to index {index}. set bucket to occupied"
                    );
                }

                return target_value_ptr;
            } else if status == BUCKET_OCCUPIED {
                // Slot is occupied, check if keys match.
                let existing_key_ptr = bucket_start_ptr.add(key_offset);
                let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                if key_slice == existing_key_slice {
                    #[cfg(feature = "debug_vm")]
                    if log {
                        eprintln!("keys matched, so we found it. overwriting index {index}");
                    }

                    // Use the pre-calculated value_offset instead of key_ptr + key_size
                    let value_dest_ptr = bucket_start_ptr.add(value_offset);

                    return value_dest_ptr;
                }
                #[cfg(feature = "debug_vm")]
                if log {
                    eprintln!("map_get_or_reserve_entry. keys didn't match, so continue searching");
                }
                // Keys don't match (collision), just continue the loop
            } else if status == BUCKET_TOMBSTONE {
                // Found a tombstone. Record its index if it's the first (earliest) one.
                if first_tombstone_index.is_none() {
                    first_tombstone_index = Some(index);
                }
                #[cfg(feature = "debug_vm")]
                if log {
                    eprintln!("map_get_or_reserve_entry. found tombstone but continue searching");
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
            let target_key_ptr = target_bucket_ptr.add(key_offset);

            // Use the pre-calculated value_offset instead of key_ptr + key_size
            let target_value_ptr = target_bucket_ptr.add(value_offset);

            #[cfg(feature = "debug_vm")]
            if log {
                eprintln!(
                    "map_get_or_reserve_entry. found tombstone in the end, overwriting with occupied"
                );
            }
            // Write status, key, and value
            ptr::write(target_status_ptr, BUCKET_OCCUPIED);
            (*header).element_count += 1;
            ptr::copy_nonoverlapping(key_ptr, target_key_ptr, key_size);

            return target_value_ptr;
        }

        #[cfg(feature = "debug_vm")]
        if log {
            eprintln!("map_get_or_reserve_entry. MAP IS FULL, CAN NOT INSERT");
        }

        // If we reach here, the map is close to full or completely full and with no tombstones.
        null_mut()
    }
}

#[must_use] pub unsafe fn has(base_ptr: *const u8, key_ptr: *const u8) -> bool {
    unsafe {
        let header = base_ptr.cast::<MapHeader>();
        let capacity = (*header).capacity as usize;
        let key_size = (*header).key_size as usize;
        let value_size = (*header).value_size as usize;
        debug_assert_ne!(key_size, 0);
        debug_assert_ne!(value_size, 0);
        debug_assert_ne!(capacity, 0);
        debug_assert!(is_power_of_two(capacity));

        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = calculate_hash(key_slice);

        // Calculate bucket layout sizes
        let key_offset: usize = (*header).key_offset as usize;
        let bucket_size: usize = (*header).bucket_size as usize;

        let buckets_ptr = base_ptr.add(MAP_BUCKETS_OFFSET);

        // Use bitwise AND for modulo (capacity is always a power of two)
        let mut index = (hash as usize) & (capacity - 1);

        #[cfg(feature = "debug_vm")]
        if log {
            eprintln!(
                "checks if has item with hash: {hash:08X} start index: {index} capacity: {capacity} key_size:{key_size} value_size: {value_size}"
            );
        }
        let max_probe_count = min(capacity, MAX_PROBE_DISTANCE);

        // Linear probing loop with max distance
        for _ in 0..max_probe_count {
            let bucket_start_ptr = buckets_ptr.add(index * bucket_size);
            let status_ptr = bucket_start_ptr; // Status is at the start

            let status = ptr::read(status_ptr);

            if status == BUCKET_EMPTY {
                // Found an empty slot. The key cannot be present.
                return false;
            } else if status == BUCKET_OCCUPIED {
                // Slot is occupied, check if the keys match.
                let existing_key_ptr = bucket_start_ptr.add(key_offset);
                let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                if key_slice == existing_key_slice {
                    // Keys match! The key exists.
                    return true;
                }
                // Keys don't match (collision), continue probing.
            } else if status == BUCKET_TOMBSTONE {
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

pub unsafe fn lookup(base_ptr: *mut u8, key_ptr: *const u8) -> *mut u8 {
    unsafe {
        let header = base_ptr.cast::<MapHeader>();
        let capacity = (*header).capacity as usize;
        debug_assert_ne!(capacity, 0);
        debug_assert!(is_power_of_two(capacity));
        let key_size = (*header).key_size as usize;
        debug_assert_ne!(key_size, 0);
        let value_size = (*header).value_size as usize;
        debug_assert_ne!(value_size, 0);

        // Calculate bucket layout sizes (same as insert)
        let key_offset: usize = (*header).key_offset as usize;
        let bucket_size: usize = (*header).bucket_size as usize;
        let buckets_ptr = base_ptr.add(MAP_BUCKETS_OFFSET);

        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = calculate_hash(key_slice);

        // Use bitwise AND for modulo (capacity is always a power of two)
        let mut index = (hash as usize) & (capacity - 1);

        #[cfg(feature = "debug_vm")]
        if log {
            eprintln!(
                "map_lookup_existing_entry: calculated search hash {hash:16X} starting at {index}"
            );
        }

        let max_probe_count = min(capacity, MAX_PROBE_DISTANCE);
        // Linear probing loop with max distance
        for _ in 0..max_probe_count {
            let bucket_start_ptr = buckets_ptr.add(index * bucket_size);
            let status_ptr = bucket_start_ptr; // Status is at the start

            let status = ptr::read(status_ptr);

            if status == BUCKET_EMPTY {
                // Found an empty slot. The key cannot be present further down
                // the probe sequence, because insertion would have stopped here.
                #[cfg(feature = "debug_vm")]
                if log {
                    eprintln!(
                        "map_lookup_existing_entry: found empty bucket, so gave up on searching"
                    );
                }
                return null_mut();
            } else if status == BUCKET_OCCUPIED {
                // Slot is occupied, check if the keys match.
                let existing_key_ptr = bucket_start_ptr.add(key_offset);
                let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                if key_slice == existing_key_slice {
                    #[cfg(feature = "debug_vm")]
                    if log {
                        eprintln!(
                            "map_lookup_existing_entry: matching new key {key_slice:?} with existing key {existing_key_slice:?}. returning this existing entry at {index} value_offset:{}",
                            (*header).value_offset
                        );
                    }
                    // Keys match! return the pointer.
                    let value_src_ptr = bucket_start_ptr.add((*header).value_offset as usize);

                    return value_src_ptr;
                }
                // Keys don't match (collision), continue probing.
            } else if status == BUCKET_TOMBSTONE {
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
        if log {
            eprintln!("map_lookup_existing_entry: lookup failed to find any matches, returning 0");
        }
        null_mut()
    }
}

pub unsafe fn remove(base_ptr: *mut u8, key_ptr: *const u8) -> bool {
    unsafe {
        let header = base_ptr.cast::<MapHeader>();
        let capacity = (*header).capacity as usize;
        debug_assert_ne!(capacity, 0);
        debug_assert!(is_power_of_two(capacity));

        let key_size = (*header).key_size as usize;
        debug_assert_ne!(key_size, 0);

        let value_size = (*header).value_size as usize;
        debug_assert_ne!(value_size, 0);

        let buckets_ptr_mut = base_ptr.add(MAP_BUCKETS_OFFSET);
        let key_offset: usize = (*header).key_offset as usize;
        let bucket_size: usize = (*header).bucket_size as usize;

        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = calculate_hash(key_slice);

        // Use bitwise AND for modulo (capacity is always a power of two)
        let mut index = (hash as usize) & (capacity - 1);

        #[cfg(feature = "debug_vm")]
        if log {
            eprintln!("Attempting to remove item with hash: {hash:08X} starting at index: {index}");
        }

        let max_probe_count = min(capacity, MAX_PROBE_DISTANCE);

        for _ in 0..max_probe_count {
            let bucket_start_ptr_mut = buckets_ptr_mut.add(index * bucket_size);
            let status_ptr_mut = bucket_start_ptr_mut; // Status is at the start

            let status = ptr::read(status_ptr_mut);

            if status == BUCKET_EMPTY {
                // Found an empty slot. The key cannot be present further down
                // the probe sequence, because insertion would have stopped here.
                #[cfg(feature = "debug_vm")]
                if log {
                    eprintln!("Found empty bucket, key not found for removal.");
                }
                return false;
            } else if status == BUCKET_OCCUPIED {
                let existing_key_ptr = bucket_start_ptr_mut.add(key_offset);
                let existing_key_slice = slice::from_raw_parts(existing_key_ptr, key_size);

                if key_slice == existing_key_slice {
                    // We found it! Mark this bucket as a tombstone.
                    ptr::write(status_ptr_mut, BUCKET_TOMBSTONE);
                    (*header).element_count -= 1;
                    #[cfg(feature = "debug_vm")]
                    if log {
                        eprintln!("Successfully removed (tombstone) entry at index: {index}");
                    }
                    return true;
                }
            } else if status == BUCKET_TOMBSTONE {
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

pub unsafe fn overwrite(target_base: *mut u8, source: *const u8) -> bool {
    unsafe {
        let target_map_header = target_base.cast::<MapHeader>();
        let source_map_header = source.cast::<MapHeader>();
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "map_overwrite: target_capacity:{} target_logical_limit:{}, source_capacity:{}, source_element_count:{}",
                (*target_map_header).capacity,
                (*target_map_header).logical_limit,
                (*source_map_header).capacity,
                (*source_map_header).element_count
            );
        }

        if (*target_map_header).logical_limit < (*source_map_header).element_count {
            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                eprintln!(
                    "map_overwrite: target_capacity:{} source_capacity:{}, source_element_count:{}",
                    (*target_map_header).capacity,
                    (*source_map_header).capacity,
                    (*source_map_header).element_count
                );
            }

            return false;
        }

        let source_buckets_ptr = source.add(MAP_BUCKETS_OFFSET);

        let target_buckets_ptr = target_base.add(MAP_BUCKETS_OFFSET);

        let bucket_size = (*source_map_header).bucket_size as usize;
        let key_offset = (*source_map_header).key_offset as usize;
        let value_offset = (*source_map_header).value_offset as usize;

        let target_bucket_size = (*target_map_header).bucket_size as usize;
        debug_assert_eq!(bucket_size, target_bucket_size);

        let target_capacity = (*target_map_header).capacity as usize;

        // For each bucket in source
        for i in 0..(*source_map_header).capacity as usize {
            let source_bucket_ptr = source_buckets_ptr.add(i * bucket_size);

            let status = ptr::read(source_bucket_ptr);

            if status == BUCKET_OCCUPIED {
                let source_key_ptr = source_bucket_ptr.add(key_offset);
                let source_value_ptr = source_bucket_ptr.add(value_offset);

                let target_value_ptr = get_or_reserve_entry(target_base, source_key_ptr);

                if target_value_ptr.is_null() {
                    return false;
                }

                let value_size = (*source_map_header).value_size as usize;
                ptr::copy_nonoverlapping(source_value_ptr, target_value_ptr, value_size);
            }
        }

        debug_assert_eq!(
            (*target_map_header).element_count,
            (*source_map_header).element_count,
            "Target map should have same number of elements as source after copy"
        );
    }
    true
}

pub unsafe fn find_next_valid_entry(base: *mut u8, start_index: u16) -> (*const u8, *mut u8, u16) { unsafe {
    let map_header = base as *const MapHeader;

    let bucket_size = (*map_header).bucket_size as usize;

    let buckets_start = base.add(MAP_BUCKETS_OFFSET);

    let mut index = start_index;

    while index < (*map_header).capacity {
        // Calculate the address of the current entry
        let entry_ptr = buckets_start.add(index as usize * bucket_size);

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "map_iter_next_pair: iterating to bucket_addr:{element_start_address_including_status:X}, index:{index}, status:{status}"
            );
        }

        if *entry_ptr == BUCKET_OCCUPIED {
            let key_addr = entry_ptr.add((*map_header).key_offset as usize);
            let value_addr = entry_ptr.add((*map_header).value_offset as usize);

            #[cfg(feature = "debug_vm")]
            if self.debug_operations_enabled {
                eprintln!(
                    "map_iter_next: element_addr 0x{key_addr:X} to r{target_key_reg} and 0x{value_addr:X} to r{target_value_reg}"
                );
            }
            return (key_addr, value_addr, index);
        }
        index += 1;
    }

    (null_mut(), null_mut(), 0xffff)
}}
