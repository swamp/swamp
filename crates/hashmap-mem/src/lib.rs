use fxhash::FxHasher64;
use std::cmp::{max, min};
use std::hash::Hasher;
use std::mem::size_of;
use std::ops::Not;
use std::{ptr, slice};

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BucketStatus {
    Empty = 0, // Must be zero, do not change!
    Tombstone = 1,
    Occupied = 2,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct MapHeader {
    // Do not change the order of the fields!
    pub capacity: u16,
    pub element_count: u16,
    pub key_size: u16,
    pub value_size: u16,
    pub logical_limit: u16,
    pub bucket_size: u16,
    pub value_offset: u16,
    pub key_offset: u8,
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

#[derive(Clone, Copy, Debug)]
pub struct BucketLayout {
    pub bucket_size: u16,
    pub key_offset: u8,
    pub value_offset: u16,
}

const MAP_BUCKETS_OFFSET: usize = size_of::<MapHeader>();
const MAX_PROBE_DISTANCE: usize = 32;

#[inline]
fn calculate_hash_bytes(key_bytes: &[u8]) -> u64 {
    let mut hasher = FxHasher64::default();
    hasher.write(key_bytes);
    hasher.finish()
}


/// Calculate memory layout for a map bucket
#[inline]
#[must_use]
pub fn calculate_bucket_layout(
    key_size: u16,
    key_alignment: u8,
    value_size: u16,
    value_alignment: u8,
) -> BucketLayout {
    let status_size: u16 = 1;
    let mut current_offset = status_size;

    // Align key
    let key_align = u16::from(key_alignment);
    let key_offset = (current_offset + key_align - 1) & !(key_align - 1);
    current_offset = key_offset + key_size;

    // Align value
    let value_align = u16::from(value_alignment);
    let value_offset = (current_offset + value_align - 1) & !(value_align - 1);
    current_offset = value_offset + value_size;

    // Calculate final bucket size with proper alignment
    let bucket_content_alignment = max(key_align, value_align);
    let bucket_size =
        (current_offset + bucket_content_alignment - 1) & !(bucket_content_alignment - 1);

    BucketLayout {
        bucket_size,
        key_offset: key_offset as u8,
        value_offset,
    }
}

/// Initialize a new hash map in pre-allocated memory
///
/// # Safety
///
/// - `map_base` must point to valid, properly aligned memory of sufficient size
/// - The memory must remain valid for the lifetime of the map
pub unsafe fn init(map_base: *mut u8, config: &MapInit) {
    unsafe {
        debug_assert!(
            config.capacity.is_power_of_two(),
            "Capacity must be a power of two"
        );

        let map_header = map_base.cast::<MapHeader>();
        let layout = calculate_bucket_layout(
            config.key_size,
            config.key_alignment,
            config.value_size,
            config.value_alignment,
        );

        // Initialize header
        unsafe {
            ptr::write(
                map_header,
                MapHeader {
                    capacity: config.capacity,
                    logical_limit: config.logical_limit,
                    key_size: config.key_size,
                    value_size: config.value_size,
                    bucket_size: layout.bucket_size,
                    key_offset: layout.key_offset,
                    value_offset: layout.value_offset,
                    element_count: 0,
                    padding2: 0,
                },
            );
        }

        // Initialize buckets to empty
        let buckets_start_ptr = map_base.add(MAP_BUCKETS_OFFSET);
        let capacity = usize::from(config.capacity);
        let bucket_size = usize::from(layout.bucket_size);

        // Zero out all bucket status bytes (Empty = 0)
        for i in 0..capacity {
            unsafe {
                ptr::write(
                    buckets_start_ptr.add(i * bucket_size),
                    BucketStatus::Empty as u8,
                );
            }
        }
    }
}

/// Fast key comparison helper
#[inline]
unsafe fn matches_key(a: *const u8, b: *const u8, len: usize) -> bool {
    unsafe {
        // For small keys (<=16 bytes), do direct comparison - avoids function call overhead
        if len <= 16 {
            match len {
                0 => true,
                1 => *a == *b,
                2 => *a.cast::<u16>() == *b.cast::<u16>(),
                4 => *a.cast::<u32>() == *b.cast::<u32>(),
                8 => *a.cast::<u64>() == *b.cast::<u64>(),
                // For other small sizes, compare bytes directly
                _ => {
                    for i in 0..len {
                        if *a.add(i) != *b.add(i) {
                            return false;
                        }
                    }
                    true
                }
            }
        } else {
            // For larger keys, use memcmp equivalent
            slice::from_raw_parts(a, len) == slice::from_raw_parts(b, len)
        }
    }
}

/// Get or reserve an entry in the map
///
/// # Safety
///
/// - `base_ptr` must point to a valid initialized map
/// - `key_ptr` must point to a valid key of the size specified in the map header
///
/// # Returns
///
/// Pointer to the value location, or null if the map is full
#[inline]
pub unsafe fn get_or_reserve_entry(base_ptr: *mut u8, key_ptr: *const u8) -> *mut u8 {
    unsafe {
        let header = &*base_ptr.cast::<MapHeader>();

        // Validate parameters
        let capacity = header.capacity as usize;
        let key_size = header.key_size as usize;
        let bucket_size = header.bucket_size as usize;
        let key_offset = header.key_offset as usize;
        let value_offset = header.value_offset as usize;

        debug_assert_ne!(key_size, 0, "Key size cannot be zero");
        debug_assert_ne!(capacity, 0, "Capacity cannot be zero");
        debug_assert!(capacity.is_power_of_two(), "Capacity must be a power of two");

        let buckets_ptr = base_ptr.add(MAP_BUCKETS_OFFSET);
        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = calculate_hash_bytes(key_slice);

        // Initial probe position
        let mut index = hash as usize & (capacity - 1);

        // Track first tombstone for potential reuse
        let mut first_tombstone = None;
        let probe_limit = min(capacity, MAX_PROBE_DISTANCE);

        for _ in 0..probe_limit {
            let bucket_ptr = buckets_ptr.add(index * bucket_size);
            let status = *bucket_ptr;

            match status {
                status if status == BucketStatus::Empty as u8 => {
                    // TODO: Maybe go back to BucketStatus as constants instead, this feel a bit awkward
                    // Use tombstone if found, otherwise use current empty slot
                    let insert_index = first_tombstone.unwrap_or(index);
                    let target_bucket = buckets_ptr.add(insert_index * bucket_size);

                    // Mark as occupied and copy key
                    *target_bucket = BucketStatus::Occupied as u8;
                    let target_key_ptr = target_bucket.add(key_offset);
                    ptr::copy_nonoverlapping(key_ptr, target_key_ptr, key_size);

                    // Update element count
                    let header_mut = &mut *base_ptr.cast::<MapHeader>();
                    header_mut.element_count += 1;

                    return target_bucket.add(value_offset);
                }
                status if status == BucketStatus::Occupied as u8 => {
                    // Check if keys match
                    let existing_key_ptr = bucket_ptr.add(key_offset);
                    if matches_key(existing_key_ptr, key_ptr, key_size) {
                        return bucket_ptr.add(value_offset);
                    }
                }
                status if status == BucketStatus::Tombstone as u8 => {
                    // Remember first tombstone for potential reuse
                    if first_tombstone.is_none() {
                        first_tombstone = Some(index);
                    }
                }
                _ => unreachable!(),
            }

            // Linear probing with wraparound using bitmask
            index = (index + 1) & (capacity - 1);
        }

        // If we found a tombstone during probing, use it
        if let Some(tombstone_index) = first_tombstone {
            let target_bucket = buckets_ptr.add(tombstone_index * bucket_size);

            // Mark as occupied and copy key
            *target_bucket = BucketStatus::Occupied as u8;
            let target_key_ptr = target_bucket.add(key_offset);
            ptr::copy_nonoverlapping(key_ptr, target_key_ptr, key_size);

            // Update element count
            let header_mut = &mut *base_ptr.cast::<MapHeader>();
            header_mut.element_count += 1;

            return target_bucket.add(value_offset);
        }

        // Map is full or probe limit exceeded
        ptr::null_mut()
    }
}

/// Check if a key exists in the map
///
/// # Safety
///
/// - `base_ptr` must point to a valid initialized map
/// - `key_ptr` must point to a valid key of the size specified in the map header
#[inline]
#[must_use]
pub unsafe fn has(base_ptr: *const u8, key_ptr: *const u8) -> bool {
    unsafe {
        lookup(base_ptr.cast_mut(), key_ptr).is_null().not()
    }
}

/// Lookup an existing entry in the map
///
/// # Safety
///
/// - `base_ptr` must point to a valid initialized map
/// - `key_ptr` must point to a valid key of the size specified in the map header
///
/// # Returns
///
/// Pointer to the found value, or null if not found
#[inline]
pub unsafe fn lookup(base_ptr: *mut u8, key_ptr: *const u8) -> *mut u8 {
    unsafe {
        let header = &*base_ptr.cast::<MapHeader>();

        let capacity = header.capacity as usize;
        let key_size = header.key_size as usize;
        let bucket_size = header.bucket_size as usize;
        let key_offset = header.key_offset as usize;
        let value_offset = header.value_offset as usize;

        debug_assert_ne!(key_size, 0, "Key size cannot be zero");
        debug_assert_ne!(capacity, 0, "Capacity cannot be zero");
        debug_assert!(capacity.is_power_of_two(), "Capacity must be a power of two");

        let buckets_ptr = base_ptr.add(MAP_BUCKETS_OFFSET);
        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = calculate_hash_bytes(key_slice);

        // Initial probe position
        let mut index = hash as usize & (capacity - 1);
        let probe_limit = min(capacity, MAX_PROBE_DISTANCE);

        for _ in 0..probe_limit {
            let bucket_ptr = buckets_ptr.add(index * bucket_size);
            let status = *bucket_ptr;

            match status {
                status if status == BucketStatus::Empty as u8 => {
                    // TODO: Maybe go back to constant
                    // Empty slot means the key is not in the map
                    return ptr::null_mut();
                }
                status if status == BucketStatus::Occupied as u8 => {
                    // Check if keys match
                    let existing_key_ptr = bucket_ptr.add(key_offset);
                    if matches_key(existing_key_ptr, key_ptr, key_size) {
                        return bucket_ptr.add(value_offset);
                    }
                }
                _ => {} // Continue probing for tombstones
            }

            index = (index + 1) & (capacity - 1);
        }

        // Key not found within probe limit
        ptr::null_mut()
    }
}

/// Remove an entry from the map
///
/// # Safety
///
/// - `base_ptr` must point to a valid initialized map
/// - `key_ptr` must point to a valid key of the size specified in the map header
///
/// # Returns
///
/// `true` if the key was found and removed, `false` otherwise
#[inline]
pub unsafe fn remove(base_ptr: *mut u8, key_ptr: *const u8) -> bool {
    unsafe {
        let header = &*base_ptr.cast::<MapHeader>();

        let capacity = header.capacity as usize;
        let key_size = header.key_size as usize;
        let bucket_size = header.bucket_size as usize;
        let key_offset = header.key_offset as usize;

        debug_assert_ne!(key_size, 0, "Key size cannot be zero");
        debug_assert_ne!(capacity, 0, "Capacity cannot be zero");
        debug_assert!(capacity.is_power_of_two(), "Capacity must be a power of two");

        let buckets_ptr = base_ptr.add(MAP_BUCKETS_OFFSET);
        let key_slice = slice::from_raw_parts(key_ptr, key_size);
        let hash = calculate_hash_bytes(key_slice);

        // Initial probe position
        let mut index = hash as usize & (capacity - 1);
        let probe_limit = min(capacity, MAX_PROBE_DISTANCE);

        for _ in 0..probe_limit {
            let bucket_ptr = buckets_ptr.add(index * bucket_size);
            let status = *bucket_ptr;

            match status {
                status if status == BucketStatus::Empty as u8 => {
                    // Empty slot means the key is not in the map
                    return false;
                }
                status if status == BucketStatus::Occupied as u8 => {
                    // Check if keys match
                    let existing_key_ptr = bucket_ptr.add(key_offset);
                    if matches_key(existing_key_ptr, key_ptr, key_size) {
                        // Convert to tombstone
                        *bucket_ptr = BucketStatus::Tombstone as u8;

                        // Update count
                        let header_mut = &mut *base_ptr.cast::<MapHeader>();
                        header_mut.element_count -= 1;

                        return true;
                    }
                }
                _ => {} // Continue probing for tombstones
            }

            index = (index + 1) & (capacity - 1);
        }

        // Key not found within probe limit
        false
    }
}

/// Copy all entries from source map to target map
///
/// # Safety
///
/// - Both maps must be properly initialized with compatible layouts (capacity can differ)
/// - Target map must have sufficient capacity
///
/// # Returns
///
/// `true` if the operation succeeded, `false` if the target has insufficient capacity
#[inline]
pub unsafe fn overwrite(target_base: *mut u8, source: *const u8) -> bool {
    unsafe {
        let target_header = &mut *target_base.cast::<MapHeader>();
        let source_header = &*source.cast::<MapHeader>();

        // Check if target has enough capacity
        if target_header.logical_limit < source_header.element_count {
            return false;
        }

        // Validate compatible layouts
        debug_assert_eq!(
            target_header.bucket_size, source_header.bucket_size,
            "Incompatible bucket sizes"
        );
        debug_assert_eq!(
            target_header.key_size, source_header.key_size,
            "Incompatible key sizes"
        );
        debug_assert_eq!(
            target_header.value_size, source_header.value_size,
            "Incompatible value sizes"
        );

        let source_buckets_ptr = source.add(MAP_BUCKETS_OFFSET);
        let bucket_size = source_header.bucket_size as usize;
        let key_offset = source_header.key_offset as usize;
        let value_offset = source_header.value_offset as usize;
        let value_size = source_header.value_size as usize;

        // Copy each occupied bucket
        for i in 0..source_header.capacity as usize {
            let source_bucket = source_buckets_ptr.add(i * bucket_size);

            if *source_bucket == BucketStatus::Occupied as u8 {
                let source_key_ptr = source_bucket.add(key_offset);
                let source_value_ptr = source_bucket.add(value_offset);

                let target_value_ptr = get_or_reserve_entry(target_base, source_key_ptr);

                if target_value_ptr.is_null() {
                    return false;
                }

                ptr::copy_nonoverlapping(source_value_ptr, target_value_ptr, value_size);
            }
        }

        true
    }
}

/// Find the next valid entry in the map
///
/// # Safety
///
/// - `base` must point to a valid initialized map
///
/// # Returns
///
/// Tuple of (`key_ptr`, `value_ptr`, index) of the next valid entry,
/// or (null, null, 0xFFFF) if no more entries exist
#[inline]
pub unsafe fn find_next_valid_entry(base: *mut u8, start_index: u16) -> (*const u8, *mut u8, u16) {
    unsafe {
        let map_header = &*base.cast::<MapHeader>();
        let bucket_size = map_header.bucket_size as usize;
        let buckets_start = base.add(MAP_BUCKETS_OFFSET);
        let key_offset = map_header.key_offset as usize;
        let value_offset = map_header.value_offset as usize;

        let mut index = start_index as usize;

        while index < map_header.capacity as usize {
            let entry_ptr = buckets_start.add(index * bucket_size);

            // Properly use the enum instead of magic number
            if *entry_ptr == BucketStatus::Occupied as u8 {
                let key_addr = entry_ptr.add(key_offset);
                let value_addr = entry_ptr.add(value_offset);

                return (key_addr, value_addr, index as u16);
            }

            index += 1;
        }

        (ptr::null(), ptr::null_mut(), 0xFFFF)
    }
}
