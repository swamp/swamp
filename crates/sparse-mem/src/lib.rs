/// Memory layout for a fixed-capacity, generation-tracked sparse array in memory. Designed for the Swamp VM.
///
/// Follows Swamp VM Collection standard with capacity and element count first, 2-bytes each.
/// Aligns to a 4-byte aligned values region, since it is mainly for a 32-bit VM:
/// ```text
/// offset 0:   capacity                (u16)
/// offset 2:   element_count           (u16)
/// offset 4:   element_size            (u32)
/// offset 8:   slot_to_id              (u16[capacity])             // reverse lookup
/// offset 8+2*capacity: id_to_slot     (u16[capacity])             // forward lookup
/// offset 8+4*capacity: generation     (u16[capacity])             // bumped on alloc/remove
/// offset 8+6*capacity:     pad (u8[pad])                          // pad to 4-byte boundary
/// offset 8+6*capacity+pad: values (raw bytes)
/// ```
use std::ptr;

/// Compute total bytes needed in memory for a sparse array. Used for code generator to know
/// how much space to reserve.
#[must_use]
pub const fn layout_size(capacity: u16, element_size: u32) -> usize {
    let cap = capacity as usize;
    // slot_to_id + id_to_slot: each u16[capacity]
    let lookup = 2 * cap * size_of::<u16>();
    // generation: u16[capacity]
    let generation_size = cap * size_of::<u16>();
    // bytes before values
    let before_vals = HEADER_SIZE + lookup + generation_size;
    // pad to 8-byte alignment
    let pad = (8 - (before_vals % 8)) % 8;
    // values: raw bytes per element
    let vals = cap * element_size as usize;
    before_vals + pad + vals
}

/// Alignment requirement for the sparse array.
/// Report 4 just for the benefit of values
#[must_use]
pub const fn alignment() -> usize {
    8
}

const SLOT_OFFSET: usize = HEADER_SIZE;
const HEADER_SIZE: usize = 8;

/// Initialize the sparse array to memory specified by the raw memory pointer.
/// `base` must point to a region of at least `layout_size(capacity, element_size)` bytes.
pub unsafe fn init(base: *mut u8, capacity: u16, element_size: u32) {
    unsafe {
        ptr::write(base.cast::<u16>(), capacity);
        ptr::write(base.add(2).cast::<u16>(), 0);
        ptr::write(base.add(4).cast::<u32>(), element_size);
        let cap = capacity as usize;
        let id_offset = SLOT_OFFSET + cap * size_of::<u16>();
        let generation_offset = id_offset + cap * size_of::<u16>();
        for i in 0..cap {
            ptr::write(
                base.add(SLOT_OFFSET).cast::<u16>().add(i),
                (cap - 1 - i) as u16,
            );
        }
        for i in 0..cap {
            ptr::write(base.add(id_offset).cast::<u16>().add(i), u16::MAX);
        }
        for i in 0..cap {
            ptr::write(base.add(generation_offset).cast::<u16>().add(i), 0);
        }
    }
}

/// Allocate a new ID and generation.
pub unsafe fn allocate(base: *mut u8) -> Option<(u16, u16)> {
    unsafe {
        let capacity = *base.cast::<u16>() as usize;
        let count_ptr = base.add(2).cast::<u16>();
        let count = *count_ptr as usize;
        if count >= capacity {
            return None;
        }

        let id_offset = SLOT_OFFSET + capacity * size_of::<u16>();
        let generation_offset = id_offset + capacity * size_of::<u16>();

        let id = *base.add(SLOT_OFFSET).cast::<u16>().add(count);
        ptr::write(count_ptr, (count as u16).wrapping_add(1));
        ptr::write(
            base.add(id_offset).cast::<u16>().add(id as usize),
            count as u16,
        );

        let generation_ptr = base.add(generation_offset).cast::<u16>();
        let new_gen = generation_ptr.add(id as usize).read().wrapping_add(1);

        ptr::write(generation_ptr.add(id as usize), new_gen);

        Some((id, new_gen))
    }
}

/// Compute offset of values region (values are aligned to 8 bytes)
#[must_use]
pub const fn values_offset(base: *const u8) -> usize {
    let capacity = unsafe { *base.cast::<u16>() } as usize;
    let id_offset = SLOT_OFFSET + capacity * size_of::<u16>();
    let generation_offset = id_offset + capacity * size_of::<u16>();
    let before_values = generation_offset + capacity * size_of::<u16>();
    let padding = (8 - (before_values % 8)) % 8;
    before_values + padding
}

/// Insert raw bytes at handle id
/// # Safety
///
#[inline]
pub unsafe fn insert(base: *mut u8, id: u16, src: *const u8) -> bool {
    unsafe {
        let capacity = *base.cast::<u16>() as usize;

        // TODO: These should panic!()
        #[cfg(debug_assertions)]
        {
            // BOUNDS CHECK: Ensure id is within capacity
            if id as usize >= capacity {
                return false;
            }

            // LIVENESS CHECK: Ensure the slot is actually allocated
            // We need to check if this id has a valid slot assignment
            let id_offset = SLOT_OFFSET + capacity * size_of::<u16>();
            let slot = *base.add(id_offset).cast::<u16>().add(id as usize);
            let count = *base.add(2).cast::<u16>() as usize;

            // If slot is u16::MAX or >= count, the slot is not alive
            if slot == u16::MAX || slot as usize >= count {
                return false;
            }

            // Double-check: verify the slot_to_id mapping is consistent
            let slot_to_id_ptr = base.add(SLOT_OFFSET).cast::<u16>();
            if *slot_to_id_ptr.add(slot as usize) != id {
                return false;
            }
        }

        let element_size = *base.add(4).cast::<u32>() as usize;
        let off = values_offset(base) + id as usize * element_size;
        ptr::copy_nonoverlapping(src, base.add(off), element_size);
        true
    }
}

/// Remove by handle; frees slot and bumps generation. Returns false on mismatching generations.
/// # Safety
///
pub unsafe fn remove(base: *mut u8, id: u16, generation: u16) -> bool {
    unsafe {
        let capacity = *base.cast::<u16>() as usize;

        // TODO: These should panic!()
        #[cfg(debug_assertions)]
        {
            // BOUNDS CHECK: Ensure id is within capacity
            if id as usize >= capacity {
                return false;
            }

            if !is_alive(base, id, generation) {
                return false;
            }
        }

        let id_offset = SLOT_OFFSET + capacity * size_of::<u16>();
        let generation_offset = id_offset + capacity * size_of::<u16>();
        let count_ptr = base.add(2).cast::<u16>();
        let count = (*count_ptr) as usize;
        let last = count - 1;
        ptr::write(count_ptr, last as u16);

        let slot_ptr = base.add(SLOT_OFFSET).cast::<u16>();
        let slot_idx = *base.add(id_offset).cast::<u16>().add(id as usize) as usize;
        let last_id = *slot_ptr.add(last);
        ptr::write(slot_ptr.add(slot_idx), last_id);
        ptr::write(
            base.add(id_offset).cast::<u16>().add(last_id as usize),
            slot_idx as u16,
        );
        ptr::write(slot_ptr.add(last), id);
        ptr::write(base.add(id_offset).cast::<u16>().add(id as usize), u16::MAX);
        let gen_ptr = base.add(generation_offset).cast::<u16>();
        ptr::write(gen_ptr.add(id as usize), generation.wrapping_add(1));

        true
    }
}

/// Check handle validity
/// # Safety
///
pub unsafe fn is_alive(base: *mut u8, id: u16, generation: u16) -> bool {
    unsafe {
        let capacity = *base.cast::<u16>() as usize;

        // TODO: These should panic!()
        #[cfg(debug_assertions)]
        {
            // BOUNDS CHECK: Ensure id is within capacity
            if id as usize >= capacity {
                return false;
            }
        }

        let count = *base.add(2).cast::<u16>() as usize;
        let id_offset = SLOT_OFFSET + capacity * size_of::<u16>();
        let generation_offset = id_offset + capacity * size_of::<u16>();
        // Not only check generation, but also check that it slot is among the live ones.
        // Also makes extra check to look in the other direction as well.
        let slot = *base.add(id_offset).cast::<u16>().add(id as usize) as usize;
        let current_generation = *base.add(generation_offset).cast::<u16>().add(id as usize);
        slot < count
            && current_generation == generation
            && *base.add(SLOT_OFFSET).cast::<u16>().add(slot) == id
    }
}

/// Get a pointer to the `slot_to_id` array (reverse lookup)
/// # Safety
///
pub const unsafe fn slot_to_id_ptr(base: *mut u8) -> *mut u16 {
    unsafe { base.add(SLOT_OFFSET).cast::<u16>() }
}

#[must_use]
pub const unsafe fn generation_ptr_const(base: *const u8) -> *const u16 {
    unsafe {
        let capacity = *base.cast::<u16>() as usize;
        let id_offset = SLOT_OFFSET + capacity * size_of::<u16>();
        let generation_offset = id_offset + capacity * size_of::<u16>();

        base.add(generation_offset).cast::<u16>()
    }
}

/// Get a pointer to the `slot_to_id` array (reverse lookup)
/// # Safety
///
#[must_use]
pub const unsafe fn slot_to_id_ptr_const(base: *const u8) -> *const u16 {
    unsafe { base.add(SLOT_OFFSET).cast::<u16>() }
}

/// Get a pointer to the `id_to_slot` array (forward lookup)
/// # Safety
///
pub unsafe fn id_to_slot_ptr(base: *mut u8) -> *mut u16 {
    unsafe {
        let capacity = *base.cast::<u16>() as usize;
        base.add(SLOT_OFFSET + capacity * size_of::<u16>())
            .cast::<u16>()
    }
}

#[must_use]
pub const unsafe fn id_to_slot_ptr_const(base: *const u8) -> *const u16 {
    unsafe {
        let capacity = *base.cast::<u16>() as usize;
        base.add(SLOT_OFFSET + capacity * size_of::<u16>())
            .cast::<u16>()
    }
}

/// Get current element count
/// # Safety
///
#[must_use]
pub const unsafe fn element_count(base: *const u8) -> u16 {
    unsafe { *base.add(2).cast::<u16>() }
}

/// Get current element size
/// # Safety
#[must_use]
pub const unsafe fn element_size(base: *const u8) -> u32 {
    unsafe { *base.add(4).cast::<u32>() }
}

/// Safe wrapper for insert that validates the handle
/// Insert raw bytes at handle id, but only if the handle is valid
/// Returns true if successful, false if the handle is invalid
/// # Safety
pub unsafe fn insert_if_alive(base: *mut u8, id: u16, generation: u16, src: *const u8) -> bool {
    unsafe {
        if is_alive(base, id, generation) {
            insert(base, id, src)
        } else {
            false
        }
    }
}

/// Get value pointer for a valid handle
/// Returns None if the handle is invalid
/// # Safety
pub unsafe fn get_value_ptr(base: *mut u8, id: u16, generation: u16) -> Option<*mut u8> {
    unsafe {
        if !is_alive(base, id, generation) {
            return None;
        }

        let capacity = *base.cast::<u16>() as usize;
        if id as usize >= capacity {
            return None;
        }

        let element_size = *base.add(4).cast::<u32>() as usize;
        let off = values_offset(base) + id as usize * element_size;
        Some(base.add(off))
    }
}
