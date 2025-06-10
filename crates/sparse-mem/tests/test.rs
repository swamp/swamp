use sparse_mem::{
    alignment, allocate, element_count, init, insert, is_alive, layout_size, remove,
    slot_to_id_ptr, values_offset,
};

#[test]
fn test_layout_size_and_alignment() {
    let capacity = 3u16;
    let element_size = 2u16;
    let size = layout_size(capacity, element_size);

    let header = 3 * size_of::<u16>(); // capacity + element_count + element_size
    let lookup = 2 * capacity as usize * size_of::<u16>(); // slot_to_id + id_to_slot
    let generation_size = capacity as usize * size_of::<u16>();
    let before_vals = header + lookup + generation_size;
    let pad = (8 - (before_vals % 8)) % 8; // Changed to 8-byte alignment
    let vals = capacity as usize * element_size as usize;

    let expected = before_vals + pad + vals;

    assert_eq!(size, expected);
    assert_eq!(alignment(), 8);
}

#[test]
fn test_init_allocate_remove() {
    let capacity = 4u16;
    let element_size = 1u16;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();
    unsafe {
        init(base, capacity, element_size);
        assert_eq!(*(base as *const u16), capacity);
        assert_eq!(element_count(base), 0u16);
        let mut generations = Vec::new();
        let mut ids = Vec::new();
        for _ in 0..capacity {
            let (id, generation) = allocate(base).expect("should work");
            assert!(id < capacity);
            assert_eq!(generation, 1);
            assert!(is_alive(base, id, generation));
            generations.push(generation);
            ids.push(id);
        }
        assert!(allocate(base).is_none());
        let id: u16 = ids[0];
        let generation_before = generations[0];
        assert!(remove(base, id, generation_before));
        assert!(!is_alive(base, id, generation_before));
        assert_eq!(element_count(base), capacity - 1);
        let (id_new, gen_new) = allocate(base).unwrap();
        assert_eq!(id_new, id);
        assert_eq!(gen_new, generation_before + 2); // Both alloc and remove bumps the generation
    }
}

#[test]
fn test_element_count_and_slot_to_id_loop() {
    let capacity = 3u16;
    let element_size = 1u16;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();
    unsafe {
        init(base, capacity, element_size);
        let values = [10u8, 20u8, 30u8];
        for &v in &values {
            let (id, generation) = allocate(base).unwrap();
            insert(base, id, &raw const v);
            assert!(is_alive(base, id, generation));
        }
        let count = element_count(base) as usize;
        assert_eq!(count, values.len());
        let slot_to_id = slot_to_id_ptr(base);
        let pre = values_offset(base);
        for i in 0..count {
            let id = *slot_to_id.add(i);
            let val = *base.add(pre + id as usize);
            assert_eq!(val, values[i]);
        }
    }
}
