use std::alloc::{Layout, alloc};

use hashmap_mem::{MapHeader, get_or_reserve_entry, init, layout, lookup, overwrite, remove};

#[test]
fn test_basic_insert_lookup() {
    // Test inserting and looking up a simple key-value pair
    let key_size = 4;
    let value_size = 8;
    let (_, map_init) = layout(key_size, 4, value_size, 8, 16);

    // Allocate memory for the map
    let layout = Layout::from_size_align(map_init.total_size as usize, 8).unwrap();
    let map_base = unsafe { alloc(layout) };
    assert!(!map_base.is_null());

    unsafe {
        // Initialize map
        init(map_base, &map_init);

        // Create a test key and value
        let key: u32 = 0x12345678;
        let key_ptr = &key as *const u32 as *const u8;

        // Insert a value
        let value_ptr = get_or_reserve_entry(map_base, key_ptr);
        assert!(!value_ptr.is_null());
        *(value_ptr as *mut u64) = 0xABCDEF0123456789;

        // Verify the element was inserted
        let header = &*(map_base as *const MapHeader);
        assert_eq!(header.element_count, 1);

        // Look up the value
        let found_ptr = lookup(map_base, key_ptr);
        assert!(!found_ptr.is_null());
        assert_eq!(*(found_ptr as *const u64), 0xABCDEF0123456789);
    }
}

#[test]
fn test_remove() {
    // Test removing entries from the map
    let key_size = 2;
    let value_size = 4;
    let (_, map_init) = layout(key_size, 2, value_size, 4, 8);

    // Allocate memory for the map
    let layout = Layout::from_size_align(map_init.total_size as usize, 8).unwrap();
    let map_base = unsafe { alloc(layout) };
    assert!(!map_base.is_null());

    unsafe {
        // Initialize map
        init(map_base, &map_init);

        let key1: u16 = 100;
        let key2: u16 = 200;
        let key1_ptr = &key1 as *const u16 as *const u8;
        let key2_ptr = &key2 as *const u16 as *const u8;

        let value1_ptr = get_or_reserve_entry(map_base, key1_ptr);
        *(value1_ptr as *mut u32) = 1000;

        let value2_ptr = get_or_reserve_entry(map_base, key2_ptr);
        *(value2_ptr as *mut u32) = 2000;

        // Verify it has two elements
        let header = &*(map_base as *const MapHeader);
        assert_eq!(header.element_count, 2);

        // Remove one key
        let removed = remove(map_base, key1_ptr);
        assert!(removed);

        // Verify count decreased
        let header = &*(map_base as *const MapHeader);
        assert_eq!(header.element_count, 1);

        // Verify first key no longer exists
        let found_ptr = lookup(map_base, key1_ptr);
        assert!(found_ptr.is_null());

        // Verify second key still exists
        let found_ptr = lookup(map_base, key2_ptr);
        assert!(!found_ptr.is_null());
        assert_eq!(*(found_ptr as *const u32), 2000);
    }
}

#[test]
fn test_overwrite() {
    let key_size = 4;
    let value_size = 4;
    let (_, source_init) = layout(key_size, 4, value_size, 4, 8);
    let (_, target_init) = layout(key_size, 4, value_size, 4, 16);

    let source_layout = Layout::from_size_align(source_init.total_size as usize, 8).unwrap();
    let source_base = unsafe { alloc(source_layout) };
    assert!(!source_base.is_null());

    let target_layout = Layout::from_size_align(target_init.total_size as usize, 8).unwrap();
    let target_base = unsafe { alloc(target_layout) };
    assert!(!target_base.is_null());

    unsafe {
        init(source_base, &source_init);
        init(target_base, &target_init);

        // Insert keys into source
        for i in 0..3 {
            let key = i;
            let key_ptr = &key as *const i32 as *const u8;
            let value_ptr = get_or_reserve_entry(source_base, key_ptr);
            *(value_ptr as *mut i32) = i * 100;
        }

        let source_header = &*(source_base as *const MapHeader);
        assert_eq!(source_header.element_count, 3);

        let success = overwrite(target_base, source_base);
        assert!(success);

        let target_header = &*(target_base as *const MapHeader);
        assert_eq!(target_header.element_count, 3);

        // Verify all keys were copied correctly
        for i in 0..3 {
            let key = i;
            let key_ptr = &key as *const i32 as *const u8;
            let found_ptr = lookup(target_base, key_ptr);
            assert!(!found_ptr.is_null());
            assert_eq!(*(found_ptr as *const i32), i * 100);
        }
    }
}
