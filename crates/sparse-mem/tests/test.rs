use sparse_mem::{
    alignment, allocate, element_count, init, insert, insert_if_alive, is_alive, layout_size,
    remove, slot_to_id_ptr, values_offset,
};

#[test]
fn test_layout_size_and_alignment() {
    let capacity = 3u16;
    let element_size = 2u32;
    let size = layout_size(capacity, element_size);

    let header = 3 * size_of::<u32>(); // capacity + element_count + element_size
    let lookup = 2 * capacity as usize * size_of::<u16>(); // slot_to_id + id_to_slot
    let generation_size = capacity as usize * size_of::<u16>();
    let before_vals = header + lookup + generation_size;
    let pad = (8 - (before_vals % 8)) % 8; // 8-byte alignment
    let vals = capacity as usize * element_size as usize;

    let expected = before_vals + pad + vals;

    assert_eq!(size, expected);
    assert_eq!(alignment(), 8);
}

#[test]
fn test_init_allocate_remove() {
    let capacity = 4u16;
    let element_size = 1u32;
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
        assert_eq!(gen_new, generation_before + 2); // NOTE: Both alloc and remove bumps the generation
    }
}

#[test]
fn test_element_count_and_slot_to_id_loop() {
    let capacity = 3u16;
    let element_size = 1u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();
    unsafe {
        init(base, capacity, element_size);
        let values = [10u8, 20u8, 30u8];
        for &v in &values {
            let (id, generation) = allocate(base).unwrap();
            assert!(insert(base, id, &raw const v));
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

#[test]
fn test_frequent_add_remove_index_reuse() {
    let capacity = 10u16;
    let element_size = 4u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        let mut handles = Vec::new();

        //  Fill to capacity
        for i in 0..capacity {
            let (id, generation) = allocate(base).expect("should allocate");
            let value = (i + 100) as u32; // Use distinctive values
            insert(base, id, (&raw const value).cast::<u8>());
            handles.push((id, generation, value));
            assert_eq!(element_count(base), i + 1);
        }

        assert!(allocate(base).is_none());
        assert_eq!(element_count(base), capacity);

        let mut removed_indices = Vec::new();
        for i in (0..handles.len()).step_by(2) {
            let (id, generation, _) = handles[i];
            assert!(remove(base, id, generation));
            assert!(!is_alive(base, id, generation));
            removed_indices.push(i);
        }

        let expected_count = capacity - (removed_indices.len() as u16);
        assert_eq!(element_count(base), expected_count);

        let mut new_handles = Vec::new();
        for i in 0..removed_indices.len() {
            let (id, generation) = allocate(base).expect("should reuse freed slot");
            let value = (i + 200) as u32; // Different values to distinguish
            insert(base, id, (&raw const value).cast::<u8>());
            new_handles.push((id, generation, value));
        }

        assert_eq!(element_count(base), capacity);
        assert!(allocate(base).is_none());

        // Verify all new handles are valid and old removed ones are not
        for (id, generation, _) in &new_handles {
            assert!(is_alive(base, *id, *generation));
        }

        for &removed_idx in &removed_indices {
            let (id, old_generation, _) = handles[removed_idx];
            assert!(!is_alive(base, id, old_generation));
        }
    }
}

#[test]
fn test_stress_add_remove_patterns() {
    let capacity = 20u16;
    let element_size = 8u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        let mut active_handles = Vec::new();
        let mut next_value = 1000u64;

        // Perform 1000 operations of mixed add/remove
        for iteration in 0..1000 {
            let current_count = element_count(base) as usize;

            let should_add = if current_count == 0 {
                true // Must add if empty
            } else if current_count == capacity as usize {
                false // Must remove if full
            } else {
                (iteration % 3) != 0 // 2/3 chance to add, 1/3 to remove
            };

            if should_add {
                if let Some((id, generation)) = allocate(base) {
                    let value = next_value;
                    next_value += 1;
                    insert(base, id, (&raw const value).cast::<u8>());
                    active_handles.push((id, generation, value));

                    assert!(is_alive(base, id, generation));
                }
            } else if !active_handles.is_empty() {
                // Remove a random element
                let remove_idx = iteration % active_handles.len();
                let (id, generation, _) = active_handles.remove(remove_idx);

                assert!(remove(base, id, generation));
                assert!(!is_alive(base, id, generation));
            }

            assert_eq!(element_count(base) as usize, active_handles.len());

            for (id, generation, _) in &active_handles {
                assert!(is_alive(base, *id, *generation));
            }
        }
    }
}

#[test]
fn test_generation_overflow_behavior() {
    let capacity = 2u16;
    let element_size = 1u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        let mut last_generation = 0u16;

        for i in 0..100 {
            let (id, generation) = allocate(base).expect("should allocate");

            // Generation should increase each time (accounting for wrapping)
            if i > 0 {
                let expected = last_generation.wrapping_add(2); // alloc + remove both bump
                assert_eq!(generation, expected, "Generation mismatch at iteration {i}",);
            }

            let value = (i as u8).wrapping_add(50);
            insert(base, id, &raw const value);

            assert!(is_alive(base, id, generation));
            assert!(remove(base, id, generation));
            assert!(!is_alive(base, id, generation));

            last_generation = generation;
        }
    }
}

#[test]
fn test_index_reuse_order() {
    let capacity = 5u16;
    let element_size = 2u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        // Fill completely
        let mut handles = Vec::new();
        for _ in 0..capacity {
            let (id, generation) = allocate(base).unwrap();
            handles.push((id, generation));
        }

        let remove_order = [1, 3, 0];
        let mut removed_ids = Vec::new();

        for &idx in &remove_order {
            let (id, generation) = handles[idx];
            assert!(remove(base, id, generation));
            removed_ids.push(id);
        }

        // Now allocate again - should reuse in LIFO order (stack-like)
        // The last removed should be the first reused
        let mut reused_ids = Vec::new();
        for _ in 0..remove_order.len() {
            let (id, _generation) = allocate(base).unwrap();
            reused_ids.push(id);
        }

        // Verify LIFO reuse: last removed (id 0) should be first reused
        assert_eq!(reused_ids[0], removed_ids[2]); // removed_ids[2] is the last removed (id 0)
        assert_eq!(reused_ids[1], removed_ids[1]); // removed_ids[1] is id 3
        assert_eq!(reused_ids[2], removed_ids[0]); // removed_ids[0] is id 1
    }
}

#[test]
fn test_capacity_boundaries() {
    let capacity = 3u16;
    let element_size = 1u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        // Test multiple fill/empty cycles
        for cycle in 0..5 {
            // Fill to capacity
            let mut handles = Vec::new();
            for i in 0..capacity {
                let (id, generation) = allocate(base).expect("should allocate in cycle");
                let value = (cycle * 10 + i) as u8;
                insert(base, id, &raw const value);
                handles.push((id, generation, value));
            }

            assert_eq!(element_count(base), capacity);
            assert!(allocate(base).is_none(), "should be full in cycle {cycle}");

            // Empty completely
            for (id, generation, _) in handles {
                assert!(remove(base, id, generation));
            }

            assert_eq!(element_count(base), 0);
        }
    }
}

#[test]
fn test_invalid_operations() {
    let capacity = 3u16;
    let element_size = 1u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        let (id, generation) = allocate(base).unwrap();

        assert!(!remove(base, id, generation.wrapping_add(1)));
        assert!(is_alive(base, id, generation)); // Should still be alive

        assert!(remove(base, id, generation));
        assert!(!is_alive(base, id, generation));

        assert!(!remove(base, id, generation));

        assert!(!is_alive(base, id, generation));
    }
}

#[test]
fn test_insert_into_removed_slot_bug() {
    let capacity = 5u16;
    let element_size = 4u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        let (id, generation) = allocate(base).unwrap();
        let original_value = 42u32;
        assert!(insert(base, id, (&raw const original_value).cast::<u8>()));

        // Remove the slot
        assert!(remove(base, id, generation));
        assert!(!is_alive(base, id, generation));

        // insert() now checks if slot is alive
        let malicious_value = 999u32;
        let insert_result = insert(base, id, (&raw const malicious_value).cast::<u8>());
        assert!(!insert_result, "Insert into removed slot should fail!");

        // Now allocate the same slot again (should reuse the id)
        let (new_id, new_generation) = allocate(base).unwrap();
        assert_eq!(new_id, id); // Should be the same id
        assert_ne!(new_generation, generation); // But different generation
    }
}

#[test]
fn test_bounds_checking_issues() {
    let capacity = 3u16;
    let element_size = 4u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        // Test with id >= capacity (should be invalid)
        let invalid_id = capacity; // This is out of bounds
        let fake_generation = 1u16;

        // These operations should ideally fail gracefully, but might not:

        // is_alive with invalid id - this might access out of bounds memory
        let is_alive_result = is_alive(base, invalid_id, fake_generation);
        println!("is_alive with invalid id {invalid_id}: {is_alive_result}",);

        // insert with invalid id - this should now fail safely!
        let test_value = 123u32;
        let insert_result = insert(base, invalid_id, (&raw const test_value).cast::<u8>());
        println!("insert with invalid id {invalid_id}: {insert_result}");
        assert!(!insert_result, "Insert with invalid id should fail!");

        // remove with invalid id
        let remove_result = remove(base, invalid_id, fake_generation);
        println!("remove with invalid id {invalid_id}: {remove_result}");
    }
}

#[test]
fn test_iteration_consistency() {
    let capacity = 10u16;
    let element_size = 4u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        // Allocate some elements
        let mut handles = Vec::new();
        for i in 0..5 {
            let (id, generation) = allocate(base).unwrap();
            let value = (i + 100) as u32;
            insert(base, id, (&raw const value).cast::<u8>());
            handles.push((id, generation, value));
        }

        // Remove some elements (not in order)
        remove(base, handles[1].0, handles[1].1); // Remove second
        remove(base, handles[3].0, handles[3].1); // Remove fourth

        // Check iteration consistency
        let count = element_count(base) as usize;
        let slot_to_id = slot_to_id_ptr(base);
        let values_start = values_offset(base);

        println!("Current count: {}", count);

        // Iterate through active slots
        for slot_idx in 0..count {
            let id = *slot_to_id.add(slot_idx);
            println!("Slot {}: ID {}", slot_idx, id);

            // Verify this id is actually in our remaining handles
            let found = handles
                .iter()
                .any(|(h_id, h_gen, _)| *h_id == id && is_alive(base, *h_id, *h_gen));

            if !found {
                panic!(
                    "Iteration found ID {} that's not in our active handles!",
                    id
                );
            }

            // Read the value to ensure it's consistent
            let value_ptr = base.add(values_start + id as usize * element_size as usize);
            let stored_value = *(value_ptr as *const u32);
            println!("  Value at ID {}: {}", id, stored_value);
        }
    }
}

#[test]
fn test_memory_corruption_detection() {
    let capacity = 5u16;
    let element_size = 4u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        // Fill with known pattern
        let mut handles = Vec::new();
        for i in 0..capacity {
            let (id, generation) = allocate(base).unwrap();
            let value = (0xDEADBEEF_u32).wrapping_add(i as u32);
            insert(base, id, (&raw const value).cast::<u8>());
            handles.push((id, generation, value));
        }

        // Store original memory state
        let original_memory = memory_buffer.clone();

        // Remove and re-add elements
        for i in 0..3 {
            let (id, generation, _) = handles[i];
            remove(base, id, generation);
        }

        for i in 0..3 {
            let (id, _generation) = allocate(base).unwrap();
            let value = (0xCAFE_CAFE_u32).wrapping_add(i as u32);
            insert(base, id, (&raw const value).cast::<u8>());
        }

        let current_memory = &memory_buffer;

        assert_eq!(current_memory[0..2], original_memory[0..2]); // capacity unchanged
        assert_eq!(current_memory[4..8], original_memory[4..8]);

        println!("Memory corruption test completed - no obvious corruption detected");
    }
}

#[test]
fn test_element_count_comprehensive() {
    let capacity = 8u16;
    let element_size = 4u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        // Test 1: Initial state
        assert_eq!(element_count(base), 0, "Initial count should be 0");

        // Test 2: Sequential allocation
        let mut handles = Vec::new();
        for i in 0..capacity {
            let (id, generation) = allocate(base).expect("Should allocate");
            handles.push((id, generation));
            assert_eq!(
                element_count(base),
                i + 1,
                "Count should increment with each allocation"
            );
        }

        // Test 3: At capacity
        assert_eq!(element_count(base), capacity, "Should be at capacity");
        assert!(
            allocate(base).is_none(),
            "Should not allocate beyond capacity"
        );
        assert_eq!(
            element_count(base),
            capacity,
            "Count should remain at capacity after failed allocation"
        );

        // Test 4: Sequential removal
        for i in 0..capacity {
            let (id, generation) = handles[i as usize];
            assert!(remove(base, id, generation), "Should remove successfully");
            let expected_count = capacity - i - 1;
            assert_eq!(
                element_count(base),
                expected_count,
                "Count should decrement with each removal"
            );
        }

        // Test 5: Back to empty
        assert_eq!(element_count(base), 0, "Should be empty after removing all");

        // Test 6: Interleaved operations
        let mut active_handles = Vec::new();

        // Allocate 3
        for _ in 0..3 {
            let (id, generation) = allocate(base).unwrap();
            active_handles.push((id, generation));
        }
        assert_eq!(element_count(base), 3, "Should have 3 after allocating 3");

        // Remove 1
        let (id, generation) = active_handles.remove(1);
        assert!(remove(base, id, generation));
        assert_eq!(element_count(base), 2, "Should have 2 after removing 1");

        // Allocate 2 more
        for _ in 0..2 {
            let (id, generation) = allocate(base).unwrap();
            active_handles.push((id, generation));
        }
        assert_eq!(
            element_count(base),
            4,
            "Should have 4 after allocating 2 more"
        );

        // Remove all remaining
        for (id, generation) in active_handles {
            assert!(remove(base, id, generation));
        }
        assert_eq!(element_count(base), 0, "Should be empty after removing all");

        // Test 7: Insert operations don't affect count
        let (id, generation) = allocate(base).unwrap();
        assert_eq!(element_count(base), 1, "Should be 1 after allocation");

        let value = 42u32;
        assert!(insert(base, id, (&raw const value).cast::<u8>()));
        assert_eq!(
            element_count(base),
            1,
            "Insert should not change element count"
        );

        // Insert again (overwrite)
        let value2 = 99u32;
        assert!(insert(base, id, (&raw const value2).cast::<u8>()));
        assert_eq!(
            element_count(base),
            1,
            "Multiple inserts should not change element count"
        );

        // Remove the slot
        assert!(remove(base, id, generation));
        assert_eq!(element_count(base), 0, "Should be 0 after final removal");

        // Test 8: Failed operations don't affect count
        assert!(!remove(base, id, generation), "Double removal should fail");
        assert_eq!(
            element_count(base),
            0,
            "Failed removal should not change count"
        );

        assert!(
            !insert(base, id, (&raw const value).cast::<u8>()),
            "Insert into removed slot should fail"
        );
        assert_eq!(
            element_count(base),
            0,
            "Failed insert should not change count"
        );

        // Test with invalid IDs
        assert!(
            !remove(base, capacity, 1),
            "Remove with invalid ID should fail"
        );
        assert_eq!(
            element_count(base),
            0,
            "Failed remove with invalid ID should not change count"
        );

        assert!(
            !insert(base, capacity, (&raw const value).cast::<u8>()),
            "Insert with invalid ID should fail"
        );
        assert_eq!(
            element_count(base),
            0,
            "Failed insert with invalid ID should not change count"
        );
    }
}

#[test]
fn test_generation_zero_is_invalid() {
    let capacity = 5u16;
    let element_size = 4u32;
    let size = layout_size(capacity, element_size);
    let mut memory_buffer = vec![0u8; size];
    let base = memory_buffer.as_mut_ptr();

    unsafe {
        init(base, capacity, element_size);

        // Test 1: allocate() should never return generation 0
        for _ in 0..capacity {
            let (id, generation) = allocate(base).expect("Should allocate");
            assert_ne!(
                generation, 0,
                "allocate() should never return generation 0 for id {id}",
            );
            assert!(
                generation >= 1,
                "Generation should be at least 1, got {generation} for id {id}",
            );
        }

        // Test 2: Generation 0 handles should never be considered alive
        for id in 0..capacity {
            assert!(
                !is_alive(base, id, 0),
                "Generation 0 should never be alive for id {id}",
            );
        }

        // Test 3: Trying to remove with generation 0 should fail
        for id in 0..capacity {
            assert!(
                !remove(base, id, 0),
                "Remove with generation 0 should fail for id {id}",
            );
        }

        // Test 4: Trying to insert with generation 0 should fail
        for id in 0..capacity {
            let value = 42u32;
            assert!(
                !insert_if_alive(base, id, 0, (&raw const value).cast::<u8>()),
                "insert_if_alive with generation 0 should fail for id {id}",
            );
        }

        // Test 5: Even after removing and reallocating, generation 0 should remain invalid
        // First, we need to clear the array since it's already full from previous tests
        // Let's start fresh
        init(base, capacity, element_size);

        // Allocate some slots
        let mut handles = Vec::new();
        for _ in 0..3 {
            let (id, generation) = allocate(base).unwrap();
            handles.push((id, generation));
        }

        // Remove all
        for (id, generation) in handles {
            assert!(remove(base, id, generation));
            // After removal, generation 0 should still be invalid
            assert!(
                !is_alive(base, id, 0),
                "Generation 0 should still be invalid after removal for id {id}",
            );
        }

        // Reallocate - should still not return generation 0
        for _ in 0..3 {
            let (id, generation) = allocate(base).expect("Should reallocate");
            assert_ne!(
                generation, 0,
                "Reallocation should never return generation 0 for id {id}",
            );
            assert!(
                generation >= 2,
                "After one remove cycle, generation should be at least 2, got {generation} for id {id}",
            );
        }
    }
}
