/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::rc::Rc;
    use swamp_types::TypeId;
    use swamp_types::prelude::{TypeCache, TypeKind};

    fn count_unique_types(cache: &TypeCache) -> usize {
        cache.type_id_to_type().len()
    }

    #[test]
    fn test_no_duplication_in_primitive_types() {
        let mut cache = TypeCache::new();

        // Create the same primitive type multiple times
        let int1 = cache.int();
        let int2 = cache.int();
        let int3 = cache.int();

        assert_eq!(count_unique_types(&cache), 1);

        assert_eq!(int1.id, int2.id);
        assert_eq!(int2.id, int3.id);
    }

    #[test]
    fn test_container_type_reuses_inner_types() {
        let mut cache = TypeCache::new();

        let int_type = cache.int();
        let float_type = cache.float();

        assert_eq!(count_unique_types(&cache), 2);

        let vec_int = cache.vec_storage(&int_type, 10);
        let optional_int = cache.optional(&int_type);
        let vec_float = cache.vec_storage(&float_type, 10);
        let optional_float = cache.optional(&float_type);

        assert_eq!(count_unique_types(&cache), 6);

        // Recreate the same container types
        let vec_int2 = cache.vec_storage(&int_type, 10);
        let optional_int2 = cache.optional(&int_type);
        let vec_float2 = cache.vec_storage(&float_type, 10);
        let optional_float2 = cache.optional(&float_type);

        // Since we used the same types, the count should not have changed
        assert_eq!(count_unique_types(&cache), 6);

        // Verify type IDs are reused for both int and float containers
        assert_eq!(vec_int.id, vec_int2.id);
        assert_eq!(optional_int.id, optional_int2.id);
        assert_eq!(vec_float.id, vec_float2.id);
        assert_eq!(optional_float.id, optional_float2.id);

        // Verify containers with different inner types have different IDs
        assert_ne!(vec_int.id, vec_float.id);
        assert_ne!(optional_int.id, optional_float.id);

        // Extract and check the inner types
        match (&*vec_int.kind, &*vec_int2.kind) {
            (TypeKind::VecStorage(inner1, _), TypeKind::VecStorage(inner2, _)) => {
                // Important check: Inner types should be the exact same Rc (same pointer)
                assert!(Rc::ptr_eq(inner1, inner2));
                // And they should be the original int type
                assert!(Rc::ptr_eq(inner1, &int_type));
            }
            _ => panic!("Unexpected type kind"),
        }

        // Do the same check for float containers
        match (&*vec_float.kind, &*vec_float2.kind) {
            (TypeKind::VecStorage(inner1, _), TypeKind::VecStorage(inner2, _)) => {
                assert!(Rc::ptr_eq(inner1, inner2));
                assert!(Rc::ptr_eq(inner1, &float_type));
            }
            _ => panic!("Unexpected type kind"),
        }
    }

    #[test]
    fn test_complex_nested_type_reuse() {
        let mut cache = TypeCache::new();

        let int_type = cache.int();
        let string_type = cache.string(); // Note that string also allocates byte() and char()

        let vec_int_5 = cache.vec_storage(&int_type, 5);
        let vec_int_10 = cache.vec_storage(&int_type, 10);
        let opt_string = cache.optional(&string_type);

        assert_eq!(count_unique_types(&cache), 7);

        let map_vec5_opt = cache.map_storage(&vec_int_5, &opt_string, 20);
        let map_vec10_opt = cache.map_storage(&vec_int_10, &opt_string, 20);

        assert_eq!(count_unique_types(&cache), 9);

        // Create the same complex types again
        let map_vec5_opt2 = cache.map_storage(&vec_int_5, &opt_string, 20);
        let map_vec10_opt2 = cache.map_storage(&vec_int_10, &opt_string, 20);

        assert_eq!(count_unique_types(&cache), 9);
        assert_eq!(map_vec5_opt.id, map_vec5_opt2.id);
        assert_eq!(map_vec10_opt.id, map_vec10_opt2.id);

        assert_ne!(map_vec5_opt.id, map_vec10_opt.id);

        // Create a slightly different map type using the same inner types but different capacity
        let map_vec5_opt_diff = cache.map_storage(&vec_int_5, &opt_string, 30);

        assert_eq!(count_unique_types(&cache), 10);
        assert_ne!(map_vec5_opt.id, map_vec5_opt_diff.id);

        match (&*map_vec5_opt.kind, &*map_vec10_opt.kind) {
            (TypeKind::MapStorage(key1, val1, _), TypeKind::MapStorage(key2, val2, _)) => {
                // Keys should be different (different vector capacities)
                assert!(!Rc::ptr_eq(key1, key2));
                // But values should be the same
                assert!(Rc::ptr_eq(val1, val2));

                // And they should be our original types
                assert!(Rc::ptr_eq(key1, &vec_int_5));
                assert!(Rc::ptr_eq(key2, &vec_int_10));
                assert!(Rc::ptr_eq(val1, &opt_string));
            }
            _ => panic!("Unexpected type kind"),
        }
    }

    #[test]
    fn test_deeply_nested_types() {
        let mut cache = TypeCache::new();

        let int_type = cache.int();

        let vec_int = cache.vec_storage(&int_type, 10);
        let opt_vec_int = cache.optional(&vec_int);
        let stack_opt_vec_int = cache.stack_storage(&opt_vec_int, 5);
        let map_int_to_stack = cache.map_storage(&int_type, &stack_opt_vec_int, 8);

        assert_eq!(count_unique_types(&cache), 5);

        let opt_int = cache.optional(&int_type);
        let stack_opt_int = cache.stack_storage(&opt_int, 5);

        assert_eq!(count_unique_types(&cache), 7);

        // Create a type that combines both hierarchies
        let tuple_type = cache.tuple(vec![
            Rc::clone(&map_int_to_stack),
            Rc::clone(&stack_opt_int),
        ]);

        assert_eq!(count_unique_types(&cache), 8);

        // Create a different tuple with the same elements in reverse order
        let tuple_reversed = cache.tuple(vec![
            Rc::clone(&stack_opt_int),
            Rc::clone(&map_int_to_stack),
        ]);

        // Different order = different type, so 9 total
        assert_eq!(count_unique_types(&cache), 9);

        let type_ids: HashSet<TypeId> = cache.type_id_to_type().keys().copied().collect();
        assert_eq!(type_ids.len(), 9);

        // Verify both tuples are properly constructed by checking pointer equality of elements
        match (&*tuple_type.kind, &*tuple_reversed.kind) {
            (TypeKind::Tuple(elems1), TypeKind::Tuple(elems2)) => {
                assert_eq!(elems1.len(), 2);
                assert!(Rc::ptr_eq(&elems1[0], &map_int_to_stack));
                assert!(Rc::ptr_eq(&elems1[1], &stack_opt_int));

                assert_eq!(elems2.len(), 2);
                assert!(Rc::ptr_eq(&elems2[0], &stack_opt_int));
                assert!(Rc::ptr_eq(&elems2[1], &map_int_to_stack));
            }
            _ => panic!("Unexpected type kind"),
        }

        match &*map_int_to_stack.kind {
            TypeKind::MapStorage(key, _, _) => {
                assert!(Rc::ptr_eq(key, &int_type));
            }
            _ => panic!("Unexpected type kind"),
        }
    }
    #[test]
    fn test_type_id_assignment_and_reuse() {
        let mut cache = TypeCache::new();

        let int = cache.int();
        assert_eq!(int.id, TypeId::new(0));

        let float = cache.float();
        assert_eq!(float.id, TypeId::new(1));

        let bool = cache.bool();
        assert_eq!(bool.id, TypeId::new(2));

        let vec_int = cache.vec_storage(&int, 10);
        assert_eq!(vec_int.id, TypeId::new(3));

        let int2 = cache.int();
        assert_eq!(int2.id, TypeId::new(0));

        let vec_int2 = cache.vec_storage(&int, 10);
        assert_eq!(vec_int2.id, TypeId::new(3));

        let string = cache.string(); // Note: String allocates also byte() (id 4) and char() (id 5) first, so it gets id=6
        assert_eq!(string.id, TypeId::new(6));

        assert_eq!(count_unique_types(&cache), 7);
    }

    #[test]
    fn test_tuple_sharing() {
        let mut cache = TypeCache::new();

        let int = cache.int();
        let float = cache.float();
        let string = cache.string(); // Note that string also allocates byte() and char()

        let tuple1 = cache.tuple(vec![Rc::clone(&int), Rc::clone(&float)]);
        let tuple2 = cache.tuple(vec![Rc::clone(&int), Rc::clone(&float)]);
        let tuple3 = cache.tuple(vec![Rc::clone(&float), Rc::clone(&int)]);

        assert_eq!(tuple1.id, tuple2.id);
        assert_ne!(tuple1.id, tuple3.id);
        assert_eq!(count_unique_types(&cache), 7);

        let nested_tuple1 = cache.tuple(vec![Rc::clone(&tuple1), Rc::clone(&string)]);
        let nested_tuple2 = cache.tuple(vec![Rc::clone(&tuple1), Rc::clone(&string)]);

        assert_eq!(nested_tuple1.id, nested_tuple2.id);

        assert_eq!(count_unique_types(&cache), 8);

        match &*nested_tuple1.kind {
            TypeKind::Tuple(elements) => {
                assert!(Rc::ptr_eq(&elements[0], &tuple1));
                assert!(Rc::ptr_eq(&elements[1], &string));
            }
            _ => panic!("Unexpected type kind"),
        }
    }
}
