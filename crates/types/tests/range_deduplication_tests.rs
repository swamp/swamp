/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMap;
use source_map_node::Node;
use swamp_types::prelude::{AnonymousStructType, NamedStructType, StructTypeField, TypeCache};

#[test]
fn test_range_deduplication_with_named_struct() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();

    // Create an anonymous struct for the Range type: {min: int, max: int, inclusive: bool}
    let mut range_fields = SeqMap::new();
    let _ = range_fields.insert(
        "min".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = range_fields.insert(
        "max".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type,
        },
    );
    let _ = range_fields.insert(
        "inclusive".to_string(),
        StructTypeField {
            identifier: None,
            field_type: bool_type,
        },
    );

    let anon_struct = AnonymousStructType::new(range_fields);
    let anon_struct_ref = type_cache.anonymous_struct(anon_struct);

    // Create a NamedStructType for Range
    let range_named_struct = NamedStructType::new(
        Node::default(),
        "Range",
        anon_struct_ref,
        &["core".to_string()],
    );

    let named_struct_ref1 = type_cache.named_struct(range_named_struct.clone());
    let named_struct_ref2 = type_cache.named_struct(range_named_struct);

    // The named struct TypeRefs should be the same due to deduplication
    assert!(std::rc::Rc::ptr_eq(&named_struct_ref1, &named_struct_ref2));

    // Create Range types using the named struct
    let range_type1 = type_cache.range(named_struct_ref1);
    let range_type2 = type_cache.range(named_struct_ref2);

    // The Range TypeRefs should be the same due to deduplication
    assert!(std::rc::Rc::ptr_eq(&range_type1, &range_type2));
}

#[test]
fn test_range_field_access() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();

    // Create an anonymous struct for the Range type
    let mut range_fields = SeqMap::new();
    let _ = range_fields.insert(
        "min".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = range_fields.insert(
        "max".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type,
        },
    );
    let _ = range_fields.insert(
        "inclusive".to_string(),
        StructTypeField {
            identifier: None,
            field_type: bool_type,
        },
    );

    let anon_struct = AnonymousStructType::new(range_fields);
    let anon_struct_ref = type_cache.anonymous_struct(anon_struct);

    // Create a NamedStructType for Range
    let range_named_struct = NamedStructType::new(
        Node::default(),
        "Range",
        anon_struct_ref,
        &["core".to_string()],
    );

    let named_struct_ref = type_cache.named_struct(range_named_struct);
    let range_type = type_cache.range(named_struct_ref);

    // Verify the Range type structure
    if let swamp_types::TypeKind::Range(range_ref) = &*range_type.kind {
        if let swamp_types::TypeKind::NamedStruct(named_struct) = &*range_ref.kind {
            assert_eq!(named_struct.assigned_name, "Range");
            assert_eq!(named_struct.field_index("min"), Some(0));
            assert_eq!(named_struct.field_index("max"), Some(1));
            assert_eq!(named_struct.field_index("inclusive"), Some(2));
            assert_eq!(named_struct.field_index("nonexistent"), None);
        } else {
            panic!("Expected NamedStruct in Range TypeRef");
        }
    } else {
        panic!("Expected Range TypeKind");
    }
}
