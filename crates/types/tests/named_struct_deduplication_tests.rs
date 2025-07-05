/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMap;
use source_map_node::Node;
use swamp_types::prelude::{AnonymousStructType, NamedStructType, StructTypeField, TypeCache};

#[test]
fn test_named_struct_deduplication_with_type_ref() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();

    let mut fields1 = SeqMap::new();
    let _ = fields1.insert(
        "x".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = fields1.insert(
        "y".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type.clone(),
        },
    );

    let mut fields2 = SeqMap::new();
    let _ = fields2.insert(
        "x".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = fields2.insert(
        "y".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type,
        },
    );

    // Create two AnonymousStructType instances with identical structure
    let anon_struct1 = AnonymousStructType::new(fields1);
    let anon_struct2 = AnonymousStructType::new(fields2);

    let anon_struct_ref1 = type_cache.anonymous_struct(anon_struct1);
    let anon_struct_ref2 = type_cache.anonymous_struct(anon_struct2);

    // The TypeRefs should be the same due to deduplication
    assert!(std::rc::Rc::ptr_eq(&anon_struct_ref1, &anon_struct_ref2));

    // Create two NamedStructTypes that use the same anonymous struct
    let named_struct1 = NamedStructType::new(
        Node::default(),
        "Point",
        anon_struct_ref1,
        &["test".to_string()],
    );

    let named_struct2 = NamedStructType::new(
        Node::default(),
        "Point",
        anon_struct_ref2,
        &["test".to_string()],
    );

    // The NamedStructTypes should have the same anon_struct_type TypeRef
    assert!(std::rc::Rc::ptr_eq(
        &named_struct1.anon_struct_type,
        &named_struct2.anon_struct_type
    ));

    let named_struct_ref1 = type_cache.named_struct(named_struct1);
    let named_struct_ref2 = type_cache.named_struct(named_struct2);

    assert!(std::rc::Rc::ptr_eq(&named_struct_ref1, &named_struct_ref2));
}

#[test]
fn test_named_struct_field_access() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();

    let mut fields = SeqMap::new();
    let _ = fields.insert(
        "x".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type,
        },
    );
    let _ = fields.insert(
        "active".to_string(),
        StructTypeField {
            identifier: None,
            field_type: bool_type,
        },
    );

    let anon_struct = AnonymousStructType::new(fields);
    let anon_struct_ref = type_cache.anonymous_struct(anon_struct);

    let named_struct = NamedStructType::new(
        Node::default(),
        "Entity",
        anon_struct_ref,
        &["test".to_string()],
    );

    assert_eq!(named_struct.field_index("x"), Some(0));
    assert_eq!(named_struct.field_index("active"), Some(1));
    assert_eq!(named_struct.field_index("nonexistent"), None);
}
