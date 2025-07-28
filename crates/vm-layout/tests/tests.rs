/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

// IMPORTANT: Understanding id_to_layout vs kind_to_layout
//
// These two caches serve different purposes and can have different sizes:
//
// id_to_layout: Maps TypeId -> BasicTypeRef
// - Contains one entry per unique TypeId from swamp-types
// - Size depends on how many distinct types swamp-types creates
//
// kind_to_layout: Maps TypeKind -> BasicTypeRef
// - Contains one entry per unique structural layout
// - Enables sharing layouts between structurally identical types
// - Size depends on structural uniqueness, not TypeId uniqueness
//
// Possible relationships:
// 1. id_to_layout.len() == kind_to_layout.len()
//    - Each TypeId has a unique structure (most common case)
// 2. id_to_layout.len() > kind_to_layout.len()
//    - Multiple TypeIds share the same layout structure
//    - Example: Two different named structs with identical field layout
// 3. id_to_layout.len() < kind_to_layout.len()
//    - Internal types created for layout purposes without explicit TypeIds
//    - Example: MapStorage creates internal tuple types for key-value pair.
//    TODO: This is something that should be removed or changed. Maybe a layout can be created for
//    the whole bucket size (status, key, value) to be more correct

use seq_map::SeqMap;
use std::rc::Rc;
use swamp_symbol::TopLevelSymbolId;
use swamp_types::prelude::{
    AnonymousStructType, EnumType, EnumVariantCommon, EnumVariantType, StructTypeField, TypeCache,
    TypeRef,
};
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::types::BasicTypeKind;
use swamp_vm_types::MemoryOffset;

#[test]
fn test_create_cache() {
    let cache = LayoutCache::new();
    assert_eq!(cache.id_to_layout.len(), 0);
    assert_eq!(cache.kind_to_layout.len(), 0);
}

#[test]
fn test_primitive_types_deduplication() {
    let mut type_cache = TypeCache::new();

    let int1 = type_cache.int();
    let int2 = type_cache.int();

    let mut layout_cache = LayoutCache::new();

    let layout1 = layout_cache.layout(&int1);
    let layout2 = layout_cache.layout(&int2);

    // The two ints should have the exact same pointer in memory
    assert!(Rc::ptr_eq(&layout1, &layout2));

    // Verify cache sizes
    assert_eq!(layout_cache.id_to_layout.len(), 1);
    assert_eq!(layout_cache.kind_to_layout.len(), 1);
}

#[test]
fn test_struct_field_offsets() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();
    let string_type = type_cache.string();

    // Create an anonymous struct type: {field1: int, field2: bool, field3: string}
    let mut fields = SeqMap::new();
    let _ = fields.insert(
        "field1".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type,
        },
    );
    let _ = fields.insert(
        "field2".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type,
        },
    );
    let _ = fields.insert(
        "field3".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: string_type,
        },
    );

    let anon_struct = AnonymousStructType::new(fields);
    let struct_type = type_cache.anonymous_struct(anon_struct);

    let mut layout_cache = LayoutCache::new();
    let struct_layout = layout_cache.layout(&struct_type);

    // Verify it's a struct type
    match &struct_layout.kind {
        BasicTypeKind::Struct(struct_type) => {
            assert_eq!(struct_type.fields.len(), 3);

            assert_eq!(struct_type.fields[0].name, "field1");
            assert_eq!(struct_type.fields[1].name, "field2");
            assert_eq!(struct_type.fields[2].name, "field3");

            // field1 should be at offset 0
            assert_eq!(struct_type.fields[0].offset, MemoryOffset(0));

            // field2 should be after field1 (int is 4 bytes)
            assert_eq!(struct_type.fields[1].offset, MemoryOffset(4));

            // field3 should be after field2 with proper alignment
            // bool is 1 byte, but string pointer needs alignment to 4 bytes
            assert_eq!(struct_type.fields[2].offset, MemoryOffset(8));

            assert_eq!(struct_type.total_size.0, 12); // 4 (int) + padding to 8 + 4 (string ptr)
        }
        _ => panic!("Expected struct type"),
    }
}

#[test]
fn test_nested_struct_deduplication() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();

    // Create anonymous struct: {x:int, y:int}
    let mut inner_fields = SeqMap::new();
    let _ = inner_fields.insert(
        "x".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = inner_fields.insert(
        "y".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type,
        },
    );

    let inner_struct = AnonymousStructType::new(inner_fields);
    let inner_struct_type = type_cache.anonymous_struct(inner_struct);

    // Create an outer struct that contains the inner (point) struct twice
    let mut outer_fields = SeqMap::new();
    let _ = outer_fields.insert(
        "point1".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: inner_struct_type.clone(),
        },
    );
    let _ = outer_fields.insert(
        "point2".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: inner_struct_type,
        },
    );

    let outer_struct = AnonymousStructType::new(outer_fields);
    let outer_struct_type = TypeRef::from(type_cache.anonymous_struct(outer_struct));

    // Layout the outer struct
    let mut layout_cache = LayoutCache::new();
    let outer_layout = layout_cache.layout(&outer_struct_type);

    match &outer_layout.kind {
        BasicTypeKind::Struct(outer_struct) => {
            // Get the layouts of the two inner structs
            let point1_layout = &outer_struct.fields[0].ty;
            let point2_layout = &outer_struct.fields[1].ty;

            // Verify that the two points are pointing to the same location (pointer equality)
            assert!(Rc::ptr_eq(point1_layout, point2_layout));

            // Verify the inner point struct fields are correct
            match &point1_layout.kind {
                BasicTypeKind::Struct(inner_struct) => {
                    assert_eq!(inner_struct.fields.len(), 2);
                    assert_eq!(inner_struct.fields[0].name, "x");
                    assert_eq!(inner_struct.fields[1].name, "y");
                }
                _ => panic!("Expected struct type for point1"),
            }
        }
        _ => panic!("Expected struct type for outer struct"),
    }

    // Test for nested struct deduplication
    //
    // What swamp-types does: The TypeCache deduplicates structurally identical types.
    // When we create the inner struct {x: int, y: int} and use it twice in the outer struct,
    // swamp-types recognizes they're the same structure and assigns the same TypeId.
    //
    // Expected types in id_to_layout:
    // 1. int type (S32) - used in the inner struct fields
    // 2. inner struct type - the {x: int, y: int} point struct (used twice but same TypeId)
    // 3. outer struct type - contains two fields referencing the same inner struct
    //
    // Total: 3 unique TypeIds, even though the inner struct is referenced twice
    assert_eq!(
        layout_cache.id_to_layout.len(),
        3,
        "id_to_layout should contain exactly 3 entries: int type, inner struct type, and outer struct type. \
        The inner struct appears twice in the outer struct but swamp-types deduplicates it to the same TypeId."
    );

    // kind_to_layout also has 3 entries: int kind, inner struct kind, outer struct kind
    //
    // Note: kind_to_layout.len() == id_to_layout.len() in this case because each TypeId
    // corresponds to a unique TypeKind. There's no layout sharing between different types here.
    assert_eq!(
        layout_cache.kind_to_layout.len(),
        3,
        "kind_to_layout should contain exactly 3 entries: int kind, inner struct kind, and outer struct kind"
    );
}

#[test]
fn test_tuple_deduplication() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();

    // Create two tuple types with the same element types
    let tuple1 = type_cache.tuple(vec![int_type.clone(), bool_type.clone()]);
    let tuple2 = type_cache.tuple(vec![int_type, bool_type]);

    let mut layout_cache = LayoutCache::new();
    let layout1 = layout_cache.layout(&tuple1);
    let layout2 = layout_cache.layout(&tuple2);

    // Verify that they point to the same memory location
    assert!(Rc::ptr_eq(&layout1, &layout2));

    // Test for tuple deduplication
    //
    // What swamp-types does: When we call type_cache.tuple() twice with the same element types,
    // swamp-types recognizes the structural equality and returns the SAME TypeId for both calls.
    // This is because tuples are deduplicated based on their element types.
    //
    // Expected types in id_to_layout:
    // 1. int type (S32) - first element type
    // 2. bool type - second element type
    // 3. tuple type (int, bool) - only ONE tuple type because both tuple1 and tuple2 have the same TypeId
    //
    // Total: 3 unique TypeIds
    assert_eq!(
        layout_cache.id_to_layout.len(),
        3,
        "id_to_layout should contain exactly 3 entries: int type, bool type, and tuple type. \
        Even though we created tuple1 and tuple2 separately, swamp-types deduplicates them to the same TypeId \
        because they have identical structure (int, bool)."
    );

    // kind_to_layout has 3 entries: int kind, bool kind, tuple kind
    //
    // Note: kind_to_layout.len() == id_to_layout.len() in this case because swamp-types
    // already deduplicated tuple1 and tuple2 to the same TypeId, so there's only one
    // tuple TypeKind as well. No additional layout sharing occurs at the layout cache level.
    assert_eq!(
        layout_cache.kind_to_layout.len(),
        3,
        "kind_to_layout should contain exactly 3 entries: int kind, bool kind, and tuple kind"
    );
}

#[test]
fn test_optional_type_deduplication() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();

    // Create optional types with the same inner type
    let optional1 = type_cache.optional(&int_type);
    let optional2 = type_cache.optional(&int_type);

    // Layout both optional types
    let mut layout_cache = LayoutCache::new();
    let layout1 = layout_cache.layout(&optional1);
    let layout2 = layout_cache.layout(&optional2);

    // Verify they point to same memory
    assert!(Rc::ptr_eq(&layout1, &layout2));

    match &layout1.kind {
        BasicTypeKind::Optional(tagged_union) => {
            // Should have 2 variants: None and Some
            assert_eq!(tagged_union.variants.len(), 2);
            assert_eq!(tagged_union.variants[0].name, "None");
            assert_eq!(tagged_union.variants[1].name, "Some");

            // The `Some` variant should contain the int type
            match &tagged_union.variants[1].ty.kind {
                BasicTypeKind::S32 => {}
                _ => panic!("Expected S32 type for Some variant"),
            }
        }
        _ => panic!("Expected Optional type"),
    }

    // Test for optional type deduplication
    // id_to_layout contains: optional type and int type
    assert_eq!(
        layout_cache.id_to_layout.len(),
        2,
        "id_to_layout should contain exactly 2 entries: optional type and int type"
    );
    // kind_to_layout contains: int and optional
    //
    // Note: kind_to_layout.len() == id_to_layout.len() here (both are 2) because
    // the optional type creates a unique structure. Even though it internally
    // creates variant types, those are managed within the Optional structure
    // and don't create separate kind entries.
    assert_eq!(
        layout_cache.kind_to_layout.len(),
        2,
        "kind_to_layout should contain exactly 2 entries: int and optional"
    );
}

#[test]
fn test_collection_type_deduplication() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();

    // Create vector types with the same element type but different capacities
    let vec1 = type_cache.vec_storage(&int_type, 10);
    let vec2 = type_cache.vec_storage(&int_type, 20);

    let mut layout_cache = LayoutCache::new();
    let layout1 = layout_cache.layout(&vec1);
    let layout2 = layout_cache.layout(&vec2);

    // These should be in different memory locations, since the capacities are not the same
    assert!(!Rc::ptr_eq(&layout1, &layout2));

    match (&layout1.kind, &layout2.kind) {
        (BasicTypeKind::VecStorage(elem1, _), BasicTypeKind::VecStorage(elem2, _)) => {
            // They should share the same element type and that element type should be in one place
            assert!(Rc::ptr_eq(elem1, elem2));
        }
        _ => panic!("Expected VecStorage types"),
    }

    // Test for collection type deduplication
    // id_to_layout contains: vec1, vec2, and int type
    assert_eq!(
        layout_cache.id_to_layout.len(),
        3,
        "id_to_layout should contain exactly 3 entries: vec1, vec2, and int type"
    );
    // kind_to_layout contains: int and both vec storage types
    //
    // Note: kind_to_layout.len() == id_to_layout.len() here (both are 3) because
    // vec1 and vec2 have different capacities, making them structurally different
    // at both the TypeId level and the TypeKind level. The int type is shared
    // between them, but the VecStorage types are distinct due to different capacities.
    assert_eq!(
        layout_cache.kind_to_layout.len(),
        3,
        "kind_to_layout should contain exactly 3 entries: int and both vec storage types (with different capacities)"
    );
}

#[test]
fn test_map_storage_deduplication() {
    let mut type_cache = TypeCache::new();

    let string_type = type_cache.string(); // Note: string indirectly creates byte() and char()
    let int_type = type_cache.int();

    // Create map types with the *same* key/value types
    let map1 = type_cache.map_storage(&string_type, &int_type, 10);
    let map2 = type_cache.map_storage(&string_type, &int_type, 10);

    // Layout both map types
    let mut layout_cache = LayoutCache::new();
    let layout1 = layout_cache.layout(&map1);
    let layout2 = layout_cache.layout(&map2);

    // Verify they are pointing to the same memory
    assert!(Rc::ptr_eq(&layout1, &layout2));

    match &layout1.kind {
        BasicTypeKind::MapStorage {
            logical_limit,
            capacity,
            key_type,
            value_type,
            ..
        } => {
            // Verify capacity and logical limit
            assert_eq!(*logical_limit, 10); // logical_limit should be preserved as input
            assert_eq!(capacity.0, 16); // Next power of 2 after 10

            match key_type.kind {
                BasicTypeKind::StringView { .. } => {}
                _ => panic!("Expected string type for key"),
            }

            match value_type.kind {
                BasicTypeKind::S32 => {}
                _ => panic!("Expected S32 type for value"),
            }
        }
        _ => panic!("Expected MapStorage type"),
    }

    // Test for map storage deduplication
    //
    // What swamp-types does: When we create two maps with identical key/value types and capacity,
    // swamp-types deduplicates them and assigns the same TypeId. The map storage internally
    // creates a tuple type for key-value pairs, which is also deduplicated.
    //
    // Expected types in id_to_layout:
    // 1. string type (InternalStringPointer) - key type // and also byte and char
    // 2. int type (S32) - value type
    // 3. map storage type - only ONE because map1 and map2 have identical structure
    //
    // Note: The internal tuple type (string, int) for key-value pairs is created but doesn't
    // get a separate entry in id_to_layout because it's managed internally by the map storage.
    // It does appear in kind_to_layout for structural deduplication.
    //
    // Total: 3 unique TypeIds in id_to_layout, 4 kinds in kind_to_layout
    assert_eq!(
        layout_cache.id_to_layout.len(),
        5,
        "id_to_layout should contain exactly 5 entries: string type, byte type, char type, int type, and map storage type. \
        Even though we created map1 and map2 separately, swamp-types deduplicates them to the same TypeId \
        because they have identical structure (same key/value types and capacity)."
    );

    // kind_to_layout contains: string kind, int kind, tuple kind (for key-value pairs), and map storage kind
    //
    // IMPORTANT: kind_to_layout.len() > id_to_layout.len() here (4 vs 3)!
    //
    // This happens because the layout cache creates an internal tuple type for key-value pairs
    // that doesn't get its own TypeId in swamp-types (it's managed internally by MapStorage),
    // but it DOES get stored in kind_to_layout for structural layout sharing.
    // It will probably be changed/fixed in the future. But we can leave it for now.
    //
    // So we have:
    // - id_to_layout: 3 entries (string TypeId, int TypeId, map TypeId)
    // - kind_to_layout: 3  entries (string kind, int kind, map kind)
    //
    // The tuple kind exists for layout purposes but has no corresponding user-visible TypeId.
    // This demonstrates how the layout cache can share layouts between structurally identical
    // types even when those types don't have explicit TypeIds in the swamp-types system.
    assert_eq!(
        layout_cache.kind_to_layout.len(),
        5,
        "kind_to_layout should contain exactly 5 entries: string kind, int kind, \
         and map storage kind"
    );
}

#[test]
fn test_enum_variant_deduplication() {
    let mut type_cache = TypeCache::new();
    let mut layout_cache = LayoutCache::new();

    let enum_type = create_test_enum(&mut type_cache);

    let enum_layout = layout_cache.layout(&enum_type);

    match &enum_layout.kind {
        BasicTypeKind::TaggedUnion(tagged_union) => {
            // Should have 2 variants
            assert_eq!(tagged_union.variants.len(), 2);

            // Verify variant names
            assert_eq!(tagged_union.variants[0].name, "None");
            assert_eq!(tagged_union.variants[1].name, "Some");

            // Verify the `Some` variant contains an int
            match &tagged_union.variants[1].ty.kind {
                BasicTypeKind::Struct(struct_type) => {
                    assert_eq!(struct_type.fields.len(), 1);
                    assert_eq!(struct_type.fields[0].name, "value");

                    match &struct_type.fields[0].ty.kind {
                        BasicTypeKind::S32 => {}
                        _ => panic!("Expected S32 type for Some.value"),
                    }
                }
                _ => panic!("Expected struct type for Some variant"),
            }
        }
        _ => panic!("Expected TaggedUnion type"),
    }

    // Test for enum variant deduplication
    // id_to_layout now contains: enum type, int type, struct type for Some variant, and unit type for None variant

    assert_eq!(
        layout_cache.id_to_layout.len(),
        4,
        "id_to_layout should contain exactly 3 entries: enum type, int type, and struct type for Some variant"
    );

    // kind_to_layout contains: int, struct for Some variant, enum, and Empty type for None variant
    //
    // IMPORTANT: kind_to_layout.len() > id_to_layout.len() here (4 vs 3)!
    //
    // This happens because the enum creates internal variant types:
    // - The "None" variant creates an Empty type that gets stored in kind_to_layout
    //   but doesn't get its own TypeId (it's managed internally by the enum)
    // - The "Some" variant creates a struct type that DOES get a TypeId
    //
    // So we have:
    // - id_to_layout: 3 entries (int TypeId, struct TypeId for Some, enum TypeId)
    // - kind_to_layout: 5 entries (int kind, unit, struct kind, enum kind, Empty kind for None)
    //
    // This demonstrates internal type creation for layout optimization.
    assert_eq!(
        layout_cache.kind_to_layout.len(),
        5,
        "kind_to_layout should contain exactly 5 entries: int, unit, struct for Some variant, enum, and Empty type for None variant"
    );
}
fn create_test_enum(type_cache: &mut TypeCache) -> TypeRef {
    use source_map_node::Node;

    // Create a simple enum with `None` and `Some(Int)` variants
    let mut enum_type = EnumType::new(Node::default(), "Option", TopLevelSymbolId::new_illegal(), vec!["test".to_string()]);

    let none_variant = EnumVariantType {
        common: EnumVariantCommon {
            symbol_id: TopLevelSymbolId::new_illegal(),
            name: Node::default(),
            assigned_name: "None".to_string(),
            container_index: 0,
        },
        payload_type: type_cache.unit(), // Use unit type for empty variant
    };
    let _ = enum_type.variants.insert("None".to_string(), none_variant);

    let int_type = type_cache.int();

    let mut some_fields = SeqMap::new();
    let _ = some_fields.insert(
        "value".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type,
        },
    );

    // Create the anonymous struct type first
    let anon_struct = AnonymousStructType::new(some_fields);
    let anon_struct_type = type_cache.anonymous_struct(anon_struct);

    let some_variant = EnumVariantType {
        common: EnumVariantCommon {
            symbol_id: TopLevelSymbolId::new_illegal(),
            name: Node::default(),
            assigned_name: "Some".to_string(),
            container_index: 1,
        },
        payload_type: anon_struct_type,
    };
    let _ = enum_type.variants.insert("Some".to_string(), some_variant);

    TypeRef::from(type_cache.enum_type(enum_type))
}

#[test]
fn test_structural_type_equality() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();

    // Create two structurally identical structs but with different type IDs
    // We need to make them slightly different to get different TypeIds
    let mut fields1 = SeqMap::new();
    let _ = fields1.insert(
        "x".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = fields1.insert(
        "y".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type.clone(),
        },
    );

    let struct1 = AnonymousStructType::new(fields1);
    let struct_type1 = TypeRef::from(type_cache.anonymous_struct(struct1));

    // Create a second struct with a slightly different field names
    // to ensure different TypeIds but same structural layout
    let mut fields2 = SeqMap::new();
    let _ = fields2.insert(
        "a".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = fields2.insert(
        "b".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type,
        },
    );

    let struct2 = AnonymousStructType::new(fields2);
    let struct_type2 = TypeRef::from(type_cache.anonymous_struct(struct2));

    // Verify they have different type IDs
    assert_ne!(struct_type1.id, struct_type2.id);

    // Layout both structs
    let mut layout_cache = LayoutCache::new();
    let layout1 = layout_cache.layout(&struct_type1);
    let layout2 = layout_cache.layout(&struct_type2);

    // They should be different layout objects because they have different field names
    // even though they're structurally similar
    assert!(!Rc::ptr_eq(&layout1, &layout2));

    // Test for structural type equality
    //
    // What swamp-types does: Even though both structs have the same field types (int, int),
    // they have DIFFERENT field names ({x, y} vs {a, b}), so swamp-types treats anonymous structs as
    // structurally DIFFERENT types and assigns different TypeIds.
    //
    // Expected types in id_to_layout:
    // 1. int type (S32) - shared by both structs' fields
    // 2. first struct type {x: int, y: int} - unique TypeId
    // 3. second struct type {a: int, b: int} - different TypeId because field names differ
    //
    // Total: 3 unique TypeIds
    assert_eq!(
        layout_cache.id_to_layout.len(),
        3,
        "id_to_layout should contain exactly 3 entries: int type, first struct type {{x,y}}, and second struct type {{a,b}}. \
        Even though both structs have the same field types, swamp-types treats them as different types \
        because their field names differ (structural inequality)."
    );

    // kind_to_layout has 3 entries: int kind, first struct kind, second struct kind
    //
    // Note: kind_to_layout.len() == id_to_layout.len() in this case because the two structs
    // have different field names, making them structurally different at both the swamp-types
    // level (different TypeIds) AND the layout level (different TypeKinds). No layout sharing.
    assert_eq!(
        layout_cache.kind_to_layout.len(),
        3,
        "kind_to_layout should contain exactly 3 entries: int kind, first struct kind, and second struct kind"
    );
}
