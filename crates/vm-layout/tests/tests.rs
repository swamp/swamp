use seq_map::SeqMap;
use std::rc::Rc;
use swamp_types::prelude::{
    AnonymousStructType, EnumVariantType, StructTypeField, TypeCache, TypeRef,
};
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::MemoryOffset;
use swamp_vm_types::types::BasicTypeKind;

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

    let layout1 = layout_cache.layout(int1);
    let layout2 = layout_cache.layout(int2);

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
            identifier: None,
            field_type: int_type,
        },
    );
    let _ = fields.insert(
        "field2".to_string(),
        StructTypeField {
            identifier: None,
            field_type: bool_type,
        },
    );
    let _ = fields.insert(
        "field3".to_string(),
        StructTypeField {
            identifier: None,
            field_type: string_type,
        },
    );

    let anon_struct = AnonymousStructType::new(fields);
    let struct_type = type_cache.anonymous_struct(anon_struct);

    let mut layout_cache = LayoutCache::new();
    let struct_layout = layout_cache.layout(struct_type);

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
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = inner_fields.insert(
        "y".to_string(),
        StructTypeField {
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
            identifier: None,
            field_type: inner_struct_type.clone(),
        },
    );
    let _ = outer_fields.insert(
        "point2".to_string(),
        StructTypeField {
            identifier: None,
            field_type: inner_struct_type,
        },
    );

    let outer_struct = AnonymousStructType::new(outer_fields);
    let outer_struct_type = TypeRef::from(type_cache.anonymous_struct(outer_struct));

    // Layout the outer struct
    let mut layout_cache = LayoutCache::new();
    let outer_layout = layout_cache.layout(outer_struct_type);

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

    assert_eq!(layout_cache.id_to_layout.len(), 1);
    // The kind_to_layout cache contains all types: int, inner point struct, and outer struct
    assert_eq!(layout_cache.kind_to_layout.len(), 3);
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
    let layout1 = layout_cache.layout(tuple1);
    let layout2 = layout_cache.layout(tuple2);

    // Verify that they point to the same memory location
    assert!(Rc::ptr_eq(&layout1, &layout2));

    // The `id_to_layout` cache only contains the tuple type. (TODO: The int_type should also be in the id_to_layout)
    assert_eq!(layout_cache.id_to_layout.len(), 1);
    // The `kind_to_layout` contains: int, bool, and tuple
    assert_eq!(layout_cache.kind_to_layout.len(), 3);
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
    let layout1 = layout_cache.layout(optional1);
    let layout2 = layout_cache.layout(optional2);

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

    // The `id_to_layout` cache only contains the optional type //TODO: should contain 2 in the future, the optional and the int.
    assert_eq!(layout_cache.id_to_layout.len(), 1);
    // The kind_to_layout cache contains all types: int and optional
    assert_eq!(layout_cache.kind_to_layout.len(), 2);
}

#[test]
fn test_collection_type_deduplication() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();

    // Create vector types with the same element type but different capacities
    let vec1 = type_cache.vec_storage(&int_type, 10);
    let vec2 = type_cache.vec_storage(&int_type, 20);

    let mut layout_cache = LayoutCache::new();
    let layout1 = layout_cache.layout(vec1);
    let layout2 = layout_cache.layout(vec2);

    // These should be in different memory locations, since the capacities are not the same
    assert!(!Rc::ptr_eq(&layout1, &layout2));

    match (&layout1.kind, &layout2.kind) {
        (BasicTypeKind::VecStorage(elem1, _), BasicTypeKind::VecStorage(elem2, _)) => {
            // They should share the same element type and that element type should be in one place
            assert!(Rc::ptr_eq(elem1, elem2));
        }
        _ => panic!("Expected VecStorage types"),
    }

    // The `id_to_layout` cache contains both vec storage types // TODO: The int type should have gotten picked up as an ID as well
    assert_eq!(layout_cache.id_to_layout.len(), 2);
    // The kind_to_layout cache contains all types: int and both vec storage types
    assert_eq!(layout_cache.kind_to_layout.len(), 3);
}

#[test]
fn test_map_storage_deduplication() {
    let mut type_cache = TypeCache::new();

    let string_type = type_cache.string();
    let int_type = type_cache.int();

    // Create map types with the *same* key/value types
    let map1 = type_cache.map_storage(&string_type, &int_type, 10);
    let map2 = type_cache.map_storage(&string_type, &int_type, 10);

    // Layout both map types
    let mut layout_cache = LayoutCache::new();
    let layout1 = layout_cache.layout(map1);
    let layout2 = layout_cache.layout(map2);

    // Verify they are pointing to the same memory
    assert!(Rc::ptr_eq(&layout1, &layout2));

    match &layout1.kind {
        BasicTypeKind::MapStorage {
            tuple_type,
            logical_limit,
            capacity,
            ..
        } => {
            // Verify capacity and logical limit
            assert_eq!(*logical_limit, 10);
            assert_eq!(capacity.0, 16); // Next power of 2 after 10

            // Verify the tuple type has string and int fields
            assert_eq!(tuple_type.fields.len(), 2);

            match &tuple_type.fields[0].ty.kind {
                BasicTypeKind::InternalStringPointer => {}
                _ => panic!("Expected string type for key"),
            }

            match &tuple_type.fields[1].ty.kind {
                BasicTypeKind::S32 => {}
                _ => panic!("Expected S32 type for value"),
            }
        }
        _ => panic!("Expected MapStorage type"),
    }

    // The id_to_layout cache only contains the map storage type. // TODO: should be 4, since string, int, tuple and map have unique ID themselves
    assert_eq!(layout_cache.id_to_layout.len(), 1);
    // The kind_to_layout cache contains all types: string, int, tuple, and map storage
    assert_eq!(layout_cache.kind_to_layout.len(), 4);
}

#[test]
fn test_enum_variant_deduplication() {
    let mut type_cache = TypeCache::new();
    let mut layout_cache = LayoutCache::new();

    let enum_type = create_test_enum(&mut type_cache);

    let enum_layout = layout_cache.layout(enum_type);

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

    // The `id_to_layout` contains: the enum type
    assert_eq!(layout_cache.id_to_layout.len(), 1);
    // The `kind_to_layout`: int, struct for Some variant, and enum
    assert_eq!(layout_cache.kind_to_layout.len(), 3);
}

fn create_test_enum(type_cache: &mut TypeCache) -> TypeRef {
    use source_map_node::Node;
    use swamp_types::prelude::{
        EnumType, EnumVariantCommon, EnumVariantSimpleType, EnumVariantStructType,
    };

    // Create a simple enum with `None` and `Some(Int)` variants
    let mut enum_type = EnumType::new(Node::default(), "Option", vec!["test".to_string()]);

    let none_variant = EnumVariantSimpleType {
        common: EnumVariantCommon {
            name: Node::default(),
            assigned_name: "None".to_string(),
            container_index: 0,
        },
    };
    let _ = enum_type
        .variants
        .insert("None".to_string(), EnumVariantType::Nothing(none_variant));

    let int_type = type_cache.int();

    let mut some_fields = SeqMap::new();
    let _ = some_fields.insert(
        "value".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type,
        },
    );

    let some_variant = EnumVariantStructType {
        common: EnumVariantCommon {
            name: Node::default(),
            assigned_name: "Some".to_string(),
            container_index: 1,
        },
        anon_struct: AnonymousStructType::new(some_fields),
    };
    let _ = enum_type
        .variants
        .insert("Some".to_string(), EnumVariantType::Struct(some_variant));

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

    let struct1 = AnonymousStructType::new(fields1);
    let struct_type1 = TypeRef::from(type_cache.anonymous_struct(struct1));

    // Create a second struct with a slightly different field names
    // to ensure different TypeIds but same structural layout
    let mut fields2 = SeqMap::new();
    let _ = fields2.insert(
        "a".to_string(),
        StructTypeField {
            identifier: None,
            field_type: int_type.clone(),
        },
    );
    let _ = fields2.insert(
        "b".to_string(),
        StructTypeField {
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
    let layout1 = layout_cache.layout(struct_type1);
    let layout2 = layout_cache.layout(struct_type2);

    // They should be different layout objects because they have different field names
    // even though they're structurally similar
    assert!(!Rc::ptr_eq(&layout1, &layout2));

    // The `id_to_layout` contains both struct types
    assert_eq!(layout_cache.id_to_layout.len(), 2);
    // The `kind_to_layout` contains: int and both struct types
    assert_eq!(layout_cache.kind_to_layout.len(), 3);
}
