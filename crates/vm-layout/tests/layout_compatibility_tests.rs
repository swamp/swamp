/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMap;
use source_map_node::Node;
use swamp_symbol::TopLevelSymbolId;
use swamp_types::prelude::{
    AnonymousStructType, EnumVariantType, StructTypeField, TypeCache, TypeRef,
};
use swamp_types::prelude::{EnumType, EnumVariantCommon};
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::types::BasicTypeKind;
use swamp_vm_types::{MemoryAlignment, MemoryOffset, MemorySize};
fn create_mixed_alignment_enum(type_cache: &mut TypeCache) -> TypeRef {
    let mut enum_type = EnumType::new(Node::default(), "TestEnum", TopLevelSymbolId::new_illegal(), vec!["test".to_string()]);

    // Add a simple variant (no payload)
    let empty_variant = EnumVariantType {
        common: EnumVariantCommon {
            symbol_id: TopLevelSymbolId::new_illegal(),
            name: Node::default(),
            assigned_name: "Empty".to_string(),
            container_index: 0,
        },
        payload_type: type_cache.unit(), // Use unit type for empty variant
    };
    let _ = enum_type
        .variants
        .insert("Empty".to_string(), empty_variant);

    // Add a variant with a single int field (4 bytes, aligned to 4)
    let int_type = type_cache.int();
    let mut int_fields = SeqMap::new();
    let _ = int_fields.insert(
        "value".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type,
        },
    );

    // Create the anonymous struct type first
    let anon_struct = AnonymousStructType::new(int_fields);
    let anon_struct_type = type_cache.anonymous_struct(anon_struct);

    let int_variant = EnumVariantType {
        common: EnumVariantCommon {
            symbol_id: TopLevelSymbolId::new_illegal(),
            name: Node::default(),
            assigned_name: "Int".to_string(),
            container_index: 1,
        },
        payload_type: anon_struct_type,
    };
    let _ = enum_type.variants.insert("Int".to_string(), int_variant);

    // Add a variant with a struct containing mixed alignments
    let mut mixed_fields = SeqMap::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();
    let float_type = type_cache.float();

    let _ = mixed_fields.insert(
        "i".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type, // 4 bytes, aligned to 4
        },
    );
    let _ = mixed_fields.insert(
        "b".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type, // 1 byte, align 1
        },
    );
    let _ = mixed_fields.insert(
        "f".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: float_type, // 4 bytes, aligned to 4
        },
    );

    // Create the anonymous struct type first
    let anon_struct = AnonymousStructType::new(mixed_fields);
    let anon_struct_type = type_cache.anonymous_struct(anon_struct);

    let mixed_variant = EnumVariantType {
        common: EnumVariantCommon {
            symbol_id: TopLevelSymbolId::new_illegal(),
            name: Node::default(),
            assigned_name: "Mixed".to_string(),
            container_index: 2,
        },
        payload_type: anon_struct_type,
    };
    let _ = enum_type
        .variants
        .insert("Mixed".to_string(), mixed_variant);

    TypeRef::from(type_cache.enum_type(enum_type))
}

#[test]
pub fn test_struct_with_mixed_alignments() {
    let mut type_cache = TypeCache::new();

    let mut fields = SeqMap::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();
    let float_type = type_cache.float();

    // Field order: int (4 bytes, align 4), bool (1 byte, align 1), float (4 bytes, align 4)
    let _ = fields.insert(
        "int_field".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type,
        },
    );
    let _ = fields.insert(
        "bool_field".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type,
        },
    );
    let _ = fields.insert(
        "float_field".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: float_type,
        },
    );

    let struct_type = AnonymousStructType::new(fields);
    let struct_ref = TypeRef::from(type_cache.anonymous_struct(struct_type));

    let mut layout_cache = LayoutCache::new();
    let struct_layout = layout_cache.layout(&struct_ref);

    // In C ABI, this would be laid out as:
    // int_field:   offset 0, size 4
    // bool_field:  offset 4, size 1
    // padding:     offset 5, size 3 (to align float_field to 4-byte boundary)
    // float_field: offset 8, size 4
    // total size:  12 bytes, aligned to 4
    match &struct_layout.kind {
        BasicTypeKind::Struct(s) => {
            // Check field count
            assert_eq!(s.fields.len(), 3);

            // Check field offsets
            assert_eq!(s.fields[0].name, "int_field");
            assert_eq!(s.fields[0].offset, MemoryOffset(0));
            assert_eq!(s.fields[0].size, MemorySize(4));

            assert_eq!(s.fields[1].name, "bool_field");
            assert_eq!(s.fields[1].offset, MemoryOffset(4));
            assert_eq!(s.fields[1].size, MemorySize(1));

            assert_eq!(s.fields[2].name, "float_field");
            assert_eq!(s.fields[2].offset, MemoryOffset(8));
            assert_eq!(s.fields[2].size, MemorySize(4));

            assert_eq!(s.total_size, MemorySize(12));
            assert_eq!(s.max_alignment, MemoryAlignment::U32);
        }
        _ => panic!("Expected struct type"),
    }
}

#[test]
pub fn test_struct_with_trailing_padding() {
    let mut type_cache = TypeCache::new();
    let mut fields = SeqMap::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();

    // Field order: int (4 bytes, align 4), bool (1 byte, align 1)
    let _ = fields.insert(
        "int_field".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type,
        },
    );
    let _ = fields.insert(
        "bool_field".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type,
        },
    );

    let struct_type = AnonymousStructType::new(fields);
    let struct_ref = TypeRef::from(type_cache.anonymous_struct(struct_type));

    let mut layout_cache = LayoutCache::new();
    let struct_layout = layout_cache.layout(&struct_ref);

    // In C ABI, this would be laid out as:
    // int_field:  offset 0, size 4
    // bool_field: offset 4, size 1
    // padding:    offset 5, size 3 (trailing padding to maintain 4-byte alignment)
    // total size: 8 bytes, aligned to 4
    match &struct_layout.kind {
        BasicTypeKind::Struct(s) => {
            assert_eq!(s.fields.len(), 2);

            assert_eq!(s.fields[0].name, "int_field");
            assert_eq!(s.fields[0].offset, MemoryOffset(0));

            assert_eq!(s.fields[1].name, "bool_field");
            assert_eq!(s.fields[1].offset, MemoryOffset(4));

            // Check total size and alignment
            assert_eq!(s.total_size, MemorySize(8)); // Should include trailing padding
            assert_eq!(s.max_alignment, MemoryAlignment::U32);
        }
        _ => panic!("Expected struct type"),
    }
}

#[test]
pub fn test_nested_struct_alignment() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();
    let float_type = type_cache.float();

    // Create an inner struct with mixed alignments
    let mut inner_fields = SeqMap::new();
    let _ = inner_fields.insert(
        "i".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type, // 4 bytes, align 4
        },
    );
    let _ = inner_fields.insert(
        "b".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type.clone(), // 1 byte, align 1
        },
    );

    let inner_struct = AnonymousStructType::new(inner_fields);
    let inner_struct_type = type_cache.anonymous_struct(inner_struct);

    // Create an outer struct containing the inner struct and another field
    let mut outer_fields = SeqMap::new();
    let _ = outer_fields.insert(
        "c".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type, // 1 byte, align 1
        },
    );
    let _ = outer_fields.insert(
        "inner".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: inner_struct_type,
        },
    );
    let _ = outer_fields.insert(
        "f".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: float_type, // 4 bytes, align 4
        },
    );

    let outer_struct = AnonymousStructType::new(outer_fields);
    let outer_struct_ref = TypeRef::from(type_cache.anonymous_struct(outer_struct));

    let mut layout_cache = LayoutCache::new();
    let outer_layout = layout_cache.layout(&outer_struct_ref);

    // In C ABI, this would be laid out as:
    // c:                offset 0, size 1
    // padding:          offset 1, size 3 (to align inner struct to 4-byte boundary)
    // inner.i:          offset 4, size 4
    // inner.b:          offset 8, size 1
    // padding:          offset 9, size 3 (to align f to 4-byte boundary)
    // f:                offset 12, size 4
    // total size:       16 bytes, aligned to 4
    match &outer_layout.kind {
        BasicTypeKind::Struct(s) => {
            assert_eq!(s.fields.len(), 3);

            assert_eq!(s.fields[0].name, "c");
            assert_eq!(s.fields[0].offset, MemoryOffset(0));

            assert_eq!(s.fields[1].name, "inner");
            assert_eq!(s.fields[1].offset, MemoryOffset(4));

            // Check inner struct fields
            match &s.fields[1].ty.kind {
                BasicTypeKind::Struct(inner) => {
                    assert_eq!(inner.fields.len(), 2);
                    assert_eq!(inner.fields[0].name, "i");
                    assert_eq!(inner.fields[0].offset, MemoryOffset(0));
                    assert_eq!(inner.fields[1].name, "b");
                    assert_eq!(inner.fields[1].offset, MemoryOffset(4));
                    assert_eq!(inner.total_size, MemorySize(8));
                }
                _ => panic!("Expected struct type for inner"),
            }

            assert_eq!(s.fields[2].name, "f");
            assert_eq!(s.fields[2].offset, MemoryOffset(12));

            assert_eq!(s.total_size, MemorySize(16));
            assert_eq!(s.max_alignment, MemoryAlignment::U32);
        }
        _ => panic!("Expected struct type"),
    }
}

#[test]
pub fn test_tuple_alignment() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();
    let float_type = type_cache.float();

    // Create a tuple with mixed alignments: (int, bool, float)
    let types = vec![int_type, bool_type, float_type];
    let tuple_ref = type_cache.tuple(types);

    let mut layout_cache = LayoutCache::new();
    let tuple_layout = layout_cache.layout(&tuple_ref);

    // In C ABI, this would be:
    // field 0 (int):   offset 0, size 4
    // field 1 (bool):  offset 4, size 1
    // padding:         offset 5, size 3 (to align field 2 to 4-byte boundary)
    // field 2 (float): offset 8, size 4
    // total size:      12 bytes, aligned to 4
    match &tuple_layout.kind {
        BasicTypeKind::Tuple(t) => {
            assert_eq!(t.fields.len(), 3);

            assert_eq!(t.fields[0].offset, MemoryOffset(0));
            assert_eq!(t.fields[0].size, MemorySize(4));

            assert_eq!(t.fields[1].offset, MemoryOffset(4));
            assert_eq!(t.fields[1].size, MemorySize(1));

            assert_eq!(t.fields[2].offset, MemoryOffset(8));
            assert_eq!(t.fields[2].size, MemorySize(4));

            assert_eq!(t.total_size, MemorySize(12));
            assert_eq!(t.max_alignment, MemoryAlignment::U32);
        }
        _ => panic!("Expected tuple type"),
    }
}

#[test]
pub fn test_tagged_union_layout() {
    let mut type_cache = TypeCache::new();

    let enum_ref = create_mixed_alignment_enum(&mut type_cache);

    let mut layout_cache = LayoutCache::new();
    let enum_layout = layout_cache.layout(&enum_ref);

    // In C ABI, a tagged union should be:
    // tag:     offset 0, size 1 (for 3 variants)
    // padding: offset 1, size 3 (to align payload to 4-byte boundary)
    // payload: offset 4, size of largest variant (which is the Mixed variant)
    match &enum_layout.kind {
        BasicTypeKind::TaggedUnion(tu) => {
            assert_eq!(tu.variants.len(), 3);

            assert_eq!(tu.tag_offset, MemoryOffset(0));
            assert_eq!(tu.tag_size, MemorySize(1));

            assert_eq!(tu.payload_offset, MemoryOffset(4));

            let mixed_variant = tu
                .variants
                .iter()
                .find(|v| v.name == "Mixed")
                .expect("Mixed variant not found");

            match &mixed_variant.ty.kind {
                BasicTypeKind::Struct(s) => {
                    assert_eq!(s.fields.len(), 3);

                    assert_eq!(s.fields[0].name, "i");
                    assert_eq!(s.fields[0].offset, MemoryOffset(0));

                    assert_eq!(s.fields[1].name, "b");
                    assert_eq!(s.fields[1].offset, MemoryOffset(4));

                    assert_eq!(s.fields[2].name, "f");
                    assert_eq!(s.fields[2].offset, MemoryOffset(8));

                    assert_eq!(s.total_size, MemorySize(12));
                }
                _ => panic!("Expected struct type for Mixed variant"),
            }

            let expected_min_size = tu.payload_offset.0 + tu.payload_max_size.0;
            assert!(tu.total_size.0 >= expected_min_size);

            let alignment_value: usize = tu.max_alignment.into();

            assert_eq!(tu.total_size.0 % (alignment_value as u32), 0);
        }
        _ => panic!("Expected tagged union type"),
    }
}

#[test]
pub fn test_complex_nested_structure() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();
    let float_type = type_cache.float();

    // Create a tuple type (bool, int)
    let tuple_types = vec![bool_type.clone(), int_type];
    let tuple_type = type_cache.tuple(tuple_types);

    // Create a struct with the tuple and other fields
    let mut struct_fields = SeqMap::new();
    let _ = struct_fields.insert(
        "f".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: float_type,
        },
    );
    let _ = struct_fields.insert(
        "t".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: tuple_type,
        },
    );
    let _ = struct_fields.insert(
        "b".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type,
        },
    );

    let struct_type = AnonymousStructType::new(struct_fields);
    let struct_ref = TypeRef::from(type_cache.anonymous_struct(struct_type));

    let mut layout_cache = LayoutCache::new();
    let layout = layout_cache.layout(&struct_ref);

    // C ABI:
    // f:          offset 0, size 4
    // t.0 (bool): offset 4, size 1
    // padding:    offset 5, size 3 (to align t.1 to 4-byte boundary)
    // t.1 (int): offset 8, size 4
    // b:           offset 12, size 1
    // padding:     offset 13, size 3 (trailing padding to maintain 4-byte alignment)
    // total size:  16 bytes, aligned to 4
    match &layout.kind {
        BasicTypeKind::Struct(s) => {
            assert_eq!(s.fields.len(), 3);

            assert_eq!(s.fields[0].name, "f");
            assert_eq!(s.fields[0].offset, MemoryOffset(0));

            assert_eq!(s.fields[1].name, "t");
            assert_eq!(s.fields[1].offset, MemoryOffset(4));

            // Check tuple fields
            match &s.fields[1].ty.kind {
                BasicTypeKind::Tuple(t) => {
                    assert_eq!(t.fields.len(), 2);
                    assert_eq!(t.fields[0].offset, MemoryOffset(0));
                    assert_eq!(t.fields[0].size, MemorySize(1));

                    assert_eq!(t.fields[1].offset, MemoryOffset(4));
                    assert_eq!(t.fields[1].size, MemorySize(4));

                    assert_eq!(t.total_size, MemorySize(8)); // 1 + 3 padding + 4
                }
                _ => panic!("Expected tuple type"),
            }

            assert_eq!(s.fields[2].name, "b");
            assert_eq!(s.fields[2].offset, MemoryOffset(12));

            assert_eq!(s.total_size, MemorySize(16)); // Should include trailing padding
            assert_eq!(s.max_alignment, MemoryAlignment::U32);
        }
        _ => panic!("Expected struct type"),
    }
}

#[test]
pub fn test_optional_type_layout() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let optional_int = type_cache.optional(&int_type);

    let mut layout_cache = LayoutCache::new();
    let optional_layout = layout_cache.layout(&optional_int);

    // C ABI layout:
    // tag:     offset 0, size 1 (for 2 variants)
    // padding: offset 1, size 3 (to align payload to 4-byte boundary)
    // payload: offset 4, size 4 (for int)
    // total:   8 bytes, aligned to 4
    match &optional_layout.kind {
        BasicTypeKind::Optional(tu) => {
            assert_eq!(tu.variants.len(), 2);
            assert_eq!(tu.variants[0].name, "None");
            assert_eq!(tu.variants[1].name, "Some");

            assert_eq!(tu.tag_offset, MemoryOffset(0));
            assert_eq!(tu.tag_size, MemorySize(1));

            assert_eq!(tu.payload_offset, MemoryOffset(4));

            assert_eq!(tu.payload_max_size, MemorySize(4));

            match &tu.variants[1].ty.kind {
                BasicTypeKind::S32 => {}
                _ => panic!("Expected S32 type for Some variant"),
            }

            assert_eq!(tu.total_size, MemorySize(8)); // tag(1) + padding(3) + payload(4)
            assert_eq!(tu.max_alignment, MemoryAlignment::U32);
        }
        _ => panic!("Expected Optional type"),
    }
}

#[test]
pub fn test_empty_struct_layout() {
    let mut type_cache = TypeCache::new();

    let empty_struct = AnonymousStructType::new(SeqMap::new());
    let empty_struct_ref = TypeRef::from(type_cache.anonymous_struct(empty_struct));

    let mut layout_cache = LayoutCache::new();
    let layout = layout_cache.layout(&empty_struct_ref);

    // Note, here is a difference towards C ABI
    // In C ABI an empty struct typically has size 1
    // but may have different sizes depending on the compiler?
    // But we are conforming towards Rust and with "Zero-Sized Types"
    // https://doc.rust-lang.org/nomicon/exotic-sizes.html#zero-sized-types-zsts
    // So even in a `#[repr(C)]` it gets zero size
    match &layout.kind {
        BasicTypeKind::Struct(s) => {
            assert_eq!(s.fields.len(), 0);

            assert_eq!(s.total_size, MemorySize(0));
            assert_eq!(s.max_alignment, MemoryAlignment::U8);
        }
        _ => panic!("Expected struct type"),
    }
}

#[test]
pub fn test_struct_with_max_alignment_field_at_end() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();

    // Create a struct with fields: bool, bool, int
    let mut fields = SeqMap::new();
    let _ = fields.insert(
        "b1".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type.clone(), // 1 byte, align 1
        },
    );
    let _ = fields.insert(
        "b2".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type, // 1 byte, align 1
        },
    );
    let _ = fields.insert(
        "i".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type, // 4 bytes, align 4
        },
    );

    let struct_type = AnonymousStructType::new(fields);
    let struct_ref = TypeRef::from(type_cache.anonymous_struct(struct_type));

    let mut layout_cache = LayoutCache::new();
    let layout = layout_cache.layout(&struct_ref);

    // C ABI:
    // b1:      offset 0, size 1
    // b2:      offset 1, size 1
    // padding: offset 2, size 2 (to align i to 4-byte boundary)
    // i:       offset 4, size 4
    // total:   8 bytes, aligned to 4
    match &layout.kind {
        BasicTypeKind::Struct(s) => {
            assert_eq!(s.fields.len(), 3);

            assert_eq!(s.fields[0].name, "b1");
            assert_eq!(s.fields[0].offset, MemoryOffset(0));

            assert_eq!(s.fields[1].name, "b2");
            assert_eq!(s.fields[1].offset, MemoryOffset(1));

            assert_eq!(s.fields[2].name, "i");
            assert_eq!(s.fields[2].offset, MemoryOffset(4));

            assert_eq!(s.total_size, MemorySize(8));
            assert_eq!(s.max_alignment, MemoryAlignment::U32);
        }
        _ => panic!("Expected struct type"),
    }
}

/// This test specifically checks that the two-pass layout algorithm is working correctly.
/// It creates a scenario where a single-pass algorithm would produce incorrect field offsets.
///
/// You need to know the maximum alignment of all fields before you can correctly position any field.
#[test]
pub fn test_two_pass_layout_requirement() {
    let mut type_cache = TypeCache::new();

    let bool_type = type_cache.bool();   // 1 byte, align 1
    let int_type = type_cache.int();     // 4 bytes, align 4

    // Create a struct where the field with the highest alignment requirement comes last
    // This is the critical case that exposes single-pass vs two-pass differences
    let mut fields = SeqMap::new();
    let _ = fields.insert(
        "byte1".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type.clone(), // 1 byte, align 1
        },
    );
    let _ = fields.insert(
        "byte2".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type.clone(), // 1 byte, align 1
        },
    );
    let _ = fields.insert(
        "byte3".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: bool_type, // 1 byte, align 1
        },
    );
    let _ = fields.insert(
        "highest_align".to_string(),
        StructTypeField {
            symbol_id: TopLevelSymbolId::new_illegal(),
            identifier: None,
            field_type: int_type, // 4 bytes, align 4 - HIGHEST ALIGNMENT
        },
    );

    let struct_type = AnonymousStructType::new(fields);
    let struct_ref = TypeRef::from(type_cache.anonymous_struct(struct_type));

    let mut layout_cache = LayoutCache::new();
    let layout = layout_cache.layout(&struct_ref);

    match &layout.kind {
        BasicTypeKind::Struct(s) => {
            assert_eq!(s.fields.len(), 4);

            // With CORRECT two-pass layout (what we expect):
            // - First pass determines max_alignment = 4 (from the int field)
            // - Second pass lays out fields knowing the struct needs 4-byte alignment

            // byte1: offset 0, size 1
            assert_eq!(s.fields[0].name, "byte1");
            assert_eq!(s.fields[0].offset, MemoryOffset(0));
            assert_eq!(s.fields[0].size, MemorySize(1));

            // byte2: offset 1, size 1
            assert_eq!(s.fields[1].name, "byte2");
            assert_eq!(s.fields[1].offset, MemoryOffset(1));
            assert_eq!(s.fields[1].size, MemorySize(1));

            // byte3: offset 2, size 1
            assert_eq!(s.fields[2].name, "byte3");
            assert_eq!(s.fields[2].offset, MemoryOffset(2));
            assert_eq!(s.fields[2].size, MemorySize(1));

            // highest_align: offset 4, size 4 (aligned to 4-byte boundary)
            // This is the CRITICAL test - the int must be at offset 4, not 3
            assert_eq!(s.fields[3].name, "highest_align");
            assert_eq!(s.fields[3].offset, MemoryOffset(4));
            assert_eq!(s.fields[3].size, MemorySize(4));

            // Total size: 8 bytes (4 for int + 4 for the three bytes + padding)
            assert_eq!(s.total_size, MemorySize(8));

            // Max alignment: 4 bytes (determined by the int field)
            assert_eq!(s.max_alignment, MemoryAlignment::U32);
        }
        _ => panic!("Expected struct type"),
    }
}
