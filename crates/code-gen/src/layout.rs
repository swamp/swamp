use crate::alloc::ScopeAllocator;
use crate::{Error, FrameAndVariableInfo, reserve};
use seq_map::SeqMap;
use source_map_node::Node;
use swamp_semantic::VariableRef;
use swamp_types::{AnonymousStructType, EnumVariantType, Type};
use swamp_vm_debug_types::{
    BasicType, BasicTypeKind, FrameAddressInfo, FrameAddressInfoKind, FrameMemoryInfo,
    OffsetMemoryItem, StructType, TaggedUnion, TaggedUnionData, TaggedUnionDataKind, TupleType,
    VariableInfo,
};
use swamp_vm_types::{
    FrameMemoryAddress, FrameMemoryRegion, HEAP_PTR_ALIGNMENT, MemoryAlignment, MemoryOffset,
    MemorySize, PTR_SIZE, adjust_size_to_alignment, align_to,
};
use tracing::trace;

pub fn type_size_and_alignment(ty: &Type) -> (MemorySize, MemoryAlignment) {
    let complex_type = layout_type(&ty, MemoryOffset(0), "size_and_alignment");

    (complex_type.total_size, complex_type.total_alignment)
}

#[allow(clippy::too_many_lines)]
pub fn layout_enum_into_tagged_union(
    name: &str,
    variants: &[EnumVariantType],
    base_offset: MemoryOffset,
) -> TaggedUnion {
    // These are redundant types that
    // just reflects the temporary status
    // so it is not confused with the final types
    // TODO: maybe not worth it?
    enum VariantPayloadLayout {
        Struct(StructType),
        Tuple(TupleType),
        Empty,
    }

    struct VariantLayout {
        name: String,
        payload: VariantPayloadLayout,
    }

    let mut max_payload_alignment = MemoryAlignment::U8;
    let mut max_payload_size = MemorySize(0);

    let mut variant_layouts = Vec::with_capacity(variants.len());

    for variant in variants {
        match variant {
            EnumVariantType::Struct(s) => {
                let struct_type =
                    layout_struct_type(&s.anon_struct, MemoryOffset(0), &s.common.assigned_name);
                if struct_type.total_alignment > max_payload_alignment {
                    max_payload_alignment = struct_type.total_alignment;
                }
                if struct_type.total_size > max_payload_size {
                    max_payload_size = struct_type.total_size;
                }
                variant_layouts.push(VariantLayout {
                    name: s.common.assigned_name.clone(),
                    payload: VariantPayloadLayout::Struct(struct_type),
                });
            }
            EnumVariantType::Tuple(t) => {
                let tuple_type = layout_tuple_items(&t.fields_in_order, MemoryOffset(0));
                if tuple_type.total_alignment > max_payload_alignment {
                    max_payload_alignment = tuple_type.total_alignment;
                }
                if tuple_type.total_size > max_payload_size {
                    max_payload_size = tuple_type.total_size;
                }
                variant_layouts.push(VariantLayout {
                    name: t.common.assigned_name.clone(),
                    payload: VariantPayloadLayout::Tuple(tuple_type),
                });
            }
            EnumVariantType::Nothing(n) => {
                variant_layouts.push(VariantLayout {
                    name: n.common.assigned_name.clone(),
                    payload: VariantPayloadLayout::Empty,
                });
            }
        }
    }

    let tag_size = MemorySize(if variants.len() <= 0xFF {
        1
    } else if variants.len() <= 0xFFFF {
        2
    } else {
        4
    });
    let tag_alignment = match tag_size.0 {
        1 => MemoryAlignment::U8,
        2 => MemoryAlignment::U16,
        4 => MemoryAlignment::U32,
        _ => panic!("Unsupported tag size"),
    };

    let payload_offset = align_to(base_offset + tag_size, max_payload_alignment);

    // Second pass: Layout each variant at the correct payload offset
    let mut tagged_variants = Vec::with_capacity(variants.len());
    for v in &variant_layouts {
        match &v.payload {
            VariantPayloadLayout::Struct(_) => {
                let struct_type = layout_struct_type(
                    match variants.iter().find(|var| match var {
                        EnumVariantType::Struct(s) => s.common.assigned_name == v.name,
                        _ => false,
                    }) {
                        Some(EnumVariantType::Struct(s)) => &s.anon_struct,
                        _ => panic!("Struct variant not found"),
                    },
                    payload_offset,
                    &v.name,
                );
                tagged_variants.push(TaggedUnionData {
                    name: v.name.clone(),
                    kind: TaggedUnionDataKind::Struct(struct_type),
                });
            }
            VariantPayloadLayout::Tuple(_) => {
                let tuple_type = layout_tuple_items(
                    match variants.iter().find(|var| match var {
                        EnumVariantType::Tuple(t) => t.common.assigned_name == v.name,
                        _ => false,
                    }) {
                        Some(EnumVariantType::Tuple(t)) => &t.fields_in_order,
                        _ => panic!("Tuple variant not found"),
                    },
                    payload_offset,
                );
                tagged_variants.push(TaggedUnionData {
                    name: v.name.clone(),
                    kind: TaggedUnionDataKind::Tuple(tuple_type),
                });
            }
            VariantPayloadLayout::Empty => {
                tagged_variants.push(TaggedUnionData {
                    name: v.name.clone(),
                    kind: TaggedUnionDataKind::Empty,
                });
            }
        }
    }

    let total_size = (align_to(payload_offset + max_payload_size, max_payload_alignment)
        - base_offset)
        .to_size();
    let total_alignment = std::cmp::max(tag_alignment, max_payload_alignment);

    TaggedUnion {
        name: name.to_string(),
        tag_offset: MemoryOffset(0),
        tag_size,
        variants: tagged_variants,
        total_size,
        max_alignment: total_alignment,
    }
}

pub fn layout_enum(
    name: &str,
    variants: &[EnumVariantType],
    base_offset: MemoryOffset,
) -> BasicType {
    let tagged_union = layout_enum_into_tagged_union(name, variants, base_offset);

    BasicType {
        total_size: tagged_union.total_size,
        total_alignment: tagged_union.max_alignment,
        kind: BasicTypeKind::TaggedUnion(tagged_union),
    }
}

fn layout_slice_pair(base_offset: MemoryOffset, a_type: &Type, b_type: &Type) -> BasicType {
    let a_info = layout_type(a_type, base_offset, "a");
    let b_info = layout_type(b_type, base_offset, "b");

    let b_offset = align_to(base_offset + a_info.total_size, b_info.total_alignment);
    let b_layout = layout_type(b_type, b_offset, "b");

    let max_alignment = std::cmp::max(a_info.total_alignment, b_layout.total_alignment);

    let end = b_offset + b_layout.total_size;
    let total_size = align_to(end, max_alignment) - base_offset;

    BasicType {
        kind: BasicTypeKind::SlicePair(Box::new(a_info), Box::new(b_layout)),
        total_size: MemorySize(total_size.0),
        total_alignment: max_alignment,
    }
}

fn basic_type_as_complex(
    basic_type_kind: BasicTypeKind,
    size: MemorySize,
    alignment: MemoryAlignment,
) -> BasicType {
    BasicType {
        kind: basic_type_kind,
        total_size: size,
        total_alignment: alignment,
    }
}

#[must_use]
pub fn layout_type(ty: &Type, memory_offset: MemoryOffset, name: &str) -> BasicType {
    match ty {
        Type::Int => basic_type_as_complex(BasicTypeKind::S32, MemorySize(4), MemoryAlignment::U32),
        Type::Float => {
            basic_type_as_complex(BasicTypeKind::Fixed32, MemorySize(4), MemoryAlignment::U32)
        }
        Type::Bool => basic_type_as_complex(BasicTypeKind::B8, MemorySize(1), MemoryAlignment::U8),
        Type::Unit => {
            basic_type_as_complex(BasicTypeKind::Empty, MemorySize(0), MemoryAlignment::U8)
        }
        Type::String => basic_type_as_complex(
            BasicTypeKind::CollectionPointer,
            MemorySize(PTR_SIZE),
            HEAP_PTR_ALIGNMENT,
        ),
        Type::Slice(inner_type) => {
            let basic = layout_type(inner_type, memory_offset, "slice");
            BasicType {
                kind: BasicTypeKind::Slice(Box::from(basic.clone())),
                total_size: basic.total_size,
                total_alignment: basic.total_alignment,
            }
        }
        Type::SlicePair(a, b) => layout_slice_pair(memory_offset, a, b),
        Type::Tuple(types) => layout_tuple(types, memory_offset),
        Type::NamedStruct(named_struct_type) => layout_struct(
            &named_struct_type.anon_struct_type,
            memory_offset,
            &named_struct_type.assigned_name,
        ),
        Type::AnonymousStruct(anon_struct_type) => {
            layout_struct(anon_struct_type, memory_offset, "")
        }
        Type::Enum(a) => layout_enum(
            &a.assigned_name,
            &a.variants.values().cloned().collect::<Vec<_>>(),
            memory_offset,
        ),
        Type::Optional(inner_type) => layout_optional_type(inner_type, memory_offset, "?"),
        Type::Function(_) => panic!("function types should not be a part of codegen"),
        Type::Never => panic!("'never' should not be a part of codegen"),
        Type::Generic(_, _) => panic!("generic should not be a part of codegen"),
        Type::Blueprint(_) => panic!("blueprint should not be a part of codegen"),
        Type::Variable(_) => panic!("type variable (generics) should not be a part of codegen"),
        Type::MutableReference(inner_type) => layout_type(inner_type, memory_offset, "mut ref"),
    }
}

pub fn layout_struct_type(
    struct_type: &AnonymousStructType,
    base_offset: MemoryOffset,
    name: &str,
) -> StructType {
    let mut offset = base_offset;
    let mut max_alignment = MemoryAlignment::U8;
    let mut items = Vec::with_capacity(struct_type.field_name_sorted_fields.len());

    for (field_name, field_type) in &struct_type.field_name_sorted_fields {
        // Recursively layout the field at the current offset
        let field_layout = layout_type(&field_type.field_type, offset, field_name);

        // Align the offset for this field
        offset = align_to(offset, field_layout.total_alignment);

        items.push(OffsetMemoryItem {
            offset,
            size: field_layout.total_size,
            name: field_name.clone(),
            ty: field_layout.clone(),
        });

        // Advance offset for next field
        offset = offset + field_layout.total_size;

        // Track max alignment
        if field_layout.total_alignment > max_alignment {
            max_alignment = field_layout.total_alignment;
        }
    }

    let total_size = adjust_size_to_alignment(offset, base_offset, max_alignment);

    StructType {
        name: name.to_string(),
        fields: items,
        total_size,
        total_alignment: max_alignment,
    }
}

pub fn layout_struct(
    struct_type: &AnonymousStructType,
    base_offset: MemoryOffset,
    name: &str,
) -> BasicType {
    let inner_struct = layout_struct_type(struct_type, base_offset, name);
    BasicType {
        total_size: inner_struct.total_size,
        total_alignment: inner_struct.total_alignment,
        kind: BasicTypeKind::Struct(inner_struct),
    }
}

pub fn layout_optional_type(inner_type: &Type, base_offset: MemoryOffset, name: &str) -> BasicType {
    let tuple_type = layout_optional_type_items(inner_type, base_offset, name);
    BasicType {
        total_size: tuple_type.total_size,
        total_alignment: tuple_type.total_alignment,
        kind: BasicTypeKind::Optional(Box::from(BasicType {
            total_size: tuple_type.total_size,
            total_alignment: tuple_type.total_alignment,
            kind: BasicTypeKind::Tuple(tuple_type),
        })),
    }
}

pub fn layout_optional_type_items(
    inner_type: &Type,
    base_offset: MemoryOffset,
    name: &str,
) -> TupleType {
    let tag_size = MemorySize(1);
    let tag_alignment = MemoryAlignment::U8;

    let payload_layout = layout_type(inner_type, MemoryOffset(0), name);
    let payload_offset = align_to(base_offset + tag_size, payload_layout.total_alignment);
    let max_alignment = std::cmp::max(tag_alignment, payload_layout.total_alignment);
    let end_offset = payload_offset + payload_layout.total_size;

    let total_size = adjust_size_to_alignment(end_offset, base_offset, max_alignment);

    let items = vec![
        OffsetMemoryItem {
            offset: base_offset,
            size: tag_size,
            name: "tag".to_string(),
            ty: BasicType {
                kind: BasicTypeKind::U8,
                total_size: tag_size,
                total_alignment: tag_alignment,
            },
        },
        OffsetMemoryItem {
            offset: payload_offset,
            size: payload_layout.total_size,
            name: "payload".to_string(),
            ty: payload_layout.clone(),
        },
    ];

    TupleType {
        fields: items,
        total_size,
        total_alignment: max_alignment,
    }
}

#[must_use]
pub fn layout_types(types: &[Type], memory_offset: MemoryOffset) -> Vec<BasicType> {
    let mut converted = Vec::new();
    let mut max_alignment = MemoryAlignment::U8;
    for ty in types {
        let complex_type = layout_type(ty, memory_offset, "not sure");
        if complex_type.total_alignment.greater_than(max_alignment) {
            max_alignment = complex_type.total_alignment;
        }
        converted.push(complex_type);
    }

    converted
}

pub fn layout_tuple_items(types: &[Type], base_offset: MemoryOffset) -> TupleType {
    let mut offset = base_offset;
    let mut max_alignment = MemoryAlignment::U8;
    let mut items = Vec::with_capacity(types.len());

    for (i, ty) in types.iter().enumerate() {
        let elem_layout = layout_type(ty, offset, &i.to_string());

        offset = align_to(offset, elem_layout.total_alignment);

        items.push(OffsetMemoryItem {
            offset,
            size: elem_layout.total_size,
            name: i.to_string(),
            ty: elem_layout.clone(),
        });

        offset = offset + elem_layout.total_size;

        if elem_layout.total_alignment > max_alignment {
            max_alignment = elem_layout.total_alignment;
        }
    }

    let total_size = adjust_size_to_alignment(offset, base_offset, max_alignment);

    TupleType {
        fields: items,
        total_size,
        total_alignment: max_alignment,
    }
}

pub fn layout_tuple(types: &[Type], base_offset: MemoryOffset) -> BasicType {
    let tuple_type = layout_tuple_items(&types, base_offset);

    BasicType {
        total_size: tuple_type.total_size,
        total_alignment: tuple_type.total_alignment,
        kind: BasicTypeKind::Tuple(tuple_type),
    }
}

/// # Errors
///
pub fn layout_variables(
    node: &Node,
    variables: &Vec<VariableRef>,
    return_type: &Type,
) -> Result<FrameAndVariableInfo, Error> {
    let mut allocator = ScopeAllocator::new(FrameMemoryRegion::new(
        FrameMemoryAddress(0),
        MemorySize(32 * 1024),
    ));
    let _current_offset = reserve(return_type, &mut allocator);

    let mut enter_comment = "variables:\n".to_string();

    let mut frame_memory_infos = Vec::new();
    let mut variable_offsets = SeqMap::new();

    for var_ref in variables {
        let var_target = reserve(&var_ref.resolved_type, &mut allocator);
        trace!(?var_ref.assigned_name, ?var_target, "laying out");
        enter_comment += &format!(
            "  ${:04X}:{} {}\n",
            var_target.addr.0, var_target.size.0, var_ref.assigned_name
        );

        frame_memory_infos.push(FrameAddressInfo {
            region: var_target,
            kind: FrameAddressInfoKind::Variable(VariableInfo {
                is_mutable: var_ref.is_mutable(),
                name: var_ref.assigned_name.clone(),
                ty: layout_type(
                    &var_ref.resolved_type,
                    MemoryOffset(0),
                    &var_ref.assigned_name,
                ),
            }),
        });

        variable_offsets
            .insert(var_ref.unique_id_within_function, var_target)
            .unwrap();
    }

    //let extra_frame_size = MemorySize(80);
    let frame_size = allocator.addr().as_size();

    let result = FrameAndVariableInfo {
        frame_memory: FrameMemoryInfo {
            infos: frame_memory_infos,
            size: frame_size,
        },
        variable_offsets,
    };

    Ok(result)
}
