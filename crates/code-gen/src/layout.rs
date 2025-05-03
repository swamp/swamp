use crate::alloc::ScopeAllocator;
use crate::{FrameAndVariableInfo, reserve};
use seq_map::SeqMap;
use source_map_node::Node;
use std::fmt::Write;
use swamp_semantic::{VariableRef, VariableType};
use swamp_types::{AnonymousStructType, EnumVariantType, NamedStructType, Type};
use swamp_vm_types::aligner::align;
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, FrameAddressInfo, FrameAddressInfoKind, FrameMemoryInfo,
    OffsetMemoryItem, StructType, TaggedUnion, TaggedUnionVariant, TupleType, VariableInfo,
};
use swamp_vm_types::{
    FrameMemoryAddress, FrameMemoryRegion, HEAP_PTR_ON_FRAME_ALIGNMENT, HEAP_PTR_ON_FRAME_SIZE,
    MAP_HEADER_ALIGNMENT, MAP_HEADER_SIZE, MAP_PTR_ALIGNMENT, MAP_PTR_SIZE, MemoryAlignment,
    MemoryOffset, MemorySize, SLICE_HEADER_ALIGNMENT, SLICE_HEADER_SIZE, SLICE_PAIR_HEADER_SIZE,
    STRING_HEADER_ALIGNMENT, STRING_HEADER_SIZE, STRING_PTR_ALIGNMENT, STRING_PTR_SIZE,
    VEC_HEADER_ALIGNMENT, VEC_HEADER_SIZE, VEC_PTR_ALIGNMENT, VEC_PTR_SIZE,
    adjust_size_to_alignment, align_to,
};
use tracing::trace;
/*
pub fn type_size_and_alignment(ty: &Type) -> (MemorySize, MemoryAlignment) {
    let complex_type = layout_type(ty, "size_and_alignment");

    (complex_type.total_size, complex_type.max_alignment)
}

 */

#[derive(Copy, Clone)]
struct VariantLayout {
    pub size: MemorySize,
    pub alignment: MemoryAlignment,
}

#[derive(Copy, Clone)]
pub struct TaggedUnionLayout {
    pub tag_offset: MemoryOffset,
    pub tag_size: MemorySize,
    pub tag_alignment: MemoryAlignment,
    pub payload_offset: MemoryOffset,
    pub payload_size: MemorySize,
    pub payload_alignment: MemoryAlignment,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

fn layout_tagged_union(variants: &[VariantLayout]) -> TaggedUnionLayout {
    let num_variants = variants.len();
    let (tag_size, tag_alignment) = if num_variants <= 0xFF {
        (MemorySize(1), MemoryAlignment::U8)
    } else if num_variants <= 0xFFFF {
        (MemorySize(2), MemoryAlignment::U16)
    } else {
        (MemorySize(4), MemoryAlignment::U32)
    };

    let max_payload_size = variants
        .iter()
        .map(|v| v.size)
        .max()
        .unwrap_or(MemorySize(0));
    let max_payload_alignment = variants
        .iter()
        .map(|v| v.alignment)
        .max()
        .unwrap_or(MemoryAlignment::U8);

    let payload_offset = align_to(MemoryOffset(tag_size.0), max_payload_alignment);
    let max_alignment = std::cmp::max(tag_alignment, max_payload_alignment);

    let complete_size_before_alignment = MemorySize(payload_offset.0 + max_payload_size.0);
    let total_size = adjust_size_to_alignment(complete_size_before_alignment, max_alignment);

    TaggedUnionLayout {
        tag_offset: MemoryOffset(0),
        tag_size,
        tag_alignment,
        payload_offset,
        payload_size: max_payload_size,
        payload_alignment: max_payload_alignment,
        total_size,
        max_alignment,
    }
}

#[allow(clippy::too_many_lines)]
pub fn layout_enum_into_tagged_union(name: &str, variants: &[EnumVariantType]) -> TaggedUnion {
    let variant_infos = variants.iter().map(|variant| match variant {
        EnumVariantType::Struct(s) => {
            let struct_type = layout_struct_type(&s.anon_struct, &s.common.assigned_name);
            (
                VariantLayout {
                    size: struct_type.total_size,
                    alignment: struct_type.max_alignment,
                },
                TaggedUnionVariant {
                    name: s.common.assigned_name.clone(),
                    ty: BasicType {
                        total_size: struct_type.total_size,
                        max_alignment: struct_type.max_alignment,
                        kind: BasicTypeKind::Struct(struct_type),
                    },
                },
            )
        }
        EnumVariantType::Tuple(t) => {
            let tuple_type = layout_tuple_items(&t.fields_in_order);
            (
                VariantLayout {
                    size: tuple_type.total_size,
                    alignment: tuple_type.max_alignment,
                },
                TaggedUnionVariant {
                    name: t.common.assigned_name.clone(),
                    ty: BasicType {
                        total_size: tuple_type.total_size,
                        max_alignment: tuple_type.max_alignment,
                        kind: BasicTypeKind::Tuple(tuple_type),
                    },
                },
            )
        }

        EnumVariantType::Nothing(n) => (
            VariantLayout {
                size: MemorySize(0),
                alignment: MemoryAlignment::U8,
            },
            TaggedUnionVariant {
                name: n.common.assigned_name.clone(),
                ty: BasicType {
                    total_size: MemorySize(0),
                    max_alignment: MemoryAlignment::U8,
                    kind: BasicTypeKind::Empty,
                },
            },
        ),
    });

    let (variant_layouts, tagged_variants): (Vec<VariantLayout>, Vec<TaggedUnionVariant>) =
        variant_infos.into_iter().unzip();
    let tagged_union_layout = layout_tagged_union(&variant_layouts);

    TaggedUnion {
        name: name.to_string(),
        tag_offset: tagged_union_layout.tag_offset,
        tag_size: tagged_union_layout.tag_size,
        payload_max_size: tagged_union_layout.payload_size,
        payload_offset: tagged_union_layout.payload_offset,
        variants: tagged_variants,
        total_size: tagged_union_layout.total_size,
        max_alignment: tagged_union_layout.max_alignment,
    }
}

pub fn layout_enum(name: &str, variants: &[EnumVariantType]) -> BasicType {
    let tagged_union = layout_enum_into_tagged_union(name, variants);

    BasicType {
        total_size: tagged_union.total_size,
        max_alignment: tagged_union.max_alignment,
        kind: BasicTypeKind::TaggedUnion(tagged_union),
    }
}

fn layout_slice(value_type: &Type, name: &str) -> BasicType {
    let basic = layout_type(value_type);
    BasicType {
        kind: BasicTypeKind::Slice(Box::from(basic)),
        total_size: SLICE_HEADER_SIZE,
        max_alignment: SLICE_HEADER_ALIGNMENT,
    }
}

fn layout_slice_pair(a_type: &Type, b_type: &Type) -> BasicType {
    let tuple_type = layout_tuple_items(&[a_type.clone(), b_type.clone()]);
    let key_type = &tuple_type.fields[0];
    let value_type = &tuple_type.fields[1];

    BasicType {
        kind: BasicTypeKind::SlicePair(Box::new(key_type.clone()), Box::new(value_type.clone())),
        total_size: SLICE_PAIR_HEADER_SIZE,
        max_alignment: SLICE_HEADER_ALIGNMENT,
    }
}

const fn basic_type(
    basic_type_kind: BasicTypeKind,
    size: MemorySize,
    alignment: MemoryAlignment,
) -> BasicType {
    BasicType {
        kind: basic_type_kind,
        total_size: size,
        max_alignment: alignment,
    }
}

#[must_use]
pub fn layout_type(ty: &Type) -> BasicType {
    match ty {
        Type::Int => basic_type(BasicTypeKind::S32, MemorySize(4), MemoryAlignment::U32),
        Type::Float => basic_type(BasicTypeKind::Fixed32, MemorySize(4), MemoryAlignment::U32),
        Type::Bool => basic_type(BasicTypeKind::B8, MemorySize(1), MemoryAlignment::U8),
        Type::Unit => basic_type(BasicTypeKind::Empty, MemorySize(0), MemoryAlignment::U8),
        Type::String => basic_type(
            BasicTypeKind::InternalStringPointer,
            STRING_PTR_SIZE,
            STRING_PTR_ALIGNMENT,
        ),
        Type::Slice(inner_type) => layout_slice(inner_type, "slice"),
        Type::SlicePair(a, b) => layout_slice_pair(a, b),
        Type::Tuple(types) => layout_tuple(types),
        Type::NamedStruct(named_struct_type) => {
            layout_named_struct(named_struct_type) // NOTE: memory_offset removed
        }
        Type::AnonymousStruct(anon_struct_type) => {
            layout_struct(anon_struct_type, "") // NOTE: memory_offset removed
        }
        Type::Enum(a) => layout_enum(
            &a.assigned_name,
            &a.variants.values().cloned().collect::<Vec<_>>(),
        ),
        Type::Optional(inner_type) => layout_optional_type(inner_type),
        Type::MutableReference(inner_type) => layout_mutable_reference(inner_type),
        Type::Function(_) => panic!("function types should not be a part of codegen"),
        Type::Never => panic!("'never' should not be a part of codegen"),
        Type::Generic(_, _) => panic!("generic should not be a part of codegen"),
        Type::Blueprint(_) => panic!("blueprint should not be a part of codegen"),
        Type::Variable(_) => panic!("type variable (generics) should not be a part of codegen"),
    }
}

fn layout_mutable_reference(analyzed_type: &Type) -> BasicType {
    let inner_type = layout_type(analyzed_type);
    BasicType {
        kind: BasicTypeKind::MutablePointer(Box::from(inner_type)),
        total_size: HEAP_PTR_ON_FRAME_SIZE,
        max_alignment: HEAP_PTR_ON_FRAME_ALIGNMENT,
    }
}

fn layout_named_struct(named_struct_type: &NamedStructType) -> BasicType {
    if named_struct_type.is_vec() {
        return basic_type(
            BasicTypeKind::InternalVecPointer(Box::from(layout_type(
                &named_struct_type.instantiated_type_parameters[0],
            ))),
            VEC_PTR_SIZE,
            VEC_PTR_ALIGNMENT,
        );
    }

    if named_struct_type.is_map() {
        let analyzed_key_type = &named_struct_type.instantiated_type_parameters[0];
        assert!(analyzed_key_type.is_concrete());
        let key_type = layout_type(analyzed_key_type);

        let analyzed_value_type = &named_struct_type.instantiated_type_parameters[1];
        assert!(analyzed_value_type.is_concrete());
        let value_type = layout_type(analyzed_value_type);
        return basic_type(
            BasicTypeKind::InternalMapPointer(Box::from(key_type), Box::from(value_type)),
            MAP_PTR_SIZE,
            MAP_PTR_ALIGNMENT,
        );
    }

    if named_struct_type.is_grid() {
        return basic_type(
            BasicTypeKind::InternalVecPointer(Box::from(layout_type(
                &named_struct_type.instantiated_type_parameters[0],
            ))),
            VEC_PTR_SIZE,
            VEC_PTR_ALIGNMENT,
        );
    }
    if named_struct_type.is_stack() {
        return basic_type(
            BasicTypeKind::InternalVecPointer(Box::from(layout_type(
                &named_struct_type.instantiated_type_parameters[0],
            ))),
            VEC_PTR_SIZE,
            VEC_PTR_ALIGNMENT,
        );
    }

    if named_struct_type.is_queue() {
        return basic_type(
            BasicTypeKind::InternalVecPointer(Box::from(layout_type(
                &named_struct_type.instantiated_type_parameters[0],
            ))),
            VEC_PTR_SIZE,
            VEC_PTR_ALIGNMENT,
        );
    }

    layout_struct(
        &named_struct_type.anon_struct_type,
        //memory_offset,
        &named_struct_type.assigned_name,
    )
}

pub fn layout_struct_type(struct_type: &AnonymousStructType, name: &str) -> StructType {
    let mut offset = MemoryOffset(0);
    let mut max_alignment = MemoryAlignment::U8;
    let mut items = Vec::with_capacity(struct_type.field_name_sorted_fields.len());

    for (field_name, field_type) in &struct_type.field_name_sorted_fields {
        let field_layout = layout_type(&field_type.field_type);

        offset = align_to(offset, field_layout.max_alignment);

        items.push(OffsetMemoryItem {
            offset,
            size: field_layout.total_size,
            name: field_name.clone(),
            ty: field_layout.clone(),
        });

        offset = offset + field_layout.total_size;

        if field_layout.max_alignment > max_alignment {
            max_alignment = field_layout.max_alignment;
        }
    }

    let total_size = adjust_size_to_alignment(offset.as_size(), max_alignment);

    StructType {
        name: name.to_string(),
        fields: items,
        total_size,
        max_alignment,
    }
}

pub fn layout_struct(struct_type: &AnonymousStructType, name: &str) -> BasicType {
    let inner_struct = layout_struct_type(struct_type, name);
    BasicType {
        total_size: inner_struct.total_size,
        max_alignment: inner_struct.max_alignment,
        kind: BasicTypeKind::Struct(inner_struct),
    }
}

pub fn layout_optional_type(inner_type: &Type) -> BasicType {
    let tagged_union_type = layout_optional_type_items(inner_type);
    BasicType {
        total_size: tagged_union_type.total_size,
        max_alignment: tagged_union_type.max_alignment,
        kind: BasicTypeKind::Optional(tagged_union_type),
    }
}

pub fn layout_optional_type_items(inner_type: &Type) -> TaggedUnion {
    let gen_type = layout_type(inner_type);
    let payload_variant = VariantLayout {
        size: gen_type.total_size,
        alignment: gen_type.max_alignment,
    };
    let none_variant = VariantLayout {
        size: MemorySize(0),
        alignment: MemoryAlignment::U8,
    };
    let tagged = layout_tagged_union(&[none_variant, payload_variant]);

    let payload_tagged_variant = TaggedUnionVariant {
        name: "Some".to_string(),
        ty: layout_type(inner_type),
    };

    let none_tagged_variant = TaggedUnionVariant {
        name: "None".to_string(),
        ty: BasicType {
            kind: BasicTypeKind::Empty,
            total_size: MemorySize(0),
            max_alignment: MemoryAlignment::U8,
        },
    };

    TaggedUnion {
        name: "option".to_string(),
        tag_offset: tagged.tag_offset,
        tag_size: tagged.tag_size,
        payload_max_size: tagged.payload_size,
        payload_offset: tagged.payload_offset,
        variants: vec![none_tagged_variant, payload_tagged_variant],
        total_size: tagged.total_size,
        max_alignment: tagged.max_alignment,
    }
}

pub fn layout_tuple_items(types: &[Type]) -> TupleType {
    let mut offset = MemoryOffset(0);
    let mut max_alignment = MemoryAlignment::U8;
    let mut items = Vec::with_capacity(types.len());

    for (i, ty) in types.iter().enumerate() {
        let elem_layout = layout_type(ty);

        offset = align_to(offset, elem_layout.max_alignment);

        items.push(OffsetMemoryItem {
            offset,
            size: elem_layout.total_size,
            name: i.to_string(),
            ty: elem_layout.clone(),
        });

        offset = offset + elem_layout.total_size;

        if elem_layout.max_alignment > max_alignment {
            max_alignment = elem_layout.max_alignment;
        }
    }

    let total_size = adjust_size_to_alignment(offset.as_size(), max_alignment);

    TupleType {
        fields: items,
        total_size,
        max_alignment,
    }
}

pub fn layout_tuple(types: &[Type]) -> BasicType {
    let tuple_type = layout_tuple_items(types);

    BasicType {
        total_size: tuple_type.total_size,
        max_alignment: tuple_type.max_alignment,
        kind: BasicTypeKind::Tuple(tuple_type),
    }
}

/// # Errors
///
pub fn layout_variables(
    _node: &Node,
    variables: &Vec<VariableRef>,
    exp_return_type: &Type,
) -> FrameAndVariableInfo {
    const TEMPORARY_SIZE: MemorySize = MemorySize(16 * 1024);

    let mut allocator = ScopeAllocator::new(FrameMemoryRegion::new(
        FrameMemoryAddress(0),
        MemorySize(32 * 1024),
    ));

    let return_placed_type_pointer = layout_type(exp_return_type).create_mutable_pointer();

    let return_placed_type = allocator.allocate_type(return_placed_type_pointer); //reserve(return_placed_type_pointer, &mut allocator);

    let mut enter_comment = "variables:\n".to_string();

    let mut frame_memory_infos = Vec::new();
    if return_placed_type.size().0 != 0 {
        // Only add return if it is non-zero. Otherwise, the debug code is hard to follow.
        frame_memory_infos.push(FrameAddressInfo {
            kind: FrameAddressInfoKind::Return,
            frame_placed_type: return_placed_type.clone(),
        });
    }

    let mut variable_offsets = SeqMap::new();

    for var_ref in variables {
        let var_frame_placed_type = reserve(&var_ref.resolved_type, &mut allocator);
        trace!(?var_ref.assigned_name, ?var_frame_placed_type, "laying out");
        writeln!(
            &mut enter_comment,
            "  ${:04X}:{} {}",
            var_frame_placed_type.addr().0,
            var_frame_placed_type.size().0,
            var_ref.assigned_name
        )
        .unwrap();

        let kind = match var_ref.variable_type {
            VariableType::Local => FrameAddressInfoKind::Variable(VariableInfo {
                is_mutable: var_ref.is_mutable(),
                name: var_ref.assigned_name.clone(),
            }),
            VariableType::Parameter => FrameAddressInfoKind::Parameter(VariableInfo {
                is_mutable: var_ref.is_mutable(),
                name: var_ref.assigned_name.clone(),
            }),
        };

        frame_memory_infos.push(FrameAddressInfo {
            kind,
            frame_placed_type: var_frame_placed_type.clone(),
        });

        variable_offsets
            .insert(var_ref.unique_id_within_function, var_frame_placed_type)
            .unwrap();
    }

    let variable_space = allocator.addr().as_size();
    let allocate_for_temp = if variable_space.0 > (TEMPORARY_SIZE.0 / 2) {
        TEMPORARY_SIZE
    } else {
        let aligned = TEMPORARY_SIZE.0 as usize - align(variable_space.0 as usize, 8);
        MemorySize(aligned as u16)
    };

    let temp_allocator_region = FrameMemoryRegion {
        addr: allocator.allocate(allocate_for_temp, MemoryAlignment::U64),
        size: allocate_for_temp,
    };

    let frame_size = allocator.addr().as_size();

    FrameAndVariableInfo {
        frame_memory: FrameMemoryInfo {
            infos: frame_memory_infos,
            total_frame_size: frame_size,
            variable_frame_size: temp_allocator_region.addr.as_size(),
        },
        return_placement: return_placed_type,
        temp_allocator_region,
        variable_offsets,
    }
}
