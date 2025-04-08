/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::alloc::{FrameMemoryRegion, ScopeAllocator};
use seq_map::SeqMap;
use swamp_types::{AnonymousStructType, EnumVariantType, Type};
use swamp_vm_types::{
    FLOAT_SIZE, INT_SIZE, MAP_REFERENCE_SIZE, MAP_SIZE, MemoryAlignment, MemoryOffset, MemorySize,
    RANGE_SIZE, STR_SIZE, VEC_REFERENCE_SIZE,
};
use tracing::{error, info};

pub fn layout_struct(anon_struct: &AnonymousStructType) -> (MemorySize, MemoryAlignment) {
    let mut calculated_offset = MemoryOffset(0);
    let mut largest_alignment = MemoryAlignment::U8;
    for (_name, field) in &anon_struct.field_name_sorted_fields {
        let (field_size, field_alignment) = type_size_and_alignment(&field.field_type);
        if field_alignment.greater_than(largest_alignment) {
            largest_alignment = field_alignment;
        }
        calculated_offset.space(field_size, field_alignment);
    }

    let total_offset = calculated_offset.space(MemorySize(0), largest_alignment);

    (total_offset.as_size(), largest_alignment)
}

#[must_use]
pub fn layout_tuple(types: &Vec<Type>) -> (MemorySize, MemoryAlignment) {
    let mut calculated_offset = MemoryOffset(0);
    let mut largest_alignment = MemoryAlignment::U8;
    for ty in types {
        let (field_size, field_alignment) = type_size_and_alignment(&ty);
        if field_alignment.greater_than(largest_alignment) {
            largest_alignment = field_alignment;
        }
        calculated_offset.space(field_size, field_alignment);
    }
    let total_offset = calculated_offset.space(MemorySize(0), largest_alignment);
    (total_offset.as_size(), largest_alignment)
}

#[must_use]
pub fn layout_tuple_elements(
    types: &Vec<Type>,
) -> (MemorySize, MemoryAlignment, Vec<(MemoryOffset, MemorySize)>) {
    let mut calculated_offset = MemoryOffset(0);
    let mut largest_alignment = MemoryAlignment::U8;

    let mut elements = Vec::new();
    for ty in types {
        let (field_size, field_alignment) = type_size_and_alignment(&ty);
        if field_alignment.greater_than(largest_alignment) {
            largest_alignment = field_alignment;
        }
        elements.push((calculated_offset, field_size));
        calculated_offset.space(field_size, field_alignment);
    }
    let total_offset = calculated_offset.space(MemorySize(0), largest_alignment);
    (total_offset.as_size(), largest_alignment, elements)
}

pub fn layout_union(variants: &SeqMap<String, EnumVariantType>) -> (MemorySize, MemoryAlignment) {
    let mut max_variant_alignment = MemoryAlignment::U8;
    let mut max_variant_size = MemorySize(0);
    let mut calculated_offset = MemoryOffset(0);
    for (_name, variant) in variants {
        let (variant_size, variant_alignment) = match variant {
            EnumVariantType::Struct(anon_struct) => layout_struct(&anon_struct.anon_struct),
            EnumVariantType::Tuple(types) => layout_tuple(&types.fields_in_order),
            EnumVariantType::Nothing(_) => (MemorySize(0), MemoryAlignment::U8),
        };

        if variant_alignment.greater_than(max_variant_alignment) {
            max_variant_alignment = variant_alignment;
        }

        if variant_size.0 > max_variant_size.0 {
            max_variant_size = variant_size;
        }
    }

    (max_variant_size, max_variant_alignment)
}

pub fn is_vec(ty: &Type) -> Option<(MemorySize, MemoryAlignment)> {
    match ty {
        Type::NamedStruct(named_struct) => {
            if named_struct.module_path == vec!["core-0.0.0".to_string()]
                && named_struct.assigned_name.starts_with("Vec<")
            {
                Some((MemorySize(VEC_REFERENCE_SIZE), MemoryAlignment::U16))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn is_map(ty: &Type) -> Option<(MemorySize, MemoryAlignment)> {
    match ty {
        Type::NamedStruct(named_struct) => {
            if named_struct.module_path == vec!["core-0.0.0".to_string()]
                && named_struct.assigned_name.starts_with("Map<")
            {
                Some((MemorySize(MAP_SIZE), MemoryAlignment::U16))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn is_range(ty: &Type) -> Option<(MemorySize, MemoryAlignment)> {
    match ty {
        Type::NamedStruct(named_struct) => {
            if named_struct.module_path == vec!["core-0.0.0".to_string()]
                && named_struct.assigned_name.starts_with("Range")
            {
                Some((MemorySize(RANGE_SIZE), MemoryAlignment::U16))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn type_size_and_alignment(ty: &Type) -> (MemorySize, MemoryAlignment) {
    match ty {
        Type::Int => (MemorySize(INT_SIZE), MemoryAlignment::U32),
        Type::Float => (MemorySize(FLOAT_SIZE), MemoryAlignment::U32),
        Type::String => (MemorySize(STR_SIZE), MemoryAlignment::U16),
        Type::Bool => (MemorySize(1), MemoryAlignment::U8),
        Type::Unit => (MemorySize(0), MemoryAlignment::U8),
        Type::Never => (MemorySize(0), MemoryAlignment::U8),
        Type::Tuple(types) => layout_tuple(types),
        Type::NamedStruct(named_struct) => {
            if named_struct.module_path == vec!["core-0.0.0".to_string()]
                && named_struct.assigned_name.starts_with("Vec<")
            {
                (MemorySize(VEC_REFERENCE_SIZE), MemoryAlignment::U16)
            } else if named_struct.module_path == vec!["core-0.0.0".to_string()]
                && named_struct.assigned_name.starts_with("Map<")
            {
                (MemorySize(MAP_REFERENCE_SIZE), MemoryAlignment::U16)
            } else {
                type_size_and_alignment(&Type::AnonymousStruct(
                    named_struct.anon_struct_type.clone(),
                ))
            }
        }
        Type::Slice(value_type) => type_size_and_alignment(value_type),
        Type::SlicePair(key_type, value_type) => {
            layout_tuple(&vec![*key_type.clone(), *value_type.clone()])
        }
        Type::AnonymousStruct(anon_struct) => layout_struct(anon_struct),
        Type::Enum(enum_type) => {
            let (offset, alignment) = layout_union(&enum_type.variants);

            let alignment_octets: usize = alignment.into();

            (MemorySize(offset.0 + alignment_octets as u16), alignment)
        }
        Type::Function(_) => (MemorySize(2), MemoryAlignment::U16),
        Type::Optional(inner_type) => {
            let (offset, alignment) = type_size_and_alignment(inner_type);

            let alignment_octets: usize = alignment.into();

            (MemorySize(offset.0 + alignment_octets as u16), alignment)
        }
        Type::Generic(a, b) => {
            error!(?a, ?b, "generic can not be generated");
            panic!("generic is not supported")
        }
        Type::MutableReference(referenced_type) => type_size_and_alignment(referenced_type),
        Type::Blueprint(_) => panic!("not supported"),
        Type::Variable(_) => panic!("not supported"),
        Type::External(_) => todo!(),
    }
}

pub fn reserve_space_for_type(ty: &Type, allocator: &mut ScopeAllocator) -> FrameMemoryRegion {
    let (size, alignment) = type_size_and_alignment(ty);

    allocator.reserve(size, alignment)
}
